{-# LANGUAGE BlockArguments #-}
module Main where

import System.Environment
import System.Exit
import System.IO
import System.IO.Error
import System.Timeout
import System.Posix.Signals
import Network.Connection
import Data.Char
import Data.Maybe
import Data.List
import Data.Bifunctor
import Data.Default.Class
import qualified Data.ByteString.Char8 as BS
import Data.ByteString.Base64
import Text.Read hiding (Char, String)
import Control.Monad
import Control.Monad.Trans.State

import Base
import Ni

onEOF handler f = catchIOError f (\e -> if isEOFError e then handler else ioError e)

sendRaw line conn = do
    putStrLn $ "<-- " ++ line
    if length line > 510 then
        putStrLn "/!\\ line too long, will not send"
    else if '\r' `elem` line || '\n' `elem` line then
        putStrLn "/!\\ multiple lines, will not send"
    else
        connectionPut conn $ BS.pack $ line ++ "\r\n"

sendIRC command params conn = sendRaw (makeLine (map toUpper command) params) conn where
    makeLine command params =
        case params of
            [] -> command
            _ -> intercalate " " $ command:(init params ++ [':':last params])

privmsg target message = sendIRC "PRIVMSG" [target, message]
notice  target message = sendIRC "NOTICE"  [target, message]

nickServ cmd = privmsg "NickServ" (unwords cmd)

splitWordWith c l = second (dropWhile (== c)) $ break (== c) $ dropWhile (== c) l
splitWord = splitWordWith ' '

splitParams "" = []
splitParams (':':p) = [p]
splitParams ps = p:splitParams rest where
    (p, rest) = splitWord ps

parseIRC l = (prefix, map toUpper command, params) where
    (prefix, rest) = case l of
        ':':r -> splitWord r
        _     -> ("", l)
    (command, params) = second splitParams $ splitWord rest

initialContext = Context [] [baseEnvironment]

main = do
    server   <- getEnvString "NIRC_SERVER"   "irc.libera.chat"
    port     <- getEnvInt    "NIRC_PORT"     6697
    useTLS   <- getEnvBool   "NIRC_USE_TLS"  True
    nickname <- getEnvString "NIRC_NICKNAME" "nirc"
    realname <- getEnvString "NIRC_REALNAME" nickname
    password <- getEnvString "NIRC_PASSWORD" ""
    modes    <- getEnvString "NIRC_MODES"    ""
    channels <- getEnvString "NIRC_CHANNELS" ""
    prefix   <- getEnvString "NIRC_PREFIX"   ","
    stdlib   <- eval . List <$> (readFile "src/stdlib.ni" >>= readIO)
    ctx      <- initConnectionContext
    conn     <- connectTo ctx $ ConnectionParams server port (if useTLS then Just def else Nothing) Nothing
    hSetBuffering stdout NoBuffering
    installHandler keyboardSignal (Catch $ sendIRC "QUIT" [] conn) Nothing
    sendIRC "CAP" ["REQ", "sasl"] conn
    sendIRC "NICK" [nickname] conn
    sendIRC "USER" [nickname, "0", "*", realname] conn
    let finish = do
            connectionClose conn
            exitSuccess
    forever do
        line <- onEOF finish $ BS.unpack . fst . BS.spanEnd (== '\r') <$> connectionGetLine 4096 conn
        putStrLn $ "--> " ++ line
        let (sender, command, params) = parseIRC line
            ((nick, user), host) = first (splitWordWith '!') $ splitWordWith '@' sender
            handleCTCP ctcp = do
                let (command, params) = first (map toUpper) $ splitWord ctcp
                    reply r = notice nick ("\1" ++ command ++ " " ++ r ++ "\1") conn
                case command of
                    "PING"    -> reply params
                    "VERSION" -> reply "nirc (https://git.monade.li/ni)"
                    "SOURCE"  -> reply "https://git.monade.li/ni"
                    _ -> return ()
            handleCommand target command = unless (nick == nickname) do
                let reply r = privmsg (if target == nickname then nick else target) r conn
                    (cmd, rest) = splitWord command
                    args = words rest
                case map toLower cmd of
                    "help" -> reply case args of
                        [] -> "I am nirc, the Ni bot (https://git.monade.li/ni). Available commands: " ++ intercalate ", " ["help", "ni", prefix, prefix ++ "*"]
                        "help":_ -> "help [command] -- prints a help message"
                        "ni":_ -> "ni -- Ni!"
                        c:_ | c == prefix -> prefix ++ " code -- run Ni code and print the resulting top value"
                            | c == prefix ++ "*" -> prefix ++ "*" ++ " code -- run Ni code and print the resulting stack"
                            | otherwise -> "Unknown command: " ++ c
                    "ni" -> reply "Ni!"
                    c | c `elem` [prefix, prefix ++ "*"] -> do
                        case List <$> readMaybe rest of
                            Just p -> do
                                result <- tryIOError $ timeout 1000000 $ execStateT (stdlib <> eval p) initialContext
                                reply case result of
                                    Left e -> "Error: " ++ ioeGetErrorString e
                                    Right Nothing -> "Timed out"
                                    Right (Just (Context [] _)) -> "(empty)"
                                    Right (Just (Context st _)) -> dropWhile (== '\1')
                                        if c == prefix ++ "*" then show st
                                        else case st of
                                            Char c:_ -> [c]
                                            String s:_ -> s
                                            v:_ -> show v
                            Nothing -> reply "Parse error"
                    _ -> reply $ "Unknown command: " ++ cmd
        case (command, params) of
            ("CAP", _:"ACK":_) -> do
                sendRaw "AUTHENTICATE PLAIN" conn
            ("AUTHENTICATE", _) -> do
                sendRaw ("AUTHENTICATE " ++ (BS.unpack . encode . BS.pack $ intercalate "\0" [nickname, nickname, password])) conn
            ("CAP", _:"NAK":_) -> do
                sendIRC "CAP" ["END"] conn
                nickServ ["IDENTIFY", password] conn
            ("903", _) -> do
                sendIRC "CAP" ["END"] conn
            ("904", _) -> finish
            ("906", _) -> finish
            ("432", _) -> finish
            ("433", _:nick:_) -> sendIRC "NICK" [nick ++ "-"] conn
            ("437", _:nick:_) -> sendIRC "NICK" [nick ++ "-"] conn
            ("PING", p:_) -> sendIRC "PONG" [p] conn
            ("001", _) -> do
                nickServ ["RELEASE", nickname] conn
                sendIRC "NICK" [nickname] conn
                unless (null modes) $ sendIRC "MODE" [nickname, modes] conn
                unless (null channels) $ sendIRC "JOIN" [channels] conn
            ("PRIVMSG", [target, '\1':ctcp]) ->
                handleCTCP (dropWhileEnd (== '\1') ctcp)
            ("PRIVMSG", [target, message])
                | target == nickname -> handleCommand target message
                | Just command <- msum $ map (`stripPrefix` message) [prefix, nickname ++ ":", nickname ++ ","] ->
                    handleCommand target command
            _ -> return ()
    where
    getEnvString n d = fromMaybe d <$> lookupEnv n
    getEnvInt    n d = maybe d read <$> lookupEnv n
    getEnvBool   n d = maybe d ((/= 0) . read) <$> lookupEnv n
