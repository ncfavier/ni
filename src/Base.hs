{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
module Base where

import Data.Char
import Data.Function
import Data.Maybe
import qualified Data.Map as M
import Control.Applicative hiding (optional, many)
import Control.Monad
import Control.Monad.Trans.State hiding (get)
import Text.Read as T hiding (pfail, get, choice, (<++))
import Text.Read.Lex as L
import Text.ParserCombinators.ReadP hiding (optional)

data Value = Integer !Integer
           | Double !Double
           | Bool !Bool
           | Char !Char
           | String !String
           | Symbol !String
           | VEnvironment Environment
           | List [Value]
           deriving Eq

data Environment = Environment { name :: String, bindings :: M.Map String (Ni ()) }

data Context = Context { stack :: [Value], environments :: [Environment] }

type Ni = StateT Context IO

instance Semigroup (Ni ()) where
    (<>) = (>>)

instance Monoid (Ni ()) where
    mempty = pure ()

instance Show Value where
    showsPrec _ (Integer n) = shows n
    showsPrec _ (Double n) = shows n
    showsPrec _ (Bool b) = showString (if b then ":true" else ":false")
    showsPrec _ (Base.Char c) = shows c
    showsPrec _ (Base.String s) = shows s
    showsPrec _ (Base.Symbol s) = showString s
    showsPrec _ (VEnvironment m) = showString $ "<environment " ++ name m ++ ">"
    showsPrec _ (List l) = showChar '[' . showList l . showChar ']'
    showList [] = id
    showList [v] = shows v
    showList (v:vs) = shows v . showChar ' ' . showList vs

optional f = (Just <$> f) <++ pure Nothing

skipSpacesAndComments = do
    skipSpaces
    optional $ do
        char '#'
        munch (/= '\n')
        skipSpacesAndComments
    return ()

readNumber = do
    sign <- option '+' $ choice $ char <$> "-+"
    let sign' = if sign == '+' then 1 else -1
    integer <- optional (munch1 isNumber)
    fractional <- optional $ char '.' >> optional (munch1 isNumber)
    case (integer, fractional) of
        (Just i, Nothing) -> do
            return $ Integer $ sign' * read i
        (Just i, Just v) -> do
            return $ Double $ sign' * read (i ++ "." ++ fromMaybe "0" v)
        (Nothing, Just (Just d)) -> do
            return $ Double $ sign' * read ("0." ++ d)
        _ -> pfail

readValue = do
    skipSpacesAndComments
    foldr1 (<++) [readNumber, readBool, readChar, readString, readSymbol, readList]
    where
        readBool = readTrue <++ readFalse
        readTrue = do
            string ":true"
            return $ Bool True
        readFalse = do
            string ":false"
            return $ Bool False
        readChar = do
            T.Char c <- L.lex
            return $ Base.Char c
        readString = do
            T.String s <- L.lex
            return $ Base.String s
        readSymbol = do
            s <- munch1 $ \c -> not (isSpace c) && c `notElem` "[]"
            return $ Base.Symbol s
        readList = do
            vs <- between (char '[') (char ']') readValues
            return $ List vs

readValues = liftA2 (:) readValue readValues <++ (skipSpacesAndComments >> pure [])

-- TODO: parse \ and $
instance Read Value where
    readPrec = lift readValue
    readListPrec = lift readValues

instance Eq Environment where
    (==) = (==) `on` name
