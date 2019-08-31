{-# LANGUAGE NoMonomorphismRestriction #-}
module Ni where

import System.Exit
import Data.List
import Data.Foldable
import Data.Functor
import qualified Data.Map as M
import Control.Applicative
import Control.Monad
import Control.Monad.Trans.State
import Control.Monad.Trans.Class

import Base

valueToNi (Symbol ('$':s)) = if null s
    then void pop
    else pop >>= bindValue s
valueToNi (Symbol ('\\':s@(_:_))) = push (Symbol s)
valueToNi (Symbol s) = do
    envs <- gets environments
    case asum $ map (M.lookup s . bindings) envs of
        Just n -> n <|> fail ("wrong arguments for " ++ s)
        Nothing -> fail $ s ++ ": unbound symbol"
valueToNi v = push v

eval v@(Symbol _) = valueToNi v
eval (List l) = mconcat $ map valueToNi l
eval v = fail $ "cannot eval " ++ show v

modifyStack f = modify $ \ctx -> ctx { stack = f (stack ctx) }
modifyEnvironments f = modify $ \ctx -> ctx { environments = f (environments ctx) }
modifyCurrentEnvironment f = modifyEnvironments $ \(m:ms) -> f m:ms
modifyCurrentBindings f = modifyCurrentEnvironment $ \(Environment n b) -> Environment n (f b)

push v = modifyStack (v:)

unconsStack f = do
    st <- gets stack
    case st of
        v:vs -> f st
        [] -> fail "empty stack"

pop  = unconsStack $ \(v:vs) -> modifyStack (const vs) >> return v
peek = unconsStack $ \(v:vs) -> return v

peekStack = gets (List . stack)

pushEnv e = modifyEnvironments (e:)

popEnv = do
    envs <- gets environments
    case envs of
        [] -> fail "empty environment stack"
        [_] -> fail "cannot unuse default environment"
        e:es -> do
            modifyEnvironments $ const es
            return e

bind s n = modifyCurrentBindings $ M.insert s n
unbind s = modifyCurrentBindings $ M.delete s
bindValue s = bind s . push
define s = bind s . eval

makeEnvironment n l = Environment n (M.fromListWith (flip (<|>)) l)

-- TODO: conversions, ordering, more math
baseEnvironment = makeEnvironment "base" $
    [("eval", pop >>= eval)
    ,("define", do l <- pop; Symbol s <- pop; define s l)
    ,("unbind", do Symbol s <- pop; unbind s)
    ,("new", do String s <- pop; push $ VEnvironment $ Environment s M.empty)
    ,("use", do VEnvironment e <- pop; pushEnv e)
    ,("unuse", do e <- popEnv; push (VEnvironment e))
    ,("=", do a <- pop; b <- pop; push $ Bool $ a == b)
    ,("not", do Bool b <- pop; push $ Bool $ not b)
    ,("and", do Bool a <- pop; Bool b <- pop; push $ Bool $ a && b)
    ,("or", do Bool a <- pop; Bool b <- pop; push $ Bool $ a || b)
    ,("ifelse", do no <- pop; yes <- pop; Bool cond <- pop; eval (if cond then yes else no))
    ] ++ (
    [("+", (+)), ("-", (-)), ("*", (*)), ("/", div), ("^", (^))] <&> \(s, f) ->
        (s, do Integer a <- pop; Integer b <- pop; push $ Integer $ f a b)
    ) ++ (
    [("+", (+)), ("-", (-)), ("*", (*)), ("/", (/)), ("^", (**))] <&> \(s, f) ->
        (s, do Double a <- pop; Double b <- pop; push $ Double $ f a b)
    ) ++
    [("+", do List a <- pop; List b <- pop; push $ List $ a ++ b)
    ,("+", do String a <- pop; String b <- pop; push $ String $ a ++ b)
    ,("null?", do List l <- pop; push $ Bool $ null l)
    ,("null?", do String s <- pop; push $ Bool $ null s)
    ,("cons", do v <- pop; List vs <- pop; push $ List (v:vs))
    ,("cons", do Char c <- pop; String cs <- pop; push $ String (c:cs))
    ,("uncons", do List (v:vs) <- pop; push (List vs); push v)
    ,("uncons", do String (c:cs) <- pop; push (String cs); push (Char c))
    ]

ioEnvironment = makeEnvironment "io"
    [("print", do String s <- pop; lift $ putStr s)
    ,("print", do Char c <- pop; lift $ putChar c)
    ,("print", pop >>= lift . print)
    ,("printStack", peekStack >>= lift . print)
    ,("getChar", do c <- lift getChar; push (Char c))
    ,("getLine", do s <- lift getLine; push (String s))
    ,("exit", do lift exitSuccess)
    ]
