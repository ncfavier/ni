{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE BangPatterns #-}
module Ni where

import System.Exit
import Control.Monad
import Control.Monad.Fail as F
import Control.Monad.Trans.State
import Control.Monad.Trans.Class
import Data.Foldable
import qualified Data.Map as M

import AST

data Context = Context { stack :: [Value], environment :: M.Map String (Ni ()) }

onStack f (Context st env) = Context (f st) env
onEnvironment f (Context st env) = Context st (f env)

type Ni = StateT Context IO

execNi = execStateT

instance Semigroup (Ni ()) where
    (<>) = (>>)

instance Monoid (Ni ()) where
    mempty = pure ()

emptyStack = die "Empty stack."
unboundSymbol s = die $ "Unbound symbol '" ++ s ++ "'."

push !v = modify $ onStack (v:)

withUncons f = do
    st <- gets stack
    case st of
        v:vs -> f v vs
        [] -> lift emptyStack

pop = withUncons $ \v vs -> do
    modify $ onStack $ const vs
    return v

peek = withUncons $ \v vs -> return v

peekStack = gets (List . stack)

bind s !n = modify $ onEnvironment $ M.insert s n
unbind s = modify $ onEnvironment $ M.delete s
bindValue s = bind s . push
define s = bind s . eval

valueToNi (Symbol ('$':s)) = if null s
    then void pop
    else pop >>= bindValue s
valueToNi (Symbol ('\\':s@(_:_))) = push (Symbol s)
valueToNi (Symbol s) = do
    env <- gets environment
    case M.lookup s env of
        Just n -> n
        Nothing -> lift $ unboundSymbol s
valueToNi v = push v

eval = mconcat . map valueToNi

printValue v = lift $ case v of
    String s -> putStr s
    Char c -> putChar c
    _ -> print v

plus (Integer a) (Integer b) = Just $ Integer (a + b)
plus (Double a) (Double b) = Just $ Double (a + b)
plus (String a) (String b) = Just $ String (a ++ b)
plus (List a) (List b) = Just $ List (a ++ b)
plus _ _ = Nothing

minus (Integer a) (Integer b) = Just $ Integer (a - b)
minus (Double a) (Double b) = Just $ Double (a - b)
minus _ _ = Nothing

multiply (Integer a) (Integer b) = Just $ Integer (a * b)
multiply (Double a) (Double b) = Just $ Double (a * b)
multiply _ _ = Nothing

divide (Integer a) (Integer b) = Just $ Integer (a `div` b)
divide (Double a) (Double b) = Just $ Double (a / b)
divide _ _ = Nothing

power (Integer a) (Integer b) = Just $ Integer (a ^ b)
power (Double a) (Double b) = Just $ Double (a ** b)
power _ _ = Nothing

isNull (List l) = Just $ Bool $ null l
isNull (String s) = Just $ Bool $ null s
isNull _ = Nothing

initialStack = []

initialEnvironment = M.fromList [
    -- Meta
    ("eval", do List l <- pop; eval l),
    -- Environment
    ("define", do Symbol s <- pop; List l <- pop; define s l),
    ("unbind", do Symbol s <- pop; unbind s),
    -- IO
    ("print", pop >>= printValue),
    ("printStack", peekStack >>= printValue),
    ("getChar", do c <- lift getChar; push (Char c)),
    ("getLine", do s <- lift getLine; push (String s)),
    ("exit", do lift exitSuccess),
    -- Logic
    ("not", do Bool b <- pop; push $ Bool $ not b),
    ("and", do Bool a <- pop; Bool b <- pop; push $ Bool $ a && b),
    ("or", do Bool a <- pop; Bool b <- pop; push $ Bool $ a || b),
    ("ifelse", do List no <- pop; List yes <- pop; Bool cond <- pop; eval (if cond then yes else no)),
    -- Math
    ("+", do a <- pop; b <- pop; let Just v = plus a b in push v),
    ("-", do a <- pop; b <- pop; let Just v = minus a b in push v),
    ("*", do a <- pop; b <- pop; let Just v = multiply a b in push v),
    ("/", do a <- pop; b <- pop; let Just v = divide a b in push v),
    ("^", do a <- pop; b <- pop; let Just v = power a b in push v),
    -- Lists
    ("null", do v <- pop; let Just r = isNull v in push r),
    ("cons", do v <- pop; List vs <- pop; push $ List (v:vs)),
    ("uncons", do List (v:vs) <- pop; push (List vs); push v)]

initialContext = Context initialStack initialEnvironment
