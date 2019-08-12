{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleInstances #-}
module Ni where

import System.Exit
import Data.List
import Data.Functor
import qualified Data.Map as M
import Control.Applicative
import Control.Monad
import Control.Monad.Trans.State
import Control.Monad.Trans.Class

import AST

data Context = Context { stack :: [Value], environment :: M.Map String (Ni ()) }

onStack f (Context st env) = Context (f st) env
onEnvironment f (Context st env) = Context st (f env)

type Ni = StateT Context IO

instance Semigroup (Ni ()) where
    (<>) = (>>)

instance Monoid (Ni ()) where
    mempty = pure ()

execNi = execStateT

failWithStackDump s = do
    List l <- peekStack
    lift $ die $ intercalate "\n" $ [s, "Stack: " ++ show l]

emptyStack = failWithStackDump "Empty stack"
unboundSymbol s = failWithStackDump $ "Unbound symbol " ++ s
notEvaluable v = failWithStackDump $ "Cannot eval " ++ show v
typeError s = failWithStackDump $ s ++ ": type error"

push v = modify $ onStack (v:)

withUncons f = do
    st <- gets stack
    case st of
        v:vs -> f v vs
        [] -> emptyStack

pop = withUncons $ \v vs -> do
    modify $ onStack $ const vs
    return v

peek = withUncons $ \v vs -> return v

peekStack = gets (List . stack)

bind s n = modify $ onEnvironment $ M.insert s n
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
        Just n -> n <|> typeError s
        Nothing -> unboundSymbol s
valueToNi v = push v

eval v@(Symbol _) = valueToNi v
eval (List l) = mconcat $ map valueToNi l
eval v = notEvaluable v

initialEnvironment = M.fromListWith (<|>) $ [
    -- Meta
    ("eval", pop >>= eval),
    -- Environment
    ("define", do l <- pop; Symbol s <- pop; define s l),
    ("unbind", do Symbol s <- pop; unbind s),
    -- IO
    ("print", do String s <- pop; lift $ putStr s),
    ("print", do Char c <- pop; lift $ putChar c),
    ("print", pop >>= lift . print),
    ("printStack", peekStack >>= lift . print),
    ("getChar", do c <- lift getChar; push (Char c)),
    ("getLine", do s <- lift getLine; push (String s)),
    ("exit", do lift exitSuccess),
    -- Logic
    ("not", do Bool b <- pop; push $ Bool $ not b),
    ("and", do Bool a <- pop; Bool b <- pop; push $ Bool $ a && b),
    ("or", do Bool a <- pop; Bool b <- pop; push $ Bool $ a || b),
    ("ifelse", do no <- pop; yes <- pop; Bool cond <- pop; eval (if cond then yes else no))] ++
    -- Math
    ([("+", (+)), ("-", (-)), ("*", (*)), ("/", div), ("^", (^))] <&> \(s, f) ->
        (s, do Integer a <- pop; Integer b <- pop; push $ Integer $ f a b)) ++
    ([("+", (+)), ("-", (-)), ("*", (*)), ("/", (/)), ("^", (**))] <&> \(s, f) ->
        (s, do Double a <- pop; Double b <- pop; push $ Double $ f a b)) ++ [
    -- Lists and strings
    ("+", do List a <- pop; List b <- pop; push $ List $ a ++ b),
    ("+", do String a <- pop; String b <- pop; push $ String $ a ++ b),
    ("null", do List l <- pop; push $ Bool $ null l),
    ("null", do String s <- pop; push $ Bool $ null s),
    ("cons", do v <- pop; List vs <- pop; push $ List (v:vs)),
    ("uncons", do List (v:vs) <- pop; push (List vs); push v)]
    -- TODO: conversions

initialContext = Context [] initialEnvironment
