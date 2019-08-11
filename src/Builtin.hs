module Builtin where

import Control.Monad
import Control.Monad.Fail as F

import AST
import Ni

printValue v = lift $ case v of
    String s -> putStrLn s
    Char c -> putChar c
    _ -> print v

initialStack = []

initialEnvironment = [
    ("print", pop >>= printValue),
    ("printStack", peekStack >>= printValue),
    ("define", join $ liftM2 define (pop >>= getSymbol) (pop >>= getList)),
    ("eval", pop >>= getList >>= eval),
    ("+", do
        a <- pop >>= getInteger
        b <- pop >>= getInteger
        push $ Integer $ a + b),
    ("null?", peek >>= getList >>= push . Bool . null),
    ("if", do
        no <- pop >>= getList
        yes <- pop >>= getList
        cond <- pop >>= getBool
        eval (if cond then yes else no)),
    ("pop", void pop),
    ("uncons", do
        l <- pop >>= getList
        case l of
            v:vs -> do
                push (List vs)
                push v
            [] -> F.fail "Empty list.")]

initialContext = Context initialStack initialEnvironment
