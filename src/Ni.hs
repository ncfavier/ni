{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleInstances #-}
module Ni where

import System.Exit
import Control.Monad
import Control.Monad.Fail as F
import Data.Foldable

import AST

data Context = Context { stack :: [Value], environment :: [(String, Ni ())] }

newtype Ni a = Ni { run :: Context -> IO (Context, a) }

instance Functor Ni where
    fmap f n = Ni $ (fmap . fmap) f . run n

instance Applicative Ni where
    pure x = Ni $ \ctx -> pure (ctx, x)
    (<*>) = ap

instance Monad Ni where
    n >>= f = Ni $ \ctx -> do
        (ctx', x) <- run n ctx
        run (f x) ctx'

instance MonadFail Ni where
    fail = lift . die

instance Semigroup (Ni ()) where
    (<>) = (>>)

instance Monoid (Ni ()) where
    mempty = pure ()

lift io = Ni $ \ctx -> do
    x <- io
    return (ctx, x)

push v = Ni $ \ctx ->
    return (ctx { stack = v:stack ctx }, ())

pop = Ni $ \ctx ->
    case stack ctx of
        v:vs -> return (ctx { stack = vs }, v)
        [] -> die "Empty stack."

peek = Ni $ \ctx ->
    case stack ctx of
        v:_ -> return (ctx, v)
        [] -> die "Empty stack."

peekStack = Ni $ \ctx ->
    return (ctx, List (stack ctx))

getInteger v = case v of
    Integer i -> return i
    _ -> F.fail "Expected integer."
getDouble v = case v of
    Double d -> return d
    _ -> F.fail "Expected double."
getBool v = case v of
    Bool b -> return b
    _ -> F.fail "Expected boolean."
getChar v = case v of
    Char c -> return c
    _ -> F.fail "Expected character."
getString v = case v of
    String s -> return s
    _ -> F.fail "Expected string."
getSymbol v = case v of
    Symbol s -> return s
    _ -> F.fail "Expected symbol."
getList v = case v of
    List l -> return l
    _ -> F.fail "Expected list."

bind s n = Ni $ \ctx ->
    return (ctx { environment = (s, n):environment ctx }, ())

bindValue s = bind s . push

define s = bind s . eval

printValue v = lift $ do
    case v of
        String s -> putStrLn s
        Char c -> putChar c
        _ -> print v

valueToNi (Symbol ('$':s)) = pop >>= bindValue s
valueToNi (Symbol ('\'':s)) = push (Symbol s)
valueToNi (Symbol s) = Ni $ \ctx ->
    case lookup s (environment ctx) of
        Just n -> run n ctx
        Nothing -> die $ "Unbound symbol '" ++ s ++ "'."
valueToNi v = push v

eval = foldMap valueToNi
