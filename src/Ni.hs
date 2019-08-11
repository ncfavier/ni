{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE BangPatterns #-}
module Ni where

import System.Exit
import Control.Monad
import Control.Monad.Fail as F
import Data.Foldable
import qualified Data.Map as M

import AST

data Context = Context { stack :: [Value], environment :: M.Map String (Ni ()) }

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

emptyStack = die "Empty stack."
unboundSymbol s = die $ "Unbound symbol '" ++ s ++ "'."

push !v = Ni $ \ctx ->
    return (ctx { stack = v:stack ctx }, ())

pop = Ni $ \ctx ->
    case stack ctx of
        v:vs -> return (ctx { stack = vs }, v)
        [] -> emptyStack

peek = Ni $ \ctx ->
    case stack ctx of
        v:_ -> return (ctx, v)
        [] -> emptyStack

peekStack = Ni $ \ctx ->
    return (ctx, List (stack ctx))

bind s !n = Ni $ \ctx ->
    return (ctx { environment = M.insert s n (environment ctx) }, ())

bindValue s = bind s . push

define s = bind s . eval

unbind s = Ni $ \ctx ->
    return (ctx { environment = M.delete s (environment ctx) }, ())

valueToNi (Symbol ('$':s)) = if null s
    then void pop
    else pop >>= bindValue s
valueToNi (Symbol ('\\':s@(_:_))) = push (Symbol s)
valueToNi (Symbol s) = Ni $ \ctx ->
    case M.lookup s (environment ctx) of
        Just n -> run n ctx
        Nothing -> unboundSymbol s
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
