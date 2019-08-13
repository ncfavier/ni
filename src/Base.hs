{-# LANGUAGE FlexibleInstances #-}
module Base where

import Data.Char
import qualified Data.Map as M
import Control.Applicative
import Control.Monad.Trans.State
import Text.Read as T
import Text.Read.Lex
import Text.ParserCombinators.ReadP hiding (choice, (<++))

data Value = Integer Integer
           | Double Double
           | Bool Bool
           | Char Char
           | String String
           | Symbol String
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
    show (Integer i) = show i
    show (Double d) = show d
    show (Bool b) = if b then "#true" else "#false"
    show (Base.Char c) = show c
    show (Base.String s) = show s
    show (Base.Symbol s) = s
    show (VEnvironment m) = "<environment " ++ name m ++ ">"
    show (List l) = "[" ++ showList l "" ++ "]"
    showList l = showString $ unwords $ map show l

-- TODO: parse \ and $
-- TODO: comments
instance Read Value where
    readPrec = (do
            Punc "[" <- lexP
            l <- readListPrec
            Punc "]" <- lexP
            return (List l)) <++
        (do Number n <- lexP
            case numberToInteger n of
                Just i -> return (Integer i)
                Nothing -> return $ Double $ fromRational $ numberToRational n) <++
        (do T.Char c <- lexP
            return (Base.Char c)) <++
        (do T.String s <- lexP
            return (Base.String s)) <++
        (do s <- lift $ do
                skipSpaces
                munch1 $ \c -> not (isSpace c || c `elem` "[]")
            case s of
                "#true" -> return (Bool True)
                "#false" -> return (Bool False)
                _ -> return (Base.Symbol s))
    readListPrec = liftA2 (:) readPrec readListPrec <++ pure []

instance Eq Environment where
    a == b = name a == name b
