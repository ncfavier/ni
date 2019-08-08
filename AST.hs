module AST where

import Control.Applicative
import Text.Read as T
import Text.Read.Lex

type Program = [Value]

data Value = Integer Integer
           | Double Double
           | Char Char
           | String String
           | Symbol String
           | Quoted Program
           deriving (Eq, Show)

instance Read Value where
    readPrec = choice [readNumber, readChar, readString, readSymbol, readQuoted] where
        readNumber = do
            Number n <- lexP
            case numberToInteger n of
                Just i -> return (Integer i)
                Nothing -> return $ Double $ fromRational $ numberToRational n
        readChar = do
            T.Char c <- lexP
            return (AST.Char c)
        readString = do
            T.String s <- lexP
            return (AST.String s)
        readSymbol = do
            T.Ident s <- lexP
            return (AST.Symbol s)
        readQuoted = do
            Punc "[" <- lexP
            p <- readListPrec
            Punc "]" <- lexP
            return (Quoted p)
    readListPrec = liftA2 (:) readPrec readListPrec +++ pure []
