module AST where

import Control.Applicative
import Data.Char
import Text.Read as T
import Text.Read.Lex
import Text.ParserCombinators.ReadP hiding (choice, (<++))

data Value = Integer Integer
           | Double Double
           | Bool Bool
           | Char Char
           | String String
           | Symbol String
           | List [Value]
           deriving Eq

instance Show Value where
    show (Integer i) = show i
    show (Double d) = show d
    show (Bool b) = if b then "#true" else "#false"
    show (AST.Char c) = show c
    show (AST.String s) = show s
    show (AST.Symbol s) = s
    show (List l) = show l
    showList l = showChar '[' . (showString . unwords . map show) l . showChar ']'

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
            return (AST.Char c)) <++
        (do T.String s <- lexP
            return (AST.String s)) <++
        (do s <- lift $ do
                skipSpaces
                munch1 $ \c -> not (isSpace c || c `elem` "[]")
            case s of
                "#true" -> return (Bool True)
                "#false" -> return (Bool False)
                _ -> return (AST.Symbol s))
    readListPrec = liftA2 (:) readPrec readListPrec <++ pure []
