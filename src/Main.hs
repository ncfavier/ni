import System.IO
import System.Exit
import System.Posix.Terminal
import System.Posix.IO
import Control.Monad
import Control.Monad.Trans.Class

import Base
import Ni

-- TODO: argument processing
main = runNi $ do
    stdlib <- lift $ readFile "src/stdlib.ni" >>= readIO
    eval (List stdlib)
    isTTY <- lift $ queryTerminal stdInput
    if isTTY then do
        lift $ do
            putStrLn "Ni!"
            hSetBuffering stdout NoBuffering
        forever $ do
            lift $ do
                putStr ">>> "
                done <- isEOF
                when done $ putChar '\n' >> exitSuccess
            l <- lift readLn
            eval (List l)
    else do
        l <- lift $ getContents >>= readIO
        eval (List l)
