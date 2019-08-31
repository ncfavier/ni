{-# LANGUAGE BlockArguments #-}
import System.IO
import System.Exit
import System.Posix.Terminal
import System.Posix.IO
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.State

import Base
import Ni

initialContext = Context [] [ioEnvironment, baseEnvironment]
runNi n = void $ runStateT n initialContext

io = liftIO
whenM m a = m >>= flip when a

-- TODO: argument processing
main = runNi do
    stdlib <- io $ readFile "src/stdlib.ni" >>= readIO
    eval (List stdlib)
    isTTY <- io $ queryTerminal stdInput
    if isTTY then do
        io do
            putStrLn "Ni!"
            hSetBuffering stdout NoBuffering
        forever do
            l <- io do
                putStr ">>> "
                whenM isEOF do
                    putChar '\n'
                    exitSuccess
                readLn
            eval (List l)
    else do
        l <- io $ getContents >>= readIO
        eval (List l)
