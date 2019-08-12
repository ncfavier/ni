import System.Exit
import System.Posix.Terminal
import System.Posix.IO

import AST
import Ni

main = do
    stdlib <- readFile "src/stdlib.ni" >>= readIO
    ctx <- execNi (eval stdlib) initialContext
    stdin <- getContents
    loop stdin ctx
    where
        loop s ctx = case reads s of
            [(v, s')] -> do
                ctx' <- execNi (valueToNi v) ctx
                loop s' ctx'
            [] -> return ()
            _ -> die "Ambiguous parse."
