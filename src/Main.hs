import AST
import Ni
import Builtin

main = do
    program <- getContents >>= readIO :: IO [Value]
    run (eval program) initialContext
