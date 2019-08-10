import AST
import Ni

main = do
    program <- getContents >>= readIO :: IO [Value]
    run (eval program) initialContext
