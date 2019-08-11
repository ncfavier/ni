import AST
import Ni

main = do
    program <- getContents >>= readIO
    run (eval program) initialContext
