import AST
import Ni

main = do
    stdlib <- readFile "src/stdlib.ni" >>= readIO
    program <- getContents >>= readIO
    run (eval (stdlib <> program)) initialContext
