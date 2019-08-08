import AST

main = do
    input <- getContents
    program <- readIO input :: IO Program
    print program
