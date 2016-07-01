import System.Environment (getArgs)

main :: IO ()
main = do
    args <- getArgs
    inp <- getContents
    case first args of
      Just "-?" -> do
        putStrLn "usage: rpnc [options]"
        putStrLn "  Compile expressions from stdin and emit instructions to stdout."
        putStrLn "  -t  tokenize only"
        putStrLn "  -p  parse only"
        putStrLn "  -o  optimize"
      Just "-t" -> do
        -- call lexer
        putStrLn "call lexer"
      Just "-p" -> do
        -- call parser
        putStrLn "call parser"
      Just "-o" -> do
        -- compile with optimizations
        putStrLn "compile with optimizations"
      Just arg -> do
        putStrLn $ arg ++ ": unrecognized option"
      Nothing -> do
        -- compile without optimizations
        putStrLn "compile"
  where
    first [] = Nothing
    first (arg : _) = Just arg
