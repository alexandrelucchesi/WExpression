import Control.Monad

import qualified Data.Attoparsec.Text as A

import Data.Char

import qualified Data.Text as T

import System.Environment
import System.IO hiding (withFile)

import WExpression

main = do
    putStrLn "WXPi, version 1.0.0"
    putStrLn ""
    putStrLn "This is WExpression's interactive mode."
    putStrLn "What are you waiting for? Enter some expressions! :-)"
    putStrLn ""
    putStrLn "\t:q to exit."
    putStrLn "\t:f to see loaded functions."
    putStrLn ""
    args <- getArgs
    case args of
        [] -> noFile
        _  -> withFile (args !! 0)

-- If a source file is not specified, launchs the interpreter for expression evaluation.
noFile = interactive (Right [])

-- Else, launchs the interpreter for both expression and function evaluation.
withFile fileName = do
    handle <- openFile fileName ReadMode
    contents <- hGetContents handle
    let funcDecls = A.parseOnly pFuncDecls (T.pack contents)
    printFunctions funcDecls
    putStrLn ""
    hClose handle
    interactive funcDecls

printFunctions (Right []) = putStrLn "No functions have been loaded."
printFunctions (Right funcDecls) = do
    putStr "Functions loaded: "
    print $ (map fname funcDecls)

-- Interative loop.
-- Type ":q" to quit.
interactive funcDecls = do
    case funcDecls of
        Right [] -> putStr "WExpression"
        Right xs -> do
            (progName:_) <- getArgs
            putStr $ let (x:xs) = map toLower $ takeWhile (/= '.') progName in ((toUpper x):xs) ++ "*"
    putStr "> "
    hFlush stdout
    line <- getLine
    if line == ":q"
        then putStrLn "Bye!"
    else return ()
    if line == ":f"
        then do
            printFunctions funcDecls
            interactive funcDecls
    else return ()
    when (line /= ":q" && line /= ":f") $ do
        let expr = (A.parseOnly pExpression (T.pack line))
    --  print expr
        run' expr funcDecls
        interactive funcDecls
            where
                run' (Right expr) (Right funcDecls) = do
                    print $ run expr funcDecls
                run' (Left err1) (Right _) = do
                    putStrLn $ "Error parsing expression: " ++ err1
                run' (Right _) (Left err2) = do
                    putStrLn $ "Error parsing functions: " ++ err2
                run' (Left err1) (Left err2) = do
                    putStrLn "Run you fools! Everything is wrong. :-("

