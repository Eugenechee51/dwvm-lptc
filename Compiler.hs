module Main where

import System.Environment
import Parser
import Text.ParserCombinators.Parsec

main :: IO ()
main = do
    args <- getArgs
    if length args /= 1 then do
        putStrLn "cdvm <source_file.cdv>"
    else
        let file = head args in do
            content <- readFile file
            res <- case parse asTree file content of
                Left e  -> return $ show e
                Right s -> return $ show s
            putStrLn res
