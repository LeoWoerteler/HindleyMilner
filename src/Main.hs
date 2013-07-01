module Main where

import Data.Map      ( empty )
import HindleyMilner ( hindleyMilner, parseLExpr, typeOf )
import System.IO     ( stdout, hFlush )

main::IO()
main = do
  line <- putStr "> " >> hFlush stdout >> getLine
  case line of
    ":exit" -> putStrLn "Goodbye :-)."
    _       -> do
      processLine line
      main
  where
    processLine l = case parseLExpr l of
      Left err   -> putStrLn $ "Parsing error:\n" ++ err
      Right expr -> do
        putStrLn $ "Expression: " ++ show expr
        putStrLn $ case hindleyMilner empty expr of
          Left  err   -> "Type error:\n" ++ err
          Right expr' -> "Type: " ++ show (typeOf expr')
