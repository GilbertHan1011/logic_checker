module Main where

import AppLogic
import System.IO (hFlush, stdout)

prompt :: String -> IO String
prompt msg = do
  putStr msg
  hFlush stdout
  getLine

readPremises :: IO [InputForm]
readPremises = go 1 []
  where
    go :: Int -> [InputForm] -> IO [InputForm]
    go n acc = do
      line <- prompt $ "Premise " ++ show n ++ " (blank to finish): "
      if null line
        then return (reverse acc)
        else case parseInputForm line of
          Left err -> do
            putStrLn err
            go n acc
          Right pf -> go (n + 1) (pf : acc)

readConclusion :: IO InputForm
readConclusion = do
  line <- prompt "Conclusion: "
  if null line
    then do
      putStrLn "Conclusion cannot be empty."
      readConclusion
    else case parseInputForm line of
      Left err -> do
        putStrLn err
        readConclusion
      Right cf -> return cf

loop :: IO ()
loop = do
  putStrLn "\nEnter an argument to be checked."
  putStrLn "Format per line: Quantifier Subject Predicate"
  putStrLn "  Quantifier: Every | Some | No | SomeNot"
  putStrLn "  Subject, Predicate: A | B | C"
  putStrLn "Example premise: Every A B"
  putStrLn "Enter premises first, then a blank line, then the conclusion."
  premises <- readPremises
  if null premises
    then putStrLn "No premises entered; nothing to check."
    else do
      conclusion <- readConclusion
      runBoth premises conclusion
  again <- prompt "Check another argument? (y/n): "
  if again `elem` ["y", "Y", "yes", "Yes"]
    then loop
    else putStrLn "Goodbye."

main :: IO ()
main = do
  putStrLn "Logical argument checker (Venn + Algebraic)"
  loop



