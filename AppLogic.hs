module AppLogic where

import qualified AlgebraicLogic as A
import qualified VennLogic      as V
import Data.Char (toLower)

-- Shared quantifier type for the application
data Quantifier = QEvery | QSome | QNo | QSomeNot
  deriving (Show, Eq)

-- Simple three-term input: quantifier + subject + predicate
-- Subject / predicate are restricted to 'A', 'B', 'C'.
data InputForm = InputForm
  { quantifier :: Quantifier
  , subjectVar :: Char
  , predicateVar :: Char
  } deriving (Show, Eq)

-- Pretty-print helper
showInputForm :: InputForm -> String
showInputForm (InputForm q s p) =
  quantStr q ++ " " ++ [s] ++ " " ++ [p]
  where
    quantStr QEvery   = "Every"
    quantStr QSome    = "Some"
    quantStr QNo      = "No"
    quantStr QSomeNot = "SomeNot"

-- Map InputForm to the Venn-based Prop (if possible)
toVennAtom :: Char -> Maybe V.Atom
toVennAtom 'A' = Just V.A
toVennAtom 'B' = Just V.B
toVennAtom 'C' = Just V.C
toVennAtom _   = Nothing

toVennProp :: InputForm -> Maybe V.Prop
toVennProp (InputForm q s p) = do
  a <- toVennAtom s
  b <- toVennAtom p
  pure $ case q of
    QEvery   -> V.Every a b
    QSome    -> V.Some a b
    QNo      -> V.No a b
    QSomeNot -> V.SomeNot a b

-- Map InputForm to the algebraic Stmt using AlgebraicLogic.normalize
quantToString :: Quantifier -> String
quantToString QEvery   = "Every"
quantToString QSome    = "Some"
quantToString QNo      = "No"
quantToString QSomeNot = "SomeNot"

charToTerm :: Char -> A.Term
charToTerm 'A' = A.Var "a"
charToTerm 'B' = A.Var "b"
charToTerm 'C' = A.Var "c"
charToTerm c   = A.Var [c]

toAlgStmt :: InputForm -> A.Stmt
toAlgStmt (InputForm q s p) =
  let qs = quantToString q
      ts = charToTerm s
      tp = charToTerm p
  in A.normalize qs ts tp

-- Run the Venn-based checker
runVenn :: [InputForm] -> InputForm -> IO ()
runVenn premises conclusion = do
  putStrLn "=== Venn / World-Model Checker ==="
  case mapM toVennProp premises of
    Nothing -> putStrLn "Error: could not map some premise to Venn atoms (only A, B, C allowed)."
    Just vennPremises ->
      case toVennProp conclusion of
        Nothing -> putStrLn "Error: could not map conclusion to Venn atoms (only A, B, C allowed)."
        Just vennConclusion -> do
          let (ok, msg) = V.isValid vennPremises vennConclusion
          putStrLn $ "Premises:   " ++ show vennPremises
          putStrLn $ "Conclusion: " ++ show vennConclusion
          putStrLn $ "Verdict:    " ++ show ok ++ " - " ++ msg

-- Run the algebraic checker
runAlgebraic :: [InputForm] -> InputForm -> IO ()
runAlgebraic premises conclusion = do
  putStrLn "=== Algebraic / Barbara-Triad Checker ==="
  let algPremises   = map toAlgStmt premises
      algConclusion = toAlgStmt conclusion
      (ok, msg)     = A.isValid algPremises algConclusion
  putStrLn $ "Premises:   " ++ show algPremises
  putStrLn $ "Conclusion: " ++ show algConclusion
  putStrLn $ "Verdict:    " ++ show ok ++ " - " ++ msg

-- Run both systems on the same structured input
runBoth :: [InputForm] -> InputForm -> IO ()
runBoth premises conclusion = do
  putStrLn "========================================="
  putStrLn "Argument:"
  mapM_ (putStrLn . ("  " ++) . showInputForm) premises
  putStrLn $ "  Therefore: " ++ showInputForm conclusion
  putStrLn "-----------------------------------------"
  runVenn premises conclusion
  putStrLn ""
  runAlgebraic premises conclusion
  putStrLn "========================================="

-- 7. PARSING FROM SIMPLE TEXT INPUT
--
-- Expected format for each line:
--   Quantifier Subject Predicate
-- where:
--   Quantifier ∈ {Every, Some, No, SomeNot} (case-insensitive)
--   Subject, Predicate ∈ {A,B,C} (single letters)

parseQuantifier :: String -> Maybe Quantifier
parseQuantifier s =
  case map toLower s of
    "every"   -> Just QEvery
    "some"    -> Just QSome
    "no"      -> Just QNo
    "somenot" -> Just QSomeNot
    _         -> Nothing

parseInputForm :: String -> Either String InputForm
parseInputForm line =
  case words line of
    [qStr, sStr, pStr]
      | length sStr == 1
      , length pStr == 1 -> 
          case parseQuantifier qStr of
            Nothing ->
              Left "Unknown quantifier. Use one of: Every, Some, No, SomeNot."
            Just q  ->
              Right $ InputForm q (head sStr) (head pStr)
    _ ->
      Left "Could not parse. Expected: Quantifier Subject Predicate (e.g. \"Every A B\")."



