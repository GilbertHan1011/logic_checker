module AlgebraicLogic where

import Data.List (permutations)

-- 1. DEFINE TERMS
-- A term can be a simple variable "A" or its negation "not A"
data Term = Var String | Not (Term) deriving (Eq)

instance Show Term where
    show (Var s) = s
    show (Not t) = "~" ++ show t

-- Helper to flip a term (not A -> A; A -> not A)
negateTerm :: Term -> Term
negateTerm (Not t) = t
negateTerm t       = Not t

-- 2. DEFINE STATEMENTS
-- We normalize everything into two types:
-- "Every A B" (Universal) and "SomeNot A B" (Existential)
data Stmt = Every Term Term | SomeNot Term Term deriving (Eq)

instance Show Stmt where
    show (Every a b)   = "All " ++ show a ++ " are " ++ show b
    show (SomeNot a b) = "Some " ++ show a ++ " is not " ++ show b

-- 3. ALGEBRAIC RULES

-- Rule A: Normalization (reducing 'No' and 'Some' to our core types)
-- This allows us to handle standard inputs by converting them.
normalize :: String -> Term -> Term -> Stmt
normalize "Every" a b   = Every a b
normalize "SomeNot" a b = SomeNot a b
normalize "No" a b      = Every a (negateTerm b)      -- No A is B = All A are (not B)
normalize "Some" a b    = SomeNot a (negateTerm b)    -- Some A is B = Some A is not (not B)

-- Rule B: Negation (For the "Deny the Conclusion" step)
-- The opposite of "Every A is B" is "Some A is Not B"
negateStmt :: Stmt -> Stmt
negateStmt (Every a b)   = SomeNot a b
negateStmt (SomeNot a b) = Every a b

-- Rule C: Contraposition (Used to align the chain)
-- "Every A B" is algebraically equivalent to "Every (not B) (not A)"
contrapositive :: Stmt -> Stmt
contrapositive (Every a b) = Every (negateTerm b) (negateTerm a)
contrapositive s           = s -- SomeNot doesn't have a clean contrapositive in this system

-- 4. THE MATCHER (Finding the Barbara Pattern)

-- We look for the "Barbara Triad": {Every X Y, Every Y Z, SomeNot X Z}
-- This implies: X->Y->Z, but X doesn't make it to Z. Impossible!
isBarbaraTriad :: [Stmt] -> Bool
isBarbaraTriad stmts = 
    let
        -- We need exactly two Universals and one Existential
        universals = [s | s@(Every _ _) <- stmts]
        existentials = [s | s@(SomeNot _ _) <- stmts]
    in
        length universals == 2 && length existentials == 1 &&
        matchChain (universals !! 0) (universals !! 1) (head existentials)

-- Check if Universals U1 and U2 form a chain that conflicts with Existential E
matchChain :: Stmt -> Stmt -> Stmt -> Bool
matchChain u1 u2 e = 
    check u1 u2 e || check u2 u1 e -- Order of universals doesn't matter
    where 
        -- Check if U1 connects to U2 (Every A B, Every B C)
        -- And if E contradicts the start/end (SomeNot A C)
        check (Every a b) (Every y z) (SomeNot start end) =
            (b == y) && (a == start) && (z == end)
        check _ _ _ = False

-- 5. THE SOLVER
-- This function tries to prove validity by finding a contradiction
isValid :: [Stmt] -> Stmt -> (Bool, String)
isValid premises conclusion = 
    let 
        -- STEP 1: Deny the Conclusion (Antilogism)
        triad = premises ++ [negateStmt conclusion]
        
        -- STEP 2: Generate all algebraic variations
        -- We permute the list, and for every "Every" statement, 
        -- we also try its Contrapositive.
        variations = expandContrapositives triad
    in 
        -- STEP 3: Check if ANY variation matches the Barbara Triad
        if any isBarbaraTriad variations
        then (True, "Valid! (Reduces to Barbara Triad)")
        else (False, "Invalid (Cannot form a contradiction chain)")

-- Helper to generate list variations including contrapositives
expandContrapositives :: [Stmt] -> [[Stmt]]
expandContrapositives list = 
    let 
        -- For each statement, allow it to be itself OR its contrapositive
        options = map (\s -> [s, contrapositive s]) list
        -- Generate all combinations
        combos = sequence options
    in 
        -- Generate all permutations of those combinations
        concatMap permutations combos

-- 6. RUN THE EXAMPLE
main :: IO ()
main = do
    let a = Var "a"
    let b = Var "b"
    let c = Var "c"

    putStrLn "Checking: [Every a b, SomeNot c a] -> SomeNot c b"
    
    -- Your specific example:
    let p1 = Every a b
    let p2 = SomeNot c a
    let conc = SomeNot c b
    
    let (result, msg) = isValid [p1, p2] conc
    putStrLn $ "Result: " ++ show result ++ " - " ++ msg

    putStrLn "\n----------------"
    putStrLn "Trace of what happened:"
    putStrLn $ "1. Triad formed: " ++ show [p1, p2, negateStmt conc]
    putStrLn $ "   (Note: negated conclusion " ++ show conc ++ " became " ++ show (negateStmt conc) ++ ")"
    putStrLn "2. We looked for a chain: X -> Y -> Z vs Some X not Z"
    putStrLn "3. Analysis:" 
    putStrLn "   We have 'Every a b' (a->b)"
    putStrLn "   We have 'Every c b' (c->b) (from negated conclusion)"
    putStrLn "   We have 'Some c not a'"
    putStrLn "   We cannot chain (a->b) and (c->b) to connect c to a."
    putStrLn "   Therefore, no contradiction found."