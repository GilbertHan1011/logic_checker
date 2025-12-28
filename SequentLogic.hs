module SequentLogic where

import AppLogic
import Data.List (find, delete)

prove :: Sequent -> Bool
prove seq = 
    if isIdentity seq 
    then True 
    else case tryReduce seq of
        Just simplifiedBranches -> all prove simplifiedBranches
        Nothing                 -> False


-- | 1. IDENTITY AXIOM (The "Immediate" Rule)
--   Valid if the intersection of Left and Right contains any common Atom.
--   e.g. "A, B |- C, A" is valid because A is on both sides.
isIdentity :: Sequent -> Bool
isIdentity (Sequent l r) = 
    any (`elem` r) l

-- | 2. REDUCTION RULES
--   Finds the first compound formula, breaks it down, and returns the new sub-goals.
tryReduce :: Sequent -> Maybe [Sequent]
tryReduce seq@(Sequent l r) =
    case findCompound l of
        -- Rule: Left Connectives (Assumptions)
        Just expr -> applyLeftRule expr seq
        Nothing   -> case findCompound r of
            -- Rule: Right Connectives (Conclusions)
            Just expr -> applyRightRule expr seq
            -- No compound formulas left on either side? We are stuck.
            Nothing   -> Nothing

-- | Helper: Find the first expression that is NOT an Atom
findCompound :: [Expr] -> Maybe Expr
findCompound [] = Nothing
findCompound (Atom _:xs) = findCompound xs
findCompound (x:_) = Just x

applyLeftRule :: Expr -> Sequent -> Maybe [Sequent]
applyLeftRule expr (Sequent l r) = 
    let l' = delete expr l -- Remove the formula we are processing
    in case expr of
        
        -- Rule: Not Left (~A |- ...) => ( |- A, ...)
        -- Logic: Move A to the Right.
        Not a -> Just [Sequent l' (a : r)]

        -- Rule: And Left (A & B |- ...) => (A, B |- ...)
        -- Logic: Assume A and Assume B.
        And a b -> Just [Sequent (a : b : l') r]

        -- Rule: Or Left (A | B |- ...) => Splits into 2 branches
        -- Logic: Case 1: Assume A. Case 2: Assume B. Both must be true.
        Or a b -> Just 
            [ Sequent (a : l') r
            , Sequent (b : l') r 
            ]

        -- Rule: Implies Left (A -> B |- ...) => Splits into 2 branches
        -- Logic: Either A is false (Right side A) OR B is true (Left side B).
        -- Corresponds to rule: (A -> B) is equivalent to (~A | B)
        Implies a b -> Just
            [ Sequent l' (a : r)      -- Branch 1: Prove A (implies A is false in context)
            , Sequent (b : l') r      -- Branch 2: Assume B
            ]

        _ -> Nothing -- Should not happen if findCompound works correctly

-------------------------------------------------------------------------------
-- RIGHT RULES (Handling Conclusions)
-- These remove the compound formula from Right and replace it with simpler ones.
-------------------------------------------------------------------------------

applyRightRule :: Expr -> Sequent -> Maybe [Sequent]
applyRightRule expr (Sequent l r) =
    let r' = delete expr r -- Remove the formula we are processing
    in case expr of

        -- Rule: Not Right ( |- ~A) => (A |- )
        -- Logic: Move A to the Left.
        Not a -> Just [Sequent (a : l) r']

        -- Rule: Or Right ( |- A | B) => ( |- A, B)
        -- Logic: We can prove A OR prove B. Just list them.
        Or a b -> Just [Sequent l (a : b : r')]

        -- Rule: And Right ( |- A & B) => Splits into 2 branches
        -- Logic: We must prove A AND we must prove B.
        And a b -> Just 
            [ Sequent l (a : r')
            , Sequent l (b : r') 
            ]

        -- Rule: Implies Right ( |- A -> B) => (A |- B)
        -- Logic: To prove A implies B, assume A and try to prove B.
        Implies a b -> Just [Sequent (a : l) (b : r')]

        _ -> Nothing