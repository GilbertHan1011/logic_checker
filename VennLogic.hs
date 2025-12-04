module VennLogic where

import Data.List (subsequences, nub)

-- 1. DEFINE THE UNIVERSE
-- We have three predicates: A, B, C
data Atom = A | B | C deriving (Eq, Show, Enum, Ord)

-- A "Region" in the Venn diagram is defined by which Atoms are True in it.
-- e.g., [A, C] means A is True, C is True, B is False.
type Region = [Atom]

-- A "World" is a specific Venn Diagram configuration.
-- It is a list of ALL regions that are currently "Inhabited" (non-empty).
-- If a region is not in this list, it is Empty.
type World = [Region]

-- Generate all 8 possible regions (The "Master" Venn Diagram)
allRegions :: [Region]
allRegions = subsequences [A, B, C]

-- Generate all 256 possible Worlds (Every combination of empty/inhabited regions)
possibleWorlds :: [World]
possibleWorlds = subsequences allRegions

-- 2. DEFINE THE PROPOSITIONS
-- These match the logical statements in your text.
data Prop = 
    Every Atom Atom      -- All A is B
  | Some Atom Atom       -- Some A is B
  | No Atom Atom         -- No A is B
  | SomeNot Atom Atom    -- Some A is not B
  deriving (Show, Eq)

-- 2a. ALGEBRAIC TRANSFORMATIONS ON PROPOSITIONS
-- These work purely symbolically, without referring to Venn worlds.

-- 3. THE LOGIC CHECKER
-- This function checks if a single Proposition is true in a specific World.
check :: World -> Prop -> Bool
check world p = case p of
    -- "Every A is B": Look for any inhabited region where A is present but B is missing.
    -- If such a region exists, the statement is False.
    Every a b -> not $ any (\r -> a `elem` r && b `notElem` r) world
    
    -- "Some A is B": Look for at least one inhabited region containing both A and B.
    Some a b  -> any (\r -> a `elem` r && b `elem` r) world
    
    -- "No A is B": Look for any inhabited region containing both A and B.
    -- If found, the statement is False.
    No a b    -> not $ any (\r -> a `elem` r && b `elem` r) world
    
    -- "Some A is not B": Look for at least one inhabited region with A but without B.
    SomeNot a b -> any (\r -> a `elem` r && b `notElem` r) world

-- 4. THE VALIDITY PROVER
-- An argument is Valid if: In ALL worlds where the Premises are True, the Conclusion is also True.
isValid :: [Prop] -> Prop -> (Bool, String)
isValid premises conclusion = 
    let 
        -- Filter the 256 worlds down to only those where all premises hold
        consistentWorlds = filter (\w -> all (check w) premises) possibleWorlds
    in 
        if null consistentWorlds
        then (True, "Valid (Vacuously - Premises represent an impossible contradiction)")
        else 
            -- Check if the conclusion holds in ALL remaining consistent worlds
            let valid = all (\w -> check w conclusion) consistentWorlds
            in if valid 
               then (True, "Valid")
               else (False, "Invalid (Counter-example found)")



-- 5. EXAMPLES FROM THE TEXT
main :: IO ()
main = do
    putStrLn "--- 1. The 'Barbara' Syllogism (Valid) ---"
    -- All A is B, All B is C -> All A is C
    print $ isValid [Every A B, Every B C] (Every A C)

    putStrLn "\n--- 2. The 'Antilogism' / Fiona Logic (Valid) ---"
    -- From text: "a->b" and "a -/-> c" (Some a is not c) implies "Some b is not c"
    -- If All A is B, and Some A is Not C -> Then Some B is Not C
    print $ isValid [Every A B, SomeNot A C] (SomeNot B C)

    putStrLn "\n--- 3. The 'Dogs/Insects/Yucky' Example (Valid) ---"
    -- No B is C, Some C is A -> Some A is Not B
    print $ isValid [No B C, Some C A] (SomeNot A B)

    putStrLn "\n--- 4. The 'Plants/Fungi/Flowers' Example (Invalid) ---"
    -- All A is B, Some C is Not A -> Some C is Not B
    -- This fails because the 'Some' might fall into the intersection of C and B.
    print $ isValid [Every A B, SomeNot C A] (SomeNot C B)
