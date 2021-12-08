import AOCUtils (split)
import Data.List (elemIndex, permutations)
import Data.Maybe (fromJust)
import Data.Set (Set, fromList)

main :: IO ()
main = do
  input <- readFile "day8/input"

  let parsedinput = map parseLine (lines input)

  print $ sum $ map bruteForce parsedinput

-- Convert line of input into Tuple of LHS and RHS segment lists
parseLine :: String -> ([String], [String])
parseLine xs = (a, b)
  where
    a : b : _ = map (`split` " ") (split xs " \\| ")

-- Helpful constants
digitSet :: [Set Char]
digitSet = map fromList ["abcefg", "cf", "acdeg", "acdfg", "bcdf", "abdfg", "abdefg", "acf", "abcdefg", "abcdfg"]

-- Apply the given permutation to the given string
applyPerm :: String -> String -> String
applyPerm xs perm = [perm !! fromJust (elemIndex x mapping) | x <- xs]
  where
    mapping = "abcdefg"

-- Check if the given permutation converts the given list to valid digits
checkPerm :: [String] -> String -> Bool
checkPerm (x : xs) perm
  | any (\s -> s == fromList permed) digitSet = checkPerm xs perm
  | otherwise = False
  where
    permed = applyPerm x perm
checkPerm [] _ = True

-- Convert a segment string to the int it represents
getDigit :: String -> Int
getDigit xs = fromJust (elemIndex (fromList xs) digitSet)

-- Find valid permutation for the given segments then return RHS number
bruteForce :: ([String], [String]) -> Int
bruteForce (inputs, rawdigits) = parseNumber digits
  where
    perm = head $ filter (checkPerm inputs) (permutations "abcdefg")
    digits = map (`applyPerm` perm) rawdigits

-- Convert segment strings to number they represent
parseNumber :: [String] -> Int
parseNumber (x : xs) = digit + (10 * parseNumber (init (x : xs)))
  where
    digit = getDigit (last (x : xs))
parseNumber [] = 0