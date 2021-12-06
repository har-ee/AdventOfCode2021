module AOCUtils where
  
import Text.Regex.TDFA ((=~))
import Data.Bits (shiftR)

-- Splits string based on regex
split :: String -> String -> [String]
split [] _ = []
split str regex = before : split upcoming regex
  where
    (before, _, upcoming) = str =~ regex :: (String, String, String)

-- Count frequency of element in list
frequency :: Eq a => a -> [a] -> Int
frequency n xs = length ([a | a <- xs, a == n])

-- Convert int into String of binary digits, i.e. 23 -> "10111"
binToStr :: Int -> String
binToStr i
  | i == 0    = ""
  | otherwise = binToStr (shiftR i 1) ++ show (i `mod` 2)

-- Convert String representing binary into Int, i.e. "10111" -> 23
strToBin :: String -> Int
strToBin (x:xs) = (i * 2 ^ length xs) + strToBin xs
  where
    i = read [x]
strToBin [] = 0

-- Calculate range from lowest of a,b to largest of a,b
intRange :: Int -> Int -> [Int]
intRange a b = [min a b .. max a b]