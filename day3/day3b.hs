import AOCUtils (frequency, strToBin)
import Data.List (transpose)

main :: IO ()
main = do
  input <- readFile "day3/input"

  let ox = calcOx $ lines input
  let co = calcCo $ lines input

  print $ strToBin ox * strToBin co

calcOx :: [String] -> String
calcOx = calc 0 calcGamma

calcCo :: [String] -> String
calcCo = calc 0 calcEpsilon

calc :: Int -> (String -> Char) -> [String] -> String
calc n fun (x : xs)
  | null xs = x
  | otherwise = calc (n + 1) fun remaining
  where
    val = fun $ transpose (x : xs) !! n
    remaining = filter (\nums -> (nums !! n) == val) (x : xs)

calcGamma :: String -> Char
calcGamma xs
  | frequency '1' xs * 2 >= length xs = '1'
  | otherwise = '0'

calcEpsilon :: String -> Char
calcEpsilon x = if calcGamma x == '1' then '0' else '1'