import AOCUtils (frequency, strToBin)
import Data.List (transpose)

main :: IO ()
main = do
  input <- readFile "day3/input"
  
  let transposedinput = transpose $ lines input

  let gamma = map calcGamma transposedinput
  let epsilon = map calcEpsilon transposedinput

  print $ strToBin gamma * strToBin epsilon

calcGamma :: String -> Char
calcGamma xs
  | frequency '1' xs * 2 >= length xs = '1'
  | otherwise = '0'

calcEpsilon :: String -> Char
calcEpsilon x = if calcGamma x == '1' then '0' else '1'