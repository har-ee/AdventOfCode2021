import AOCUtils (frequency)
import Data.List (group, sort)

main :: IO ()
main = do
  input <- readFile "day6/input"
  let i = read ("[" ++ input ++ "]") :: [Int]
  let frequencies = map (`frequency` i) [0..8] 

  print $ sum $ iterate simulate frequencies !! 256

simulate :: [Int] -> [Int]
simulate (a0:a1:a2:a3:a4:a5:a6:a7:a8:xs) = a1:a2:a3:a4:a5:a6:(a7 + a0):a8:a0:xs