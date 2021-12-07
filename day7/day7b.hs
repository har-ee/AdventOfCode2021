main :: IO ()
main = do
  input <- readFile "day7/input"
  let i = read ("[" ++ input ++ "]") :: [Int]

  print $ minimum $ map (fuelCost i) [minimum i .. maximum i]

fuelCost :: [Int] -> Int -> Int
fuelCost crabs pos = sum [triangle $ abs (crab - pos) | crab <- crabs]

triangle :: Int -> Int
triangle i = i * (i + 1) `div` 2