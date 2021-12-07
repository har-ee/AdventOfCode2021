main :: IO ()
main = do
  input <- readFile "day7/input"
  let i = read ("[" ++ input ++ "]") :: [Int]

  print $ minimum $ map (fuelCost i) [minimum i .. maximum i]

fuelCost :: [Int] -> Int -> Int
fuelCost crabs pos = sum [abs (crab - pos) | crab <- crabs]