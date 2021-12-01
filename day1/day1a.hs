main :: IO ()
main = do
  input <- readFile "day1/input"
  let sonar = map (read :: String -> Int) (lines input)

  let answer = length $ filter id $ zipWith (<) sonar (tail sonar)
  print answer