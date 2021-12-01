main :: IO ()
main = do
  input <- readFile "day1/input"
  let sonar = map (read :: String -> Int) (lines input)

  let window = zipWith3 (\x y z -> x + y + z) sonar (tail sonar) (tail $ tail sonar)
  let answer = length $ filter id $ zipWith (<) window (tail window)
  print answer