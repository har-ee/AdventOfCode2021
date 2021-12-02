import AOCUtils

main :: IO ()
main = do
  input <- readFile "day2/input"
  print $ navigate $ lines input

navigate :: [String] -> Int
navigate = navigate' (0, 0) 0

navigate' :: (Int, Int) -> Int -> [String] -> Int
navigate' (x, y) aim (line:xs)
  | dir == "forward" = navigate' (x + i, y + aim * i) aim xs
  | dir == "up"      = navigate' (x, y) (aim - i) xs
  | dir == "down"    = navigate' (x, y) (aim + i) xs
  where
    [dir, rawint] = split line " "
    i = read rawint
navigate' (x, y) _ [] = x * y