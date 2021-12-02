import AOCUtils

main :: IO ()
main = do
  input <- readFile "day2/input"
  print $ navigate $ lines input

navigate :: [String] -> Int
navigate = navigate' (0, 0)

navigate' :: (Int, Int) -> [String] -> Int
navigate' (x, y) (line:xs)
  | dir == "forward" = navigate' (x + i, y) xs
  | dir == "up"      = navigate' (x, y - i) xs
  | dir == "down"    = navigate' (x, y + i) xs
  where
    [dir, rawint] = split line " "
    i = read rawint
navigate' (x, y) [] = x * y