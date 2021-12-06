import AOCUtils (split, intRange)
import Data.List (sort, group)

main :: IO ()
main = do
  input <- readFile "day5/input"
  let i = lines input
  let points = concatMap (getPoints . parse) i

  print $ length $ filter (\x -> length x > 1) (group $ sort points)

getPoints :: ((Int, Int), (Int, Int)) -> [(Int, Int)]
getPoints ((x1, y1), (x2, y2))
  | x2 - x1 == 0 = [(x1, y) | y <- intRange y1 y2]
  | y2 - y1 == 0 = [(x, y1) | x <- intRange x1 x2]
  | otherwise  = []

parse :: String -> ((Int,Int),(Int, Int))
parse line = (l, r)
  where
    [l, r] = [(read :: String -> (Int,Int)) ("(" ++ s ++ ")") | s <- split line " -> " ]

