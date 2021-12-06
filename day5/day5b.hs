import AOCUtils (split)
import Data.List (group, sort)

type Card = [[Int]]

main :: IO ()
main = do
  input <- readFile "day5/input"
  let i = lines input
  let points = concatMap (getPoints . parse) i

  print $ length $ filter (\x -> length x > 1) (group $ sort points)

getPoints :: ((Int, Int), (Int, Int)) -> [(Int, Int)]
getPoints ((x1, y1), (x2, y2)) = zip (calcCoords x1 x2 maxdiff) (calcCoords y1 y2 maxdiff)
  where
    maxdiff = max (abs (x2 - x1)) (abs (y2 - y1))

calcCoords :: Int -> Int -> Int -> [Int]
calcCoords a1 a2 n = [a1 + (i * signum (a2 - a1)) | i <- [0..abs n]]

parse :: String -> ((Int, Int), (Int, Int))
parse line = (l, r)
  where
    [l, r] = [(read :: String -> (Int, Int)) ("(" ++ s ++ ")") | s <- split line " -> "]