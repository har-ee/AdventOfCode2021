import AOCUtils (split)
import Data.List (maximumBy, transpose)
import Data.Map (Map, fromList, (!))
import Data.Ord (comparing)

type Card = [[Int]]

main :: IO ()
main = do
  input <- readFile "day4/input"
  let i = lines input
  let nums = read ("[" ++ head i ++ "]") :: [Int]
  let cards = parseBingo (drop 2 i)

  let calltimes = fromList $ zip nums [1 ..]
  let winner = maximumBy (comparing (`winTime` calltimes)) cards
  let score = calcScore winner calltimes
  print score

-- Return the time at which the given card will win, based on the given calltimes map
winTime :: Card -> Map Int Int -> Int
winTime xs calltimes = min (winTime' xs calltimes) (winTime' (transpose xs) calltimes)

winTime' :: Card -> Map Int Int -> Int
winTime' xs calltimes = minimum [maximum times | row <- xs, let times = map (calltimes !) row]

calcScore :: Card -> Map Int Int -> Int
calcScore card calltimes = winnum * sum (filter (\x -> calltimes ! x > wintime) flattened)
  where
    flattened = concat card
    wintime = winTime card calltimes
    winnum = head $ filter (\x -> wintime == (calltimes ! x)) flattened

parseBingo :: [String] -> [Card]
parseBingo xs
  | null xs = []
  | otherwise = map parseRow' card : parseBingo (drop 6 xs)
  where
    card = take 5 xs

parseRow' :: String -> [Int]
parseRow' a = map read (filter (/= "") (split a " "))