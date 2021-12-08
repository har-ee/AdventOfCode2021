import AOCUtils (split)

main :: IO ()
main = do
  input <- readFile "day8/input"
  let parsedinput = map parseLine (lines input)

  print $ length $ concatMap (filter (\x -> length x `elem` [2, 4, 3, 7])) parsedinput

parseLine :: String -> [String]
parseLine xs = map (`split` " ") (split xs " \\| ") !! 1