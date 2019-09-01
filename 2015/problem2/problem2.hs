-- Easily scalable to n-dimensional presents

import Data.List (sort, subsequences)
import Data.List.Split (splitOn)

toInts :: String -> [Int]
toInts  = sort . map read . splitOn "x"

toSides :: [Int] -> [[Int]]
toSides = filter ((==2) . length) . subsequences

calculatePaper  :: [[Int]] -> Int
calculatePaper x = (head products) + ((*2) . sum $ products)
  where products = map product x

calculateRibbon :: [Int] -> Int
calculateRibbon x = (sum . take 2 . map (2*) $ x) + (product x)

main :: IO ()
main = do
  file <- readFile "problem2.input"
  let values = map toInts . lines $ file
      sides   = map toSides values
  print . sum . map calculatePaper  $ sides
  print . sum . map calculateRibbon $ values
