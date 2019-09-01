main :: IO ()
main = do
  file <- readFile "problem1.input"
  let values = scanl (+) 0 $ map toNum file
  print $ last values
  print . length . takeWhile (>= 0) $ values
  where toNum x
            | x == '('  = 1
            | x == ')'  = -1
            | otherwise = 0
