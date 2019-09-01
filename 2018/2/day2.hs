import Data.List (group, sort, find, intersect, (\\))
import Text.EditDistance

toWeight xs =
  (toNum $ elem 2 xs, toNum $ elem 3 xs)
  where toNum True = 1
        toNum False = 0

sequences a [] = a
sequences a (x:xs) =
  sequences (a ++ subseqs x xs) xs
  where subseqs y ys = map (\ys -> (y, ys)) ys

main = do
  f <- readFile "day2.input"
  putStrLn . show . (\(a, b) -> a * b) . outputsFor $ f
  putStrLn . show . fmap (\(x, y) -> intersect x y) . find isDistance . sequences [] . lines $ f

  where weights = toWeight . map length . group . sort
        addWeights (a, b) (c, d) = (a + c, b + d)
        outputsFor f = foldl addWeights (0,0) . map weights . lines $ f

        isDistance (x, y) = levenshteinDistance defaultEditCosts x y == 1
