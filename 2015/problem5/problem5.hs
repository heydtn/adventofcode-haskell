{- Haskell native regex is slow and bad, there are much better ways to do this
but... -}

import Data.List (isInfixOf)

hasRepeats [] = False
hasRepeats [x] = False
hasRepeats z@(x:y:xs) = if x == y then True else hasRepeats (tail z)

countVowels [] c = c
countVowels (x:xs) c = if elem x "aeiou" then
                         countVowels xs $ c+1
                       else
                         countVowels xs c
hasThreeVowels xs = 3 <= countVowels xs 0

containsNaughtySeq xs = or . map (\y -> isInfixOf y xs) $ ["ab","cd","pq","xy"]

containsRepeatWMiddle [] = False
containsRepeatWMiddle w@(x:y:z:xs) = if x == z then True else containsRepeatWMiddle (tail w)
containsRepeatWMiddle (x:y) = False

containsDuplicateSequence [] = False
containsDuplicateSequence w@(x:y:xs) = if isInfixOf (x:y:[]) xs then True else containsDuplicateSequence (tail w)
containsDuplicateSequence (x:xs) = False

main :: IO ()
main = do
  file <- readFile "problem5.input"
  print . getNiceNum nice1 $ file
  print . getNiceNum nice2 $ file
  where getNiceNum r f = length . filter (==True) . map r . lines $ f
        nice1 xs = hasRepeats xs && hasThreeVowels xs && not (containsNaughtySeq xs)
        nice2 xs = containsRepeatWMiddle xs && containsDuplicateSequence xs
