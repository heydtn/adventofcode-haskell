import Control.Applicative
import qualified Data.Set as S

data Op = Add | Subtract deriving (Show)

toOp '-' = Subtract
toOp '+' = Add

runOp a (b, c) =
  case b of
    Add -> a + c
    Subtract -> a - c

findRepeat :: S.Set Int -> [Int] -> Maybe Int
findRepeat set [] = Nothing
findRepeat set (x:xs) =
  if S.member x set then
    Just x
  else
    findRepeat (S.insert x set) xs

main = do
  f <- readFile "day1.input"
  let results = scanl runOp 0 . cycle $ toOpPair f

  putStrLn . show $ results !! (length $ toOpPair f)
  putStrLn . show $ findRepeat S.empty $ results

  where split :: String -> (Op, Int)
        split = (,) <$> toOp . (!! 0) <*> read . (drop 1)

        toOpPair = map split . lines
