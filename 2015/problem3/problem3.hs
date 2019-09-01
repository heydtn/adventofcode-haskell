import qualified Data.Map as M

type HouseMap    = M.Map (Int, Int) Bool
type Position    = (Int, Int)
type Accumulator = (HouseMap, Position)

movPos :: Position -> Char -> Position
movPos (x, y) c
  | c == '>'  = (x+1, y)
  | c == '<'  = (x-1, y)
  | c == '^'  = (x, y+1)
  | c == 'v'  = (x, y-1)
  | otherwise = (x, y)

emptyMap :: HouseMap
emptyMap = M.fromList [((0,0), True)]

emptyPos :: Position
emptyPos = (0,0)

emptyAccum :: Accumulator
emptyAccum = (emptyMap, emptyPos)

insertAndMove  :: Accumulator -> Char -> Accumulator
insertAndMove (m, p) c = (\x -> (M.insert x True m, x)) $ movPos p c

splitList :: [a] -> ([a], [a])
splitList []       = ([], [])
splitList [x]      = ([x], [])
splitList (x:y:zs) = (x:xs, y:ys)
  where (xs, ys)   = splitList zs

main :: IO ()
main = do
  file <- readFile "problem3.input"
  let eval = processMoves file
      evS  = processMoves . fst . splitList $ file
      evR  = processMoves . snd . splitList $ file
  print . M.size $ eval
  print . M.size $ M.union evS evR
  where processMoves = fst . foldl insertAndMove emptyAccum
