import Data.Char
import Data.List
import Data.Either
import Data.Maybe
import Text.Parsec (parse, try, (<|>))
import Text.Parsec.String (Parser)
import Text.Parsec.Char
import Text.Parsec.Combinator

type Position = (Int, Int)
type Velocity = (Int, Int)
data Point = Point Position Velocity deriving (Show)

data Command =
    Exit
    | Goto Int
    | Forward Int
    | Backward Int

integer :: Parser Int
integer = read <$> (neg <|> pos)
    where neg = (:) <$> char '-' <*> number
          pos = number
          number = many1 digit

intPair :: Parser (Int, Int)
intPair = (,)
    <$  char '<'
    <*> (spaces *> integer)
    <*  string ", "
    <*> (spaces *> integer)
    <*  char '>'

point :: Parser Point
point = Point
    <$  string "position="
    <*> intPair
    <*  string " velocity="
    <*> intPair

transform :: Point -> Point
transform (Point (x, y) d@(dx, dy)) =
    Point (x + dx, y + dy) d

longCommand :: Parser Command
longCommand = do
    command <- oneOf ['g','f','b', 'e']
    digits <- fromMaybe "1" <$> (optionMaybe $ many1 digit)
    return $ case command of
                 'e' ->
                     Exit
                 a ->
                     toOp a $ read digits
    where toOp 'g' = Goto
          toOp 'f' = Forward
          toOp 'b' = Backward

toCoord (Point a _) = a

manhattan (Point (x,y) _) (Point (x',y') _) = abs (x - x') + abs (y - y')

pointsWithin distance p =
    and $ map (close $ p !! 0) p
    where close x y = manhattan x y < distance

renderPoints p =
    unlines $ map (\y -> map (\x -> points x y) [80..180]) $ [100..120]
    where coords = map toCoord p
          points u v = if elem (u,v) coords then '#' else '.'

renderLoop renderer i = do
    putStrLn $ "ITERATION " <> show i
    putStrLn $ renderer i
    putStr "\nInput command (g#,f#,b#,e): "
    input <- getLine
    processCommand . fromRight (Forward 1) $ parse longCommand "in" input
    where processCommand Exit = return ()
          processCommand (Goto x) = renderLoop renderer x
          processCommand (Forward x) = renderLoop renderer $ i + x
          processCommand (Backward x) = renderLoop renderer . max 0 $ i - x

main = do
    f <- readFile "day10.input"
    let points = rights . map (parse point "pt") . lines $ f
        iteration n = (!! n) . iterate (map transform) $ points
        closeIteration = find (\(x,p) -> pointsWithin 100 p) . zip [0..] . iterate (map transform) $ points

    renderLoop (renderPoints . iteration) . fst $ fromMaybe (0,[]) closeIteration
