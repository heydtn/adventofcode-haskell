import Data.List
import Data.Maybe
import Data.Either (rights)
import Text.Parsec (parse)
import Text.Parsec.String (Parser)
import Text.Parsec.Char (digit, string)
import Text.Parsec.Combinator (many1)

data Coord = Coord Integer Integer deriving (Show)
type Delta = (Integer, Integer)
data Bounds = Bounds Delta Delta deriving (Show)
data Distance = Distance Integer Coord deriving (Show)

instance Eq Coord where
    Coord x y == Coord x' y' = x == x' && y == y'

instance Ord Coord where
    compare (Coord x y) (Coord x' y') =
        case compare x x' of
            EQ ->
              compare y y'
            a ->
              a

instance Eq Distance where
    (Distance x _) == (Distance y _) = x == y

instance Ord Distance where
    compare (Distance x _) (Distance y _) = compare x y

getX :: Coord -> Integer
getX (Coord x _) = x
getY :: Coord -> Integer
getY (Coord _ y) = y

manhattan :: Coord -> Coord -> Integer
manhattan (Coord x y) (Coord x' y') = abs (x - x') + abs (y - y')

coord :: Parser Coord
coord = do
    x <- many1 digit
    string ", "
    y <- many1 digit
    return $ Coord (read x) (read y)

shortestTo :: [Coord] -> Coord -> Maybe Distance
shortestTo cs c =
    case distances cs of
        (x:y:_) ->
            if x == y then
                Nothing
            else
                Just x
        [a] ->
          Just a
        [] ->
          Nothing
    where distances = sort . (zipWith Distance <$> map (manhattan c) <*> id)

sameNode :: Distance -> Distance -> Bool
sameNode (Distance _ x) (Distance _ y) = x == y

coordFromDistance :: Distance -> Coord
coordFromDistance (Distance _ x) = x

distance :: Distance -> Integer
distance (Distance x _) = x

determineLimits :: [Coord] -> Bounds
determineLimits cs =
    Bounds
        (getX . head $ bx, getX . last $ bx)
        (getY . head $ by, getY . last $ by)
    where bx = sortBy (\(Coord x _) (Coord x' _) -> compare x x') $ cs
          by = sortBy (\(Coord _ y) (Coord _ y') -> compare y y') $ cs

dX :: Bounds -> Delta
dX (Bounds x _) = x

dY :: Bounds -> Delta
dY (Bounds _ y) = y

outerNodes :: Bounds -> [Coord]
outerNodes (Bounds (x,x') (y,y')) =
    [Coord u v |
        u <- [x..x'],
        v <- [y..y'],
        not (u > x && v > y && u < x' && v < y') ]

innerNodes :: Bounds -> [Coord]
innerNodes (Bounds (x,x') (y,y')) =
    [Coord u v |
        u <- [x..x'],
        v <- [y..y'],
        u > x && v > y && u < x' && v < y' ]

main = do
    f <- readFile "day6.input"
    let coords = rights . map (parse coord "c") . lines $ f
        limits = determineLimits coords

    let disqualified =
            map coordFromDistance
            . nubBy sameNode
            . catMaybes
            . map (shortestTo coords)
            $ outerNodes limits
        resolvedInner =
            map coordFromDistance
            . catMaybes
            . map (shortestTo coords)
            $ innerNodes limits
        candidateList =
            filter (not . flip elem disqualified) resolvedInner
        groupedCandidates =
            group . sort $ candidateList

    putStrLn . show . last . sort . map length $ groupedCandidates

    putStrLn . show . length $
        [ Coord u v |
            u <- [1..(snd . dX $ limits)+100],
            v <- [1..(snd . dY $ limits)+100],
            (sum . map (manhattan $ Coord u v) $ coords) < 10000 ]
