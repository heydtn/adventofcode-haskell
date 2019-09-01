import Data.List (group, sort, any, deleteBy)
import Data.Either (rights)
import Text.Parsec (parse)
import Text.Parsec.String (Parser)
import Text.Parsec.Char (digit, char, string)
import Text.Parsec.Combinator (many1)

data Plot = Plot Int (Int, Int) (Int, Int) deriving (Show)

plotParser :: Parser Plot
plotParser = do
    char '#'
    plotNum <- many1 digit
    string " @ "
    startX <- many1 digit
    char ','
    startY <- many1 digit
    string ": "
    sizeX <- many1 digit
    char 'x'
    sizeY <- many1 digit
    -- We want matrix indices (thus +1) and displacements (-1)
    return $ Plot (read plotNum) (read startX + 1, read startY + 1) (read sizeX - 1, read sizeY - 1)

pointsFromPlot (Plot _ (x, y) (w, h)) =
    [ (i, j) | i <- [x..x+w],
               j <- [y..y+h]
    ]

withinPlot (Plot _ (x1, y1) (w, h)) (x,y) =
    x >= x1
    && y >= y1
    && x <= x1+w
    && y <= y1+h

main = do
    f <- readFile "day3.input"
    let plots = rights . map (parse plotParser "") . lines $ f
        coords = concat . map pointsFromPlot $ plots
        doublePlots = filter (\x -> length x > 1) . group . sort $ coords
        doublePlotCoords = map (!! 0) $ doublePlots

    putStrLn . show . length $ doublePlots
    putStrLn . show . find (\x -> not $ any (withinPlot x) doublePlotCoords) $ plots
