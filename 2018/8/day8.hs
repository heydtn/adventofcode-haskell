import Data.Tree
import Data.Either (fromRight)
import Text.Parsec (parse, try)
import Text.Parsec.String (Parser)
import Text.Parsec.Char (digit, space)
import Text.Parsec.Combinator (many1, count, eof)

type NodeCount = Int
type MetadataCount = Int
type Header = (NodeCount, MetadataCount)

type Metadata = Int

header :: Parser Header
header = do
    nodeCount <- read <$> many1 digit <* space
    metaCount <- read <$> many1 digit <* space
    return (nodeCount, metaCount)

metadata :: Parser Metadata
metadata = read <$> many1 digit <* try space

node :: Parser (Tree [Metadata])
node = do
    (nodes, entries) <- header
    ns <- count nodes node
    es <- count entries metadata
    return $ Node es ns

nodeSum :: Tree [Metadata] -> Int
nodeSum node
    | null $ subForest node = sum $ rootLabel node
    | otherwise             = sum $ map <$> valOf . subForest <*> rootLabel $ node
    where valOf xs i
              | i <= length xs = nodeSum $ xs !! (i - 1)
              | otherwise      = 0

main = do
    f <- readFile "day8.input"
    let treeData = fromRight (Node [] []) $ parse node "Tree" f

    putStrLn . show $ foldTree (\x xs -> sum $ sum x:xs) treeData
    putStrLn . show $ nodeSum treeData
