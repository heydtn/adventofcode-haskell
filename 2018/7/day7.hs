import Data.Graph
import Data.Tree
import Data.Either
import Data.List
import Text.Parsec (parse)
import Text.Parsec.String (Parser)
import Text.Parsec.Char
import Text.Parsec.Combinator

edge :: Parser (Char, Char)
edge = (,)
    <$  string "Step "
    <*> letter
    <*  string " must be finished before step "
    <*> letter

draw :: Show a => Tree a -> [String]
draw (Node x ts0) = lines (show x) ++ drawSubTrees ts0
  where
    drawSubTrees [] = []
    drawSubTrees [t] =
        "|" : shift "`- " "   " (draw t)
    drawSubTrees (t:ts) =
        "|" : shift "+- " "|  " (draw t) ++ drawSubTrees ts

    shift first other = zipWith (++) (first : repeat other)

dTree :: Show a => Tree a -> String
dTree  = unlines . draw

dForest :: Show a => Forest a -> String
dForest  = unlines . map dTree

main = do
    f <- readFile "day7.input"
    let pairs = rights . map (parse edge "edge") . lines $ f
        edges =
            reverse
            . map (\(x,y) -> (x,x,y))
            . map (\x -> (fst $ x!!0, sort $ foldl (\h (_,v) -> v : h) [] x))
            . groupBy (\(x,_) (y,_) -> x == y)
            . sortBy (\(x,_) (y,_) -> compare x y)
            $ pairs
        (graphNodes, vLookup) = graphFromEdges' edges
        childEdges = nub . foldl (\a (b,c,d) -> d ++ a) "" $ edges
        inaccessibleEdges = (\\ childEdges) . foldl (\a (b,c,d) -> b : a) "" $ edges

    putStrLn . show $ inaccessibleEdges

    putStrLn . show . map (\x -> (x, vLookup x)) $ topSort graphNodes

    putStrLn . dForest $ dfs graphNodes [1,7,16,18]
