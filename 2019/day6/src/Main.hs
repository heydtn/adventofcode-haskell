import Data.Graph.Inductive as Graph

main = do
    file <- readFile "src/day6.input"
    let parseOrbit str = mapSnd tail $ break (==')') str
        edges = map parseOrbit $ lines file
    
        graph :: Graph.Gr String Int
        (_, (nodemap, graph)) = Graph.run Graph.empty $ do
            Graph.insMapNodesM $ foldl (\acc (a,b) -> a:b:acc) [] edges
            Graph.insMapEdgesM $ foldl (\acc (a,b) -> (a,b,1):acc) [] edges

        (santa, _) = Graph.mkNode_ nodemap "SAN"
        (you, _) = Graph.mkNode_ nodemap "YOU"
    
    let orbits =
            sum
            . map (length . tail)
            . map (\node -> Graph.rdfs [node] graph)
            $ Graph.nodes graph

        toSanta = subtract 2 <$> Graph.spLength santa you (Graph.undir graph)

    putStrLn $ show orbits
    putStrLn $ show toSanta
