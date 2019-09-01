import Data.Char
import Data.List
import Control.Concurrent.Async

deleteElems :: String -> String
deleteElems [] = []
deleteElems (x:y:zs) =
    if x /= y && toLower x == toLower y then
        deleteElems zs
    else
        x : deleteElems (y:zs)
deleteElems a = a

deleteElems2 :: String -> String
deleteElems2 = foldl' cmp ""
    where cmp [] c = [c]
          cmp (x:xs) c =
              if c /= x && toLower c == toLower x then
                  xs
              else
                  c : (x : xs)

untilStable f x = until (\x -> f x == x) f x

deletePolymer polymers polymer = filter (\x -> x /= toLower polymer && x /= toUpper polymer) polymers

main = do
    f <- readFile "day5.input"
    let shortened = deleteElems2 . filter (\c -> c /= '\n') $ f
    putStrLn . show  . length $ shortened
    polymers <- mapConcurrently (\x -> return . (,) x . length . deleteElems2 . deletePolymer shortened $ x) ['a'..'z']
    putStrLn . show . head . sortBy (\(_,a) (_,b) -> compare a b) $ polymers
