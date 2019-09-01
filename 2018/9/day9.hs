
--import Control.Applicative
import Data.Maybe
import qualified Data.Dequeue as DQ
import qualified Data.Map.Strict as M
import Data.List (sortBy)
import Text.Parsec (parse)
import Text.Parsec.String (Parser)
import Text.Parsec.Char
import Text.Parsec.Combinator

type ElvesAndMarbles = (M.Map Int Int, DQ.BankersDequeue Int)

input :: Parser (Int, Int)
input = do
    players <- many1 digit
    skipMany1 $ noneOf ['0'..'9']
    points <- many1 digit
    skipMany1 $ noneOf ['0'..'9']
    return (read players, read points)

eval (Right a) = a
eval (Left a) = error "boop"

rotateRight :: DQ.Dequeue q => Int -> q a -> q a
rotateRight n =
    (!! n) . iterate rot1
    where rot1 q =
              case DQ.popBack q of
                  Nothing    -> q
                  Just (a,b) -> DQ.pushFront b a

rotateLeft :: DQ.Dequeue q => Int -> q a -> q a
rotateLeft n =
    (!! n) . iterate rot1
    where rot1 q =
              case DQ.popFront q of
                  Nothing    -> q
                  Just (a,b) -> DQ.pushBack b a

insertValueNormal val q =
    flip DQ.pushBack val . rotateLeft 1 $ q

performSpecial q =
    case movement of
        Nothing ->
            (0,q)
        Just (a,b) ->
            (a, rotateLeft 1 b)
    where movement = DQ.popBack . rotateRight 7 $ q

processData :: Int -> ElvesAndMarbles -> Int -> ElvesAndMarbles
processData elfCount (m, dq) val =
    if val > 0 && val `mod` 23 == 0 then
        (\(v,dq) -> (M.insertWith (+) currentElf (v + val) m, dq)) $ performSpecial dq
    else
        (m, insertValueNormal val dq)
    where currentElf = val `mod` elfCount

main = do
    f <- readFile "day9.input"
    let (elfCount, points) = eval $ parse input "" f

    let getAnswerFor maxPoint =
            putStrLn
                . show
                . snd
                . last
                . sortBy (\(_,v) (_,v') -> compare v v')
                . M.toList
                . fst
                $ foldl
                    (processData elfCount)
                    (M.empty, DQ.empty)
                    [0..maxPoint]

    getAnswerFor points
    getAnswerFor $ points * 100
