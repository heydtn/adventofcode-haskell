import Data.List
import Data.Either
import Text.Parsec (parse)
import Text.Parsec.String (Parser)
import Text.Parsec.Char
import Text.Parsec.Combinator

parseRange :: Parser [Int]
parseRange = do
    start <- many1 digit
    char '-'
    end <- many1 digit
    return [read start..read end]

main = do
    f <- readFile "src/day4.input"
    let Right parsed = parse parseRange "in" f

    let notDecreasing n = sort (show n) == show n

        hasDouble n = any (\x -> length x > 1) . group $ show n
        hasDouble' n = any (\x -> length x == 2) . group $ show n

        validNumbers = filter (\x -> notDecreasing x && hasDouble x) parsed
        validNumbers' = filter (\x -> notDecreasing x && hasDouble' x) parsed

    return $ (length validNumbers, length validNumbers')
