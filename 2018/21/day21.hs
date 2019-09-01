import Data.Char
import Data.List
import Control.Applicative
import Data.Either
import Text.Parsec (parse)
import Text.Parsec.String (Parser)
import Text.Parsec.Char
import Text.Parsec.Combinator

main = do
    f <- readFile "day21.input"
    putStrLn f
