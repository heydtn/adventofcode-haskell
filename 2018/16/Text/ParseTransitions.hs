module Text.ParseTransitions
    ( RegisterList
    , Transition(..)
    , NumOp
    , parseFile
    )
    where

import Control.Applicative ((<|>))
import Data.Maybe (fromMaybe)
import Text.Parsec (parse, try, skipMany)
import Text.Parsec.String (Parser)
import Text.Parsec.Char
import Text.Parsec.Combinator

import Data.Instruction

type RegisterList = [Int]
data Transition = Transition
    { _tBefore :: RegisterList
    , _tOp :: NumOp
    , _tAfter :: RegisterList
    }
    deriving (Show, Eq)

type NumOp = [Int]

registerList :: Parser RegisterList
registerList =
    read <$> manyTill anyChar (try $ lookAhead newline)

transition :: Parser Transition
transition = do
    string "Before: "
    b <- registerList
    newline
    op <- numOp
    newline
    string "After: "
    a <- registerList
    many1 newline
    return $ Transition {_tBefore = b, _tOp = op, _tAfter = a}

transitions :: Parser [Transition]
transitions = manyTill transition (notFollowedBy transition)

numOp :: Parser NumOp
numOp = map read <$> sepBy1 (many1 digit) (many1 $ char ' ')

numOps :: Parser [NumOp]
numOps = manyTill (numOp <* newline) (notFollowedBy numOp)

parseFile = do
    ts <- transitions
    manyTill newline (try . lookAhead $ notFollowedBy newline)
    ops <- numOps
    return $ (ts, ops)
