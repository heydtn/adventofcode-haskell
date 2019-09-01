module Text.ParseOperation
    ( instruction
    )
    where

import Control.Applicative ((<|>))
import Data.Maybe (fromMaybe)
import Text.Parsec (parse)
import Text.Parsec.String (Parser)
import Text.Parsec.Char
import Text.Parsec.Combinator

import Data.Instruction

opcode :: Parser Opcode
opcode =
    Noop <$ string "noop"
    <|> Add <$ string "add"
    <|> Mul <$ string "mul"
    <|> Band <$ string "ban"
    <|> Bor <$ string "bor"
    <|> Set <$ string "set"
    <|> Gt <$ string "gt"
    <|> Eq <$ string "eq"

directive :: Parser Directive
directive =
    DeclareBind <$ string "ip"

argumentType =
    Reference <$ string "r"
    <|> Immediate <$ string "i"

destination :: Parser Destination
destination =
    Dest . read <$> many1 digit

argument :: Read a => Parser a
argument = read <$> many1 digit

-- Necessary because op ordering is wonky and inconsistant
sortArguments Set a1 a2 =
    (fromMaybe Reference a1, Immediate)
sortArguments _ a1 a2 =
    case a2 of
        Nothing ->
            (Reference, fromMaybe Reference a1)
        Just a ->
            (fromMaybe Reference a1, a)

operation = do
    code <- opcode
    at1 <- optionMaybe argumentType
    at2 <- optionMaybe argumentType
    (type1, type2) <- return $ sortArguments code at1 at2
    spaces
    a1 <- type1 <$> argument
    spaces
    a2 <- type2 <$> argument
    spaces
    dest <- destination
    return $ Operation code a1 a2 dest

interrupt = do
    string "#"
    dir <- directive
    spaces
    dest <- argument
    return . Interrupt dir $ Dest dest

instruction = operation <|> interrupt
