import Control.Applicative

import Data.List
import Data.List.Split
import Data.Either (rights)
import qualified Data.Map as Map

import Text.Parsec (parse)
import Text.Parsec.String (Parser)
import Text.Parsec.Char (digit, char, string, spaces)
import Text.Parsec.Combinator (many1)

data Operation =
    BeginShift Int
    | WakeUp
    | FallAsleep
    deriving (Show, Eq)

data Event = Event Timestamp Operation deriving (Show, Eq)

type Year = Int
type Month = Int
type Day = Int
type Hour = Int
type Minute = Int

type Date = (Year, Month, Day)
type Time = (Hour, Minute)
data Timestamp = Timestamp Date Time deriving (Show, Eq, Ord)

instance Ord Event where
    compare (Event x _) (Event y _) = compare x y

timestamp :: Parser Timestamp
timestamp = do
    year <- many1 digit
    char '-'
    month <- many1 digit
    char '-'
    day <- many1 digit
    char ' '
    hours <- many1 digit
    char ':'
    minutes <- many1 digit
    return $ Timestamp (read year, read month, read day) (read hours, read minutes)

operation :: Parser Operation
operation =
    do
        string "Guard #"
        guard <- many1 digit
        return $ BeginShift (read guard)
    <|> (WakeUp <$ string "wakes up")
    <|> (FallAsleep <$ string "falls asleep")

event :: Parser Event
event = do
    char '['
    time <- timestamp
    char ']'
    spaces
    op <- operation
    return $ Event time op

isBeginShift (Event _ (BeginShift _)) = True
isBeginShift _ = False

toElfNum (Event _ (BeginShift a)) = a
toElfNum _ = 0

toTimestamp (Event t _) = t

durationMin (Timestamp _ (h,m)) (Timestamp _ (h',m')) = (h' - h)*60 + (m' - m)

minutesAsleep :: [Event] -> (Int, Float)
minutesAsleep shift =
    (elfNum, fromIntegral sleepTime)
    where elfNum = toElfNum $ head shift
          sleepEvents = tail shift
          sleepTime = sum . map (\[x,y] -> durationMin x y) . chunksOf 2 . map toTimestamp $ sleepEvents

toSleepMinutes (Timestamp _ (_, a)) (Timestamp _ (_, b)) = [a..b-1]

main = do
    f <- readFile "day4.input"
    let shifts = sort . rights . map (parse event "op") . lines $ f
        groupedShifts = groupBy (\x y -> not . isBeginShift $ y) shifts
        averageMins xs =
            (\(elf, mins) ->
                (elf, mins / fromIntegral (length xs)))
            . foldl (\(ae, am) (e, m) -> (e, am + m)) (0, 0)
            $ xs
        totalMins xs =
            foldl (\(ae, am) (e, m) -> (e, am + m)) (0, 0)
            $ xs
        elfSleepMins =
            sortBy (\(_, b) (_, b') -> compare b b')
            . map totalMins
            . groupBy (\(a,_) (b,_) -> a == b)
            . sort
            $ map minutesAsleep groupedShifts
        sleepiestElf = fst $ last elfSleepMins
        sleepiestElfShifts = filter ((==sleepiestElf) . toElfNum . head) $ groupedShifts
        sleepEvents = map (filter (not . isBeginShift)) sleepiestElfShifts
        minutes = Map.fromList $ zip [0..59] (repeat 0)
        sleepMinutes = concat $ map (toSleepMinutes <$> toTimestamp . head <*> toTimestamp . last) sleepEvents
        sleepiestMinute =
            last
            . sortBy (\(_,a) (_,b) -> compare a b)
            . Map.toList
            $ foldr (Map.adjust (+1)) minutes sleepMinutes

    putStrLn . show $ sleepiestElf
    putStrLn . show $ sleepiestMinute
    putStrLn . show $ sleepiestElf * fst sleepiestMinute
    --sequence $ map sequence $ map (map (putStrLn . show)) sleepiestElfShifts


    return ()
