import Control.Applicative
import Data.Either
import Text.Parsec (parse)

import Text.ParseTransitions
import Control.Monad.ElfVM
import Data.Instruction
    ( Instruction(..)
    , runInstruction
    , Opcode(..)
    , Argument(..)
    , Destination(..)
    )

op1 = [Add, Mul, Band, Bor, Set]
op2 = [Gt, Eq]

toOp1 op a b dest =
    [ Operation op (Reference a) (a2 b) (Dest dest)
        | a2 <- [Immediate, Reference]
    ]

toOp2 op a b dest = tail $
    [ Operation op (a1 a) (a2 b) (Dest dest)
        | a1 <- [Immediate, Reference],
          a2 <- [Immediate, Reference]
    ]

operations a b dest =
    concat . map (\x -> x a b dest) $ map toOp1 op1 <> map toOp2 op2

setupRegisters xs = zipWith set xs [0..]
    where set a c = (Operation Set (Immediate a) (Reference 0) (Dest c))

--CONTINUE HERE
instructionsWith xs a b dest = map (\x -> setupRegisters xs <> [x]) $ operations a b dest

transitionToRuntimes (Transition {_tBefore = b, _tOp = ops}) =
    instructionsWith b (ops !! 1) (ops !! 2) (ops !! 3)

execVM ops = do
    sequence $ map runInstruction ops
    sequence $ map (getRegister . Reference) ([0..3] :: [Int])

main = do
    f <- readFile "day16.input"
    let (Right (transitions, ops)) = parse parseFile "file" f
        runtimes = map (\x -> (x, transitionToRuntimes x)) transitions
        execRuntimes = map (fst . runElfVMState . execVM)
        finishedRuntimes = map (\(x,ys) -> (x, execRuntimes ys)) runtimes
        sameOpCounts = map (\(x,y) -> (x, length $ filter (== _tAfter x) y)) finishedRuntimes
        threeOrMore = filter (\(x,y) -> y >= 3) sameOpCounts

    putStrLn . show $ length threeOrMore
    return ()

    --putStrLn . show $ runElfVMState (execVM arr)
