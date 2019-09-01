import Data.Array
import Data.Ix (inRange)

import Control.Applicative
import Data.Either
import Text.Parsec (parse)

import Text.ParseOperation
import Control.Monad.ElfVM (runElfVMState, ElfVM, nextI, getI, getState)
import Data.Instruction
    ( Instruction(..)
    , runInstruction
    , Opcode(Set)
    , Argument(..)
    , Destination(..)
    )

import Debug.Trace

mainLoop ip ops =
    if not $ inRange (bounds ops) ip then
        return ()
    else do
        cip <- getI
        return $! trace ("ip=" <> show cip <> " running " <> show (ops ! ip)) ()
        runInstruction $! (ops ! ip)
        newState <- getState
        return $! trace ("after instruction state=" <> show newState) ()


        nip <- nextI
        (mainLoop $! fromIntegral nip) $! ops

execVM bind ops = do
    runInstruction bind
    runInstruction (Operation Set (Immediate 1) (Reference 0) (Dest 0))
    mainLoop 0 ops

main = do
    f <- readFile "day19.input"
    let instrs = rights . map (parse instruction "i") $ lines f
        bind = head instrs
        operations = tail instrs
        opArrLength = length operations - 1
        opArr = array (0, opArrLength) $ zipWith (,) [0..] operations

    sequence . map (putStrLn . show) $ instrs

    putStrLn . show $ runElfVMState (execVM bind opArr)
