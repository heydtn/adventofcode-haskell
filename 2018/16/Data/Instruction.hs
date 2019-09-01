module Data.Instruction
    ( Instruction(..)
    , Opcode(..)
    , Directive(..)
    , runInstruction
    , ElfVM.Argument(..)
    , ElfVM.Destination(..)
    ) where

import Debug.Trace
import Data.Bits
import qualified Control.Monad.ElfVM as ElfVM

type Arg = ElfVM.Argument
type Dest = ElfVM.Destination
data Instruction =
    Operation Opcode Arg Arg Dest
    | Interrupt Directive Dest
    deriving (Show)

data Opcode =
    Noop
    | Add
    | Mul
    | Band
    | Bor
    | Set
    | Gt
    | Eq
    deriving (Show, Eq)

data Directive =
    DeclareBind
    deriving (Show, Eq)

-- Opcode/Interrupt bindings

-- This is for the monomorphism restriction
type ElfVMStateResult =
    ElfVM.Argument
    -> ElfVM.Argument
    -> ElfVM.Destination
    -> ElfVM.ElfVMState ()

toOp :: Opcode -> ElfVMStateResult
toOp Noop = noop
toOp Add = add
toOp Mul = mul
toOp Band = ban
toOp Bor = bor
toOp Set = set
toOp Gt = gt
toOp Eq = eq

toInt DeclareBind = declareBind

runInstruction (Operation opcode a b c) = toOp opcode a b c
runInstruction (Interrupt directive a) = toInt directive a

-- Op logic

boolToBit True = 1
boolToBit False = 0

zipAndStore f a b c = do
    v <- ElfVM.zipArgumentsBy f a b
    ElfVM.store c v

noop _ _ _ = return ()
add = zipAndStore (+)
mul = zipAndStore (*)
ban = zipAndStore (.&.)
bor = zipAndStore (.|.)
set = zipAndStore const
gt = zipAndStore $ (\x y -> boolToBit $! x > y)
eq = zipAndStore $ (\x y -> boolToBit $! x == y)

declareBind (ElfVM.Dest a) = ElfVM.bindI $ ElfVM.Register a
