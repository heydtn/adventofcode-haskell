{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Control.Monad.ElfVM
    ( ElfVMState(..)
    , ElfVM(..)
    , Argument(..)
    , Destination(..)
    , InstructionPointer(..)
    , RegisterIdentifier
    , DataValue
    , runElfVMState
    )
    where

import Control.Monad.State.Strict
import qualified Data.Map.Strict as Map

newtype ElfVMState a = ElfVMState (State VMState a)
    deriving (Monad, Applicative, Functor)

type RegisterIdentifier = Int
type DataValue = Int
type Registers = Map.Map RegisterIdentifier DataValue

data Argument =
    Reference RegisterIdentifier
    | Immediate DataValue
    deriving (Show, Eq)

data Destination =
    Dest RegisterIdentifier
    deriving (Show, Eq)

data InstructionPointer =
    FreePointer DataValue
    | Register RegisterIdentifier
    deriving (Show, Eq)

data VMState = VMState
    { _vmRegisters :: Registers
    , _vmInstructionPointer :: InstructionPointer
    }
    deriving (Show, Eq)

initVM :: VMState
initVM = VMState Map.empty (FreePointer 0)

initData :: DataValue
initData = 0

class Monad m => ElfVM m where
    store :: Destination -> DataValue -> m ()
    getRegister :: Argument -> m DataValue
    zipArgumentsBy ::
        (DataValue -> DataValue -> DataValue)
        -> Argument
        -> Argument
        -> m DataValue
    bindI :: InstructionPointer -> m ()
    getI :: m DataValue
    nextI :: m DataValue
    getState :: m VMState

instance ElfVM ElfVMState where
    store (Dest d) v = ElfVMState $ do
        vmstate@(VMState {_vmRegisters = rs}) <- get
        put $! vmstate {_vmRegisters = registerInsert d v rs}

    getRegister (Reference r) = ElfVMState $ do
        VMState {_vmRegisters = rs} <- get
        return $ registerLookup r rs
    getRegister _ = error "Invalid register specification"

    zipArgumentsBy f r1 r2 = ElfVMState $ do
        VMState {_vmRegisters = rs} <- get
        let resolve d =
                case d of
                    Reference r ->
                        registerLookup r rs
                    Immediate v ->
                        v
            r1V = resolve r1
            r2V = resolve r2
        return $! f r1V r2V

    bindI np = ElfVMState $ do
        modify (\vs -> vs {_vmInstructionPointer = np})

    getI = ElfVMState $ do
        VMState {_vmRegisters = rs, _vmInstructionPointer = ip} <- get
        case ip of
            FreePointer i ->
                return $! i
            Register r ->
                return $! registerLookup r rs

    nextI = ElfVMState $ do
        vmstate@(VMState {_vmRegisters = rs, _vmInstructionPointer = ip}) <- get
        case ip of
            FreePointer i -> do
                let next = i + 1
                put $! vmstate { _vmInstructionPointer = FreePointer $ next }
                return $! next
            Register r -> do
                let val = registerLookup r rs + 1
                put $! vmstate { _vmRegisters = registerInsert r val rs }
                return $! val

    getState = ElfVMState $ get

registerLookup r rs = Map.findWithDefault initData r rs
registerInsert r val rs = Map.insert r val rs

runElfVMState (ElfVMState m) = runState m initVM
