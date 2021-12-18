{-# LANGUAGE ConstraintKinds #-}

module Emulator.Chip8.CPU where

import Control.Concurrent.MVar
import Control.Concurrent.STM.TVar
import Control.Monad.Reader
import Control.Monad.STM
import Data.Bits (Bits((.|.), shiftL), (.&.), shiftR, xor)
import Data.Default
import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import Data.Word (Word16, Word8)
import Emulator.Chip8.Instructions
import Emulator.Chip8.Memory
import Emulator.Chip8.Registers
import Emulator.Chip8.Stack
import Emulator.Chip8.Timers

type HasChip8 m = (HasStack m, HasTimers m, HasMemory m, HasRegisters m)

instance HasStack Chip8 where
  getStack = getStack . stack
  pop = pop . stack
  push v = push v . stack

instance HasTimers Chip8 where
  tick = tick . timers

instance HasMemory Chip8 where
  setPC = setPC . ram
  getPC = getPC . ram
  getMemAt = getMemAt . ram
  setMemAt = setMemAt . ram

instance HasRegisters Chip8 where
  getReg = getReg . reg
  setReg = setReg . reg

defChip8 = do
  ram' <- newTVarIO def -- Memory 0x200 $ M.fromList $ (\x -> (x,fromIntegral $ x `mod` 256)) <$> [0..4095] 
  reg' <- newTVarIO def --Registers $ M.fromList $ zip [V0 .. VF] (repeat 0)
  ir' <- newEmptyMVar
  stack' <- newTVarIO def
  timers' <- newTVarIO def
  return $ Chip8 ram' reg' ir' stack' timers' [] []

data Chip8 =
  Chip8
    { ram :: TVar Memory -- Add a newtype with size 4096
    , reg :: TVar Registers
    , ir :: MVar Word16
    , stack :: TVar Stack -- Add a newtype with fixed size 16
    , timers :: TVar Timers
    , scr :: [Bool] -- Add a newtype with dimensions 64*32
    , keys :: [Bool] -- Add a newtype with size 16
    }
