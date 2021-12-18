{-# LANGUAGE FlexibleInstances #-}
module Emulator.Chip8.Stack where

import Data.Default
import Data.Word (Word16)
import Control.Concurrent.STM (TVar, readTVarIO, atomically, stateTVar, modifyTVar')

-- implement stack here

stackSize :: Word16
stackSize = 16

data Stack =
  Stack
    { stackPointer :: Word16
    , stackData :: [Word16]
    }
  deriving (Show)

instance Default Stack where
    def = Stack 0 []

popStack :: Stack -> (Word16, Stack)
popStack (Stack 0 _) = error "can't pop an empty stack"
popStack (Stack _ []) = error "can't pop an empty stack"
popStack st@(Stack p (d:ds)) = (d, st {stackPointer = p - 1, stackData = ds})

pushStack :: Word16 -> Stack -> Stack
pushStack v st@(Stack p d)
  | p >= stackSize - 1 = error "can't push into a full stack"
  | otherwise = st {stackPointer = p + 1, stackData = v:d}

class HasStack a where
  getStack :: a -> IO Stack
  pop :: a -> IO Word16
  push :: Word16 -> a -> IO ()

instance HasStack (TVar Stack) where
  getStack = readTVarIO -- return stack
  pop st = atomically (stateTVar st popStack)
  push v st = atomically (modifyTVar' st $ pushStack v)