{-# LANGUAGE FlexibleInstances #-}

module Emulator.Chip8.Memory where

import Control.Concurrent.STM (TVar, atomically, modifyTVar', readTVarIO)
import Data.Default
import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import Data.Word (Word16, Word8)

-- implement memory here
data Memory =
  Memory
    { pc :: Word16
    , memory :: M.Map Word16 Word8
    }

instance Default Memory where
  def = Memory 0x200 M.empty -- TODO: Add sprites and other read only data at start of memory

class HasMemory a where
  setPC :: a -> Word16 -> IO ()
  getPC :: a -> IO Word16
  modifyPC :: a -> (Word16 -> Word16) -> IO ()
  modifyPC mem f = getPC mem >>= (setPC mem . f)
  incPC :: a -> IO ()
  incPC mem = modifyPC mem (+ 1)
  decPC :: a -> IO ()
  decPC mem = modifyPC mem (\x -> x - 1)
  setMemAt :: a -> Word16 -> Word8 -> IO ()
  setMemory :: a -> Word8 -> IO ()
  setMemory mem v = getPC mem >>= \c -> setMemAt mem c v
  getMemAt :: a -> Word16 -> IO Word8
  getMemory :: a -> IO Word8
  getMemory mem = getPC mem >>= getMemAt mem

instance HasMemory (TVar Memory) where
  setPC tv mv = atomically $ modifyTVar' tv (\m -> m {pc = mv})
  getPC tv = pc <$> readTVarIO tv
  getMemAt tv addr = fromMaybe 0 . M.lookup addr . memory <$> readTVarIO tv
  setMemAt tv addr v =
    atomically $
    modifyTVar' tv (\(Memory pc m) -> Memory pc $ M.insert addr v m)
