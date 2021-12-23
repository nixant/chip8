{-# LANGUAGE FlexibleInstances #-}

module Emulator.Chip8.Memory where

import Control.Concurrent.STM (TVar, atomically, modifyTVar', readTVarIO)
import Data.Default
import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import Data.Word (Word16, Word8)

data Memory =
  Memory
    { pc :: Word16
    , memory :: M.Map Word16 Word8
    }

fontSet :: [Word8]
fontSet =
  [ 0xF0
  , 0x90
  , 0x90
  , 0x90
  , 0xF0 -- 0
  , 0x20
  , 0x60
  , 0x20
  , 0x20
  , 0x70 -- 1
  , 0xF0
  , 0x10
  , 0xF0
  , 0x80
  , 0xF0 -- 2
  , 0xF0
  , 0x10
  , 0xF0
  , 0x10
  , 0xF0 -- 3
  , 0x90
  , 0x90
  , 0xF0
  , 0x10
  , 0x10 -- 4
  , 0xF0
  , 0x80
  , 0xF0
  , 0x10
  , 0xF0 -- 5
  , 0xF0
  , 0x80
  , 0xF0
  , 0x90
  , 0xF0 -- 6
  , 0xF0
  , 0x10
  , 0x20
  , 0x40
  , 0x40 -- 7
  , 0xF0
  , 0x90
  , 0xF0
  , 0x90
  , 0xF0 -- 8
  , 0xF0
  , 0x90
  , 0xF0
  , 0x10
  , 0xF0 -- 9
  , 0xF0
  , 0x90
  , 0xF0
  , 0x90
  , 0x90 -- A
  , 0xE0
  , 0x90
  , 0xE0
  , 0x90
  , 0xE0 -- B
  , 0xF0
  , 0x80
  , 0x80
  , 0x80
  , 0xF0 -- C
  , 0xE0
  , 0x90
  , 0x90
  , 0x90
  , 0xE0 -- D
  , 0xF0
  , 0x80
  , 0xF0
  , 0x80
  , 0xF0 -- E
  , 0xF0
  , 0x80
  , 0xF0
  , 0x80
  , 0x80 -- F
  ]

instance Default Memory where
  def = Memory 0x200 $ M.fromList $ zip [0 ..] fontSet

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
