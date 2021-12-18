{-# LANGUAGE FlexibleInstances #-}
module Emulator.Chip8.Registers where

import qualified Data.Map as M
import Data.Word (Word8)
import Control.Concurrent.STM (TVar, readTVarIO, atomically, modifyTVar')
import Data.Maybe (fromMaybe, fromJust)
import Data.Default

data Register
  = V0
  | V1
  | V2
  | V3
  | V4
  | V5
  | V6
  | V7
  | V8
  | V9
  | VA
  | VB
  | VC
  | VD
  | VE
  | VF
  deriving (Enum, Eq, Ord, Show)

newtype Registers =
  Registers
    { registers :: M.Map Register Word8
    }

instance Default Registers where
    def = Registers $ M.fromList $ zip [V0 .. VF] (repeat 0)

class HasRegisters a where
  getReg :: a -> Register -> IO Word8
  setReg :: a -> Register -> Word8 -> IO ()
  modifyReg :: a -> Register -> (Word8 -> Word8) -> IO ()
  modifyReg c r f = getReg c r >>= setReg c r . f

instance HasRegisters (TVar Registers) where
  getReg rs r = fromMaybe 0 . M.lookup r . registers <$> readTVarIO rs
  setReg rs r v = atomically $ modifyTVar' rs (\(Registers regs) -> Registers $ M.insert r v regs)

mkRegister r | r `elem` [0 .. 0xF] = fromJust $ lookup r $ zip [0 ..] [V0 .. VF]
             | otherwise = error "invalid register"