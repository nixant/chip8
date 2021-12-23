{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE NamedFieldPuns #-}

module Emulator.Chip8.CPU where

import Control.Concurrent
import Control.Concurrent.Chan
import Control.Concurrent.MVar
import Control.Concurrent.STM.TVar
import Control.Monad.Reader
import Control.Monad.STM
import Data.Bits (Bits((.|.), shiftL), (.&.), shiftR, xor)
import qualified Data.ByteString as BS
import Data.Default
import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import Data.Word (Word16, Word8)
import Emulator.Chip8.Display
import Emulator.Chip8.IO
import Emulator.Chip8.Instructions
import Emulator.Chip8.Keyboard
import Emulator.Chip8.Memory
import Emulator.Chip8.Registers
import Emulator.Chip8.Stack
import Emulator.Chip8.Timers
import System.IO

type HasChip8 m
   = (HasStack m, HasTimers m, HasMemory m, HasRegisters m, HasIR m, HasIO m)

instance HasStack Chip8 where
  getStack = getStack . stack
  pop = pop . stack
  push v = push v . stack

instance HasTimers Chip8 where
  tick = tick . timers
  getDT = getDT . timers
  setDT = setDT . timers
  getST = getST . timers
  setST = setST . timers

instance HasMemory Chip8 where
  setPC = setPC . ram
  getPC = getPC . ram
  getMemAt = getMemAt . ram
  setMemAt = setMemAt . ram

instance HasRegisters Chip8 where
  getReg = getReg . reg
  setReg = setReg . reg

instance HasIR Chip8 where
  getIR = getIR . ir
  setIR = setIR . ir

instance HasDisplay Chip8 where
  getDisplay Chip8 {scr} = getDisplay scr
  writeBuffer Chip8 {scr} = writeBuffer scr
  writeManyBuffer Chip8 {scr} = writeManyBuffer scr
  getDisplayBuffer Chip8 {scr} = getDisplayBuffer scr
  getDisplayState Chip8 {scr} = getDisplayState scr
  setDisplayState Chip8 {scr} = setDisplayState scr
  updateDisplayState Chip8 {scr} = updateDisplayState scr

instance HasKeyboard Chip8 where
  press = press . keyboard
  release = release . keyboard
  keyStatus = keyStatus . keyboard
  readPressed = readPressed . keyboard
  getKeyboardState = getKeyboardState . keyboard

getROM :: FilePath -> IO (M.Map Word16 Word8)
getROM rom = do
    withFile ("roms/" <> rom) ReadMode $ \handle -> do
        contents <- BS.unpack <$> BS.hGetContents handle
        return $ M.fromList $ zip [0x200 ..] contents

defChip8 = do
  putStrLn "Enter ROM Name: "
  x <- getLine
  rom <- getROM x
  ram' <- newTVarIO def -- Memory 0x200 $ M.fromList $ (\x -> (x,fromIntegral $ x `mod` 256)) <$> [0..4095]
  atomically $ modifyTVar ram' $ \(Memory p m) -> Memory p $ M.union rom m
  reg' <- newTVarIO def --Registers $ M.fromList $ zip [V0 .. VF] (repeat 0)
  ir' <- newTVarIO def
  stack' <- newTVarIO def
  timers' <- newTVarIO def
  scr' <- defDisplay
  k <- newTVarIO $ M.fromList $ zip [K0 .. KF] $ repeat False
  let c8 = Chip8 ram' reg' ir' stack' timers' scr' (Keyboard k)
  forkIO $ gameLoop c8
  return c8

data Chip8 =
  Chip8
    { ram :: TVar Memory -- Add a newtype with size 4096
    , reg :: TVar Registers
    , ir :: TVar Word16
    , stack :: TVar Stack -- Add a newtype with fixed size 16
    , timers :: TVar Timers
    , scr :: Display'
    , keyboard :: Keyboard -- Add a newtype with size 16
    }
