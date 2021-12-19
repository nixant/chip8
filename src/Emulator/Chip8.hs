{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Emulator.Chip8 where

import Control.Concurrent.MVar
-- implement complete chip here
import Control.Concurrent.STM.TVar
import Control.Monad.Reader
import Control.Monad.STM
import Data.Bits (Bits((.|.), shiftL), (.&.), shiftR, xor)
import Data.Default
import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import Data.Word (Word16, Word8)
import Emulator.Chip8.CPU
import Emulator.Chip8.Instructions
import Emulator.Chip8.Memory
import Emulator.Chip8.Registers
import Emulator.Chip8.Stack
import Emulator.Chip8.Timers
import System.Random (randomRIO)

newtype Emu a =
  Emu (ReaderT Chip8 IO a)
  deriving (Functor, Applicative, Monad, MonadReader Chip8, MonadIO) -- , MonadThrow, MonadCatch)

runEmu :: Chip8 -> Emu a -> IO a
runEmu env (Emu app) = runReaderT app env

runEmuDef :: Emu a -> IO a
runEmuDef app = do
  c8 <- defChip8
  runEmu c8 app

fetch ::
     (HasChip8 env, MonadReader env m, MonadIO m)
  => m (Word8, Word8, Word8, Word8)
fetch = do
  c8 <- ask
  ms <- liftIO $ getMemory c8
  liftIO $ incPC c8
  ls <- liftIO $ getMemory c8
  liftIO $ incPC c8
  let (d1, d2) = splitWord8 ms
  let (d3, d4) = splitWord8 ls
  return (d1, d2, d3, d4)

splitWord8 w = ((w .&. 0xF0) `shiftR` 4, w .&. 0xF)

mkWord8 (a, b) = fromIntegral a `shiftL` 4 .|. fromIntegral b

mkWord12 (a, b, c) = fromIntegral a `shiftL` 8 .|. mkWord8 (b, c)

decode :: (Word8, Word8, Word8, Word8) -> Instruction
decode (0, 0, 0, 0) = NOP -- no operation
decode (0, 0, 0xE, 0) = CLS -- clear screen
decode (0, 0, 0xE, 0xE) = RET -- return
decode (1, a, b, c) = JMP $ mkWord12 (a, b, c) -- jump to address
decode (2, a, b, c) = CALL $ mkWord12 (a, b, c) -- call subroutine
decode (3, x, a, b) = VXE (mkRegister x) $ mkWord8 (a, b) -- skip next if vx == nn
decode (4, x, a, b) = VXNE (mkRegister x) $ mkWord8 (a, b) -- skip next if vx /= nn
decode (5, x, y, 0) = VXYE (mkRegister x) (mkRegister x) -- skip next if vx == vy
decode (6, x, a, b) = SVX (mkRegister x) $ mkWord8 (a, b) -- vx = nn
decode (7, x, a, b) = AVX (mkRegister x) $ mkWord8 (a, b) -- vx += nn
decode (8, x, y, 0) = SVXY (mkRegister x) (mkRegister y) -- vx = vy
decode (8, x, y, 1) = BOXY (mkRegister x) (mkRegister y) -- vx |= vy
decode (8, x, y, 2) = BAXY (mkRegister x) (mkRegister y) -- vx &= vy
decode (8, x, y, 3) = BXXY (mkRegister x) (mkRegister y) -- vx ^= vy
decode (8, x, y, 4) = AXY (mkRegister x) (mkRegister y) -- vx += vy
decode (8, x, y, 5) = MXY (mkRegister x) (mkRegister y) -- vx -= vy
decode (8, x, y, 6) = RSX (mkRegister x) -- vx >> = 1
decode (8, x, y, 7) = MYX (mkRegister x) (mkRegister y) -- vx == vy - vx
decode (8, x, y, 0xE) = LSX (mkRegister x) -- vx << = 1
decode (9, x, y, 0) = VXYNE (mkRegister x) (mkRegister x) -- skip next if vx /= vy
decode (0xA, a, b, c) = SI $ mkWord12 (a,b,c)
decode (0xB, a, b, c) = JMV0 $ mkWord12 (a,b,c)
decode (0xC, x, a, b) = RAND (mkRegister x) $ mkWord8 (a,b)

execute :: (HasChip8 env, MonadReader env m, MonadIO m) => Instruction -> m ()
execute NOP = return ()
execute CLS = return () -- TODO: Add Clear Screen when screen implemented
execute RET = do
  c8 <- ask
  top <- liftIO $ pop c8
  liftIO $ setPC c8 top
execute (JMP addr) = do
  c8 <- ask
  liftIO $ setPC c8 addr
execute (CALL addr) = do
  c8 <- ask
  liftIO $ getPC c8 >>= \pc -> push pc c8
  execute (JMP addr)
execute (VXE reg val) = do
  c8 <- ask
  eq <- liftIO $ (== val) <$> getReg c8 reg
  when eq $ liftIO $ modifyPC c8 (+ 2)
execute (VXNE reg val) = do
  c8 <- ask
  eq <- liftIO $ (/= val) <$> getReg c8 reg
  when eq $ liftIO $ modifyPC c8 (+ 2)
execute (VXYE vx vy) = do
  c8 <- ask
  eq <- liftIO $ (==) <$> getReg c8 vx <*> getReg c8 vy
  when eq $ liftIO $ modifyPC c8 (+ 2)
execute (SVX vx nn) = do
  c8 <- ask
  liftIO $ setReg c8 vx nn
execute (AVX vx nn) = do
  c8 <- ask
  liftIO $ modifyReg c8 vx (+ nn)
execute (SVXY vx vy) = do
  c8 <- ask
  liftIO $ getReg c8 vy >>= setReg c8 vx
execute (BOXY vx vy) = do
  c8 <- ask
  liftIO $ getReg c8 vy >>= modifyReg c8 vx . (.|.)
execute (BAXY vx vy) = do
  c8 <- ask
  liftIO $ getReg c8 vy >>= modifyReg c8 vx . (.&.)
execute (BXXY vx vy) = do
  c8 <- ask
  liftIO $ getReg c8 vy >>= modifyReg c8 vx . xor
execute (AXY vx vy) = do
  c8 <- ask
  liftIO $ do
    x <- fromIntegral <$> getReg c8 vx
    y <- fromIntegral <$> getReg c8 vy
    let (s :: Word16) = x + y
    if s > 255
      then setReg c8 VF 1
      else setReg c8 VF 0
    setReg c8 vx $ fromIntegral s
execute (MXY vx vy) = do
  c8 <- ask
  liftIO $ do
    x <- fromIntegral <$> getReg c8 vx
    y <- fromIntegral <$> getReg c8 vy
    if x < y
      then setReg c8 VF 0
      else setReg c8 VF 1
    setReg c8 vx $ x - y
execute (RSX vx) = do
  c8 <- ask
  liftIO $ do
    x <- getReg c8 vx
    let vf = x .&. 1
    setReg c8 vx $ x `shiftR` 1
    setReg c8 vx vf
execute (MYX vx vy) = do
  c8 <- ask
  liftIO $ do
    x <- fromIntegral <$> getReg c8 vx
    y <- fromIntegral <$> getReg c8 vy
    if y < x
      then setReg c8 VF 0
      else setReg c8 VF 1
    setReg c8 vx $ y - x
execute (LSX vx) = do
  c8 <- ask
  liftIO $ do
    x <- getReg c8 vx
    let vf = x `shiftR` 7
    setReg c8 vx $ x `shiftL` 1
    setReg c8 vx vf
execute (VXYNE vx vy) = do
  c8 <- ask
  eq <- liftIO $ (/=) <$> getReg c8 vx <*> getReg c8 vy
  when eq $ liftIO $ modifyPC c8 (+ 2)
execute (SI addr) = do
  c8 <- ask
  liftIO $ setIR c8 addr
execute (JMV0 addr) = do
  c8 <- ask
  x <- liftIO $ getReg c8 V0
  execute (JMP $ fromIntegral x + addr)
execute (RAND vx nn) = do
  c8 <- ask
  liftIO $ do
    rn <- randomRIO (minBound, maxBound)
    setReg c8 vx (nn .&. rn)
execute x = error $ "unimplemented instruction: " <> show x

cycleEmu :: (HasChip8 env, MonadReader env m, MonadIO m) => m ()
cycleEmu = fetch >>= execute . decode >> ask >>= liftIO . tick >> cycleEmu
