{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Emulator.Chip8 where

import Control.Concurrent.MVar

import Control.Concurrent
import Control.Concurrent.STM.TVar
import Control.Monad.Reader
import Control.Monad.STM
import Data.Bits ((.|.), shiftL, (.&.), shiftR, xor)
import Data.Default
import qualified Data.Map as M
import Data.Maybe (catMaybes, fromMaybe)
import Data.Word (Word16, Word8)
import Emulator.Chip8.CPU
import Emulator.Chip8.Display
import Emulator.Chip8.Instructions
import Emulator.Chip8.Keyboard
import Emulator.Chip8.Memory
import Emulator.Chip8.Registers
import Emulator.Chip8.Registers (mkRegister)
import Emulator.Chip8.Stack
import Emulator.Chip8.Timers
import System.Random (randomRIO)
import Control.Concurrent.Async (race)
import Emulator.Chip8.IO (gameLoop)

newtype Emu a =
  Emu (ReaderT Chip8 IO a)
  deriving (Functor, Applicative, Monad, MonadReader Chip8, MonadIO) -- , MonadThrow, MonadCatch)

runEmu :: Emu a -> Chip8 -> IO ()
runEmu (Emu app) c8 = void $ race (runReaderT app c8) (gameLoop c8)
  

runEmuDef :: Emu a -> IO ()
runEmuDef app = defChip8 >>= runEmu app

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

getBCD :: Word8 -> [Word8]
getBCD = pad3 . fmap (read . (: [])) . show
  where
    pad3 [] = [0, 0, 0]
    pad3 [a] = [0, 0, a]
    pad3 [a, b] = [0, a, b]
    pad3 x = x

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
decode (0xA, a, b, c) = SI $ mkWord12 (a, b, c)
decode (0xB, a, b, c) = JMV0 $ mkWord12 (a, b, c)
decode (0xC, x, a, b) = RAND (mkRegister x) $ mkWord8 (a, b)
decode (0xD, x, y, a) = DRAW (mkRegister x) (mkRegister y) a
decode (0xE, x, 9, 0xE) = KP (mkRegister x)
decode (0xE, x, 0xA, 1) = KNP (mkRegister x)
decode (0xF, x, 0, 7) = SXDT (mkRegister x)
decode (0xF, x, 0, 0xA) = KINP (mkRegister x)
decode (0xF, x, 1, 5) = SDTX (mkRegister x)
decode (0xF, x, 1, 8) = SSTX (mkRegister x)
decode (0xF, x, 1, 0xE) = SIX (mkRegister x)
decode (0xF, x, 2, 9) = SIFX (mkRegister x)
decode (0xF, x, 3, 3) = SIBX (mkRegister x)
decode (0xF, x, 5, 5) = SMX (mkRegister x)
decode (0xF, x, 6, 5) = SXM (mkRegister x)
decode x = error $ "decode error/ invalid opcode: " <> show x

draw :: HasDisplay a => a -> [(Word8, Word8)] -> IO (Bool, [DisplayCommand])
draw d =
  foldM
    (\(f, ds) p@(x, y) -> do
       val <- getDisplayPixel d p
       flipDisplayPixel d p
       let c =
             if val
               then Color 0 0 0 0
               else Color 0 255 255 255
       return (f || val, DrawC x y c : ds))
    (False, [])

execute :: (HasChip8 env, MonadReader env m, MonadIO m) => Instruction -> m ()
execute NOP = return ()
execute CLS = do
  c8 <- ask
  liftIO $ writeBuffer c8 Clear
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
    setReg c8 vx $ x `shiftR` 1
    setReg c8 VF $ x .&. 1
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
    setReg c8 vx $ x `shiftL` 1
    setReg c8 vx $ x `shiftR` 7
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
execute (DRAW vx vy n)
 = do
  c8 <- ask
  liftIO $ do
    x <- getReg c8 vx
    y <- getReg c8 vy
    i <- getIR c8
    p <-
      forM [0 .. n - 1] $ \row -> do
        let addr = i + fromIntegral row
        pixels <- getMemAt c8 addr
        forM [0 .. 7] $ \col -> do
          if pixels .&. (128 `shiftR` col) /= 0
            then do
              let x' = (x + fromIntegral col) `mod` fromIntegral windowWidth
              let y' = (y + row) `mod` fromIntegral windowHeight
              return $ Just (x', y')
            else return Nothing
    let ps = catMaybes $ concat p
    (f, d) <- draw c8 ps
    when f $ setReg c8 VF 1
    liftIO $ writeManyBuffer c8 d
execute (KP vx) = do
  c8 <- ask
  liftIO $ do
    x <- getReg c8 vx
    pressed <- keyStatus c8 $ toEnum $ fromIntegral x
    when pressed $ modifyPC c8 (+ 2)
execute (KNP vx) = do
  c8 <- ask
  liftIO $ do
    x <- getReg c8 vx
    pressed <- keyStatus c8 $ toEnum $ fromIntegral x
    unless pressed $ modifyPC c8 (+ 2)
execute (SXDT vx) = do
  c8 <- ask
  liftIO $ getDT c8 >>= setReg c8 vx
execute (KINP vx) = do
  c8 <- ask
  liftIO $ do
    kp <- readPressed c8
    case kp of
      Nothing -> decPC c8 >> decPC c8
      Just k -> setReg c8 vx $ fromIntegral $ fromEnum k
execute (SDTX vx) = do
  c8 <- ask
  liftIO $ getReg c8 vx >>= setDT c8
execute (SSTX vx) = do
  c8 <- ask
  liftIO $ getReg c8 vx >>= setST c8
execute (SIX vx) = do
  c8 <- ask
  liftIO $ getReg c8 vx >>= modifyIR c8 . (+) . fromIntegral
execute (SIFX vx) = do
  c8 <- ask
  liftIO $ getReg c8 vx >>= setIR c8 . (* 5) . fromIntegral
execute (SIBX vx) = do
  c8 <- ask
  liftIO $ do
    addr <- getIR c8
    [a, b, c] <- getBCD <$> getReg c8 vx
    setMemAt c8 addr a
    setMemAt c8 (addr + 1) b
    setMemAt c8 (addr + 2) c
execute (SMX vx) = do
  c8 <- ask
  liftIO $ do
    i <- getIR c8
    mapM_ (\(v, addr) -> getReg c8 v >>= setMemAt c8 addr) $
      zip [V0 .. vx] [i ..]
    setIR c8 i
execute (SXM vx) = do
  c8 <- ask
  liftIO $ do
    i <- getIR c8
    mapM_ (\(v, addr) -> getMemAt c8 addr >>= setReg c8 v) $
      zip [V0 .. vx] [i ..]
    setIR c8 i
execute x = error $ "unimplemented instruction: " <> show x

cycleEmu :: (HasChip8 env, MonadReader env m, MonadIO m) => MVar Bool -> m ()
-- cycleEmu m = liftIO (takeMVar m) >> fetch >>= execute . decode >> cycleEmu m
cycleEmu m = liftIO (takeMVar m) >> fetch >>= \op -> liftIO (print (decode op)) >> execute (decode op) >> cycleEmu m

timeCPU :: MVar Bool -> IO ()
timeCPU m = forever $ do
  putMVar m True
  threadDelay (1000000 `div` 600)

