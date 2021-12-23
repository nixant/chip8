{-# LANGUAGE FlexibleInstances #-}

module Emulator.Chip8.Timers where

import Control.Concurrent
import Control.Concurrent.STM
  ( TVar
  , atomically
  , modifyTVar'
  , readTVarIO
  , writeTVar
  )
import Data.Default
import Data.Word (Word8)

data Timer =
  Timer
    { timer :: Word8
    , timerAction :: Word8 -> IO ()
    }

delayTimerFn :: Word8 -> IO ()
delayTimerFn _ = return ()

soundTimerFn :: Word8 -> IO ()
soundTimerFn 0 = beep
soundTimerFn _ = return ()

beep :: IO ()
beep = putChar '\a' -- putStrLn "b -- e -- e -- p"

data Timers =
  Timers
    { dt :: Timer
    , st :: Timer
    }

instance Default Timers where
  def = Timers {dt = Timer 4 delayTimerFn, st = Timer 4 soundTimerFn}

tickTimer :: Timer -> Timer
tickTimer (Timer 0 f) = Timer 0 f
tickTimer (Timer n f) = Timer (n - 1) f

tickTimers :: Timers -> Timers
tickTimers (Timers d s) = Timers (tickTimer d) (tickTimer s)

class HasTimers a where
  tick :: a -> IO ()
  tickAfter :: a -> Int -> IO ()
  tickAfter c t = tick c >> threadDelay (1000000 `div` t) >> tickAfter c t
  getDT :: a -> IO Word8
  setDT :: a -> Word8 -> IO ()
  getST :: a -> IO Word8
  setST :: a -> Word8 -> IO ()

instance HasTimers (TVar Timers) where
  tick t =
    atomically (modifyTVar' t tickTimers) >> readTVarIO t >>= \(Timers d s) ->
      timerAction d (timer d) >> timerAction s (timer s)
  getDT = fmap (timer . dt) . readTVarIO
  setDT t d =
    atomically $ modifyTVar' t (\(Timers dt st) -> Timers dt {timer = d} st)
  getST = fmap (timer . st) . readTVarIO
  setST t d =
    atomically $ modifyTVar' t (\(Timers dt st) -> Timers dt st {timer = d})
