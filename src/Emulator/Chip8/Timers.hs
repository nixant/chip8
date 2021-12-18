{-# LANGUAGE FlexibleInstances #-}
module Emulator.Chip8.Timers where
import Data.Word (Word8)
import Control.Concurrent.STM (TVar, atomically, modifyTVar', readTVarIO)
import Data.Default

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

instance HasTimers (TVar Timers) where
  tick t =
    atomically (modifyTVar' t tickTimers) >> readTVarIO t >>= \(Timers d s) ->
      timerAction d (timer d) >> timerAction s (timer s)
