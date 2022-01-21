{-# LANGUAGE ConstraintKinds #-}

module Emulator.Chip8.IO where

import Control.Concurrent.Async (concurrently_, race_)
import Emulator.Chip8.Display
import Emulator.Chip8.Keyboard
import Emulator.Chip8.Timers
import SDL.Video ( destroyWindow )

type HasIO m = (HasDisplay m, HasKeyboard m)

gameLoop :: (HasIO m, HasTimers m) => m -> IO ()
gameLoop c8 = do
  kb <- getKeyboardState c8
  let dsp = getDisplay c8
      db = displayBuffer dsp
      rn = displayRenderer dsp
      wd = displayWindow dsp
  race_
    (eventLoop kb >> destroyWindow wd)
    (concurrently_
       (displayLoop db rn)
       (concurrently_ (tickAfter c8 60) (presentLoop rn 600)))
