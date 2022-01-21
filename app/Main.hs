module Main where

import Data.Version
import Emulator.Chip8
import Paths_chip8
import Control.Concurrent (newMVar)
import Control.Concurrent.Async (concurrently_)
import Emulator.Chip8 (timeCPU)

main :: IO ()
main = do
  putStrLn $ "chip8-" <> showVersion version <> " - An emulator for Chip8"
  m <- newMVar True
  concurrently_ (timeCPU m) (runEmuDef $ cycleEmu m)
