module Main where

import Data.Version
import Emulator.Chip8
import Paths_chip8

main :: IO ()
main = do
  putStrLn $ "chip8-" <> showVersion version <> " - An emulator for Chip8"
  runEmuDef cycleEmu
