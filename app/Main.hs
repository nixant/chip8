module Main where

import Emulator.Chip8
import Data.Version
import Paths_chip8

main :: IO ()
main = do
    putStrLn $ "chip8-" <> showVersion version <> " - An emulator for Chip8"
    runEmuDef cycleEmu
