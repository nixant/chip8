module Emulator.Chip8.Instructions where

import Data.Word 
import Emulator.Chip8.Registers

-- TODO: refactor instruction code name

data Instruction = NOP -- 0000
                 | CLS -- 00E0
                 | RET -- 00EE
                 | JMP Word16 -- 1NNN
                 | CALL Word16 -- 2NNN
                 | VXE Register Word8 -- 3XNN (skip next if VX == NN)
                 | VXNE Register Word8 -- 4XNN (skip next if VX /= NN)
                 | VXYE Register Register  -- 5XY0 (skip next if VX == VY)
                 | SVX Register Word8 -- 6XNN (set VX to NN)
                 | AVX Register Word8 -- 7XNN  (VX += NN)
                 | SVXY Register Register -- 8XY0 (VX = VY)
                 | BOXY Register Register -- 8XY1 (VX |= VY)
                 | BAXY Register Register -- 8XY2 (VX &= VY)
                 | BXXY Register Register -- 8XY3 (VX ^= VY)
                 | AXY Register Register -- 8XY4 (VX += VY set VF if carry)
                 | MXY Register Register -- 8XY5 (VX -= VY clear VF if borrow)
                 | RSX Register -- 8XY6 (VX >> = 1 store dropped bit in VF)
                 | MYX Register Register -- 8XY7 (VX = VY - VX clear VF if borrow)
                 | LSX Register -- 8XYE (VX << = 1 store dropped bit in VF)
                 | VXYNE Register Register  -- 9XY0 (skip next if VX /= VY)
                 deriving (Eq, Show)