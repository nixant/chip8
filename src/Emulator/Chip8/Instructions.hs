module Emulator.Chip8.Instructions where

import Data.Word
import Emulator.Chip8.Registers

-- TODO: refactor instruction code name
data Instruction
  = NOP -- 0000
  | CLS -- 00E0
  | RET -- 00EE
  | JMP Word16 -- 1NNN
  | CALL Word16 -- 2NNN
  | VXE Register Word8 -- 3XNN (skip next if VX == NN)
  | VXNE Register Word8 -- 4XNN (skip next if VX /= NN)
  | VXYE Register Register -- 5XY0 (skip next if VX == VY)
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
  | VXYNE Register Register -- 9XY0 (skip next if VX /= VY)
  | SI Word16 -- ANNN (set IR to NNN)
  | JMV0 Word16 -- BNNN (jump to V0 + NNN)
  | RAND Register Word8 -- CXNN (set VX to random && NN)
  | DRAW Register Register Word8 -- DXYN (draw an 8xN size sprite at values at VX and VY)
  | KP Register -- EX9E
  | KNP Register -- EXA1
  | SXDT Register -- FX07 (set VX to Delay Timer)
  | KINP Register -- FX0A (wait for key press | blocking)
  | SDTX Register -- FX15 (dt = vx)
  | SSTX Register -- FX18 (st = vx)
  | SIX Register -- FX1E (I += VX)
  | SIFX Register -- FX29 (set I to font character in VX)
  | SIBX Register -- FX33 (set BCD encoding of VX into I)
  | SMX Register -- FX55 (store V0 to VX into RAM starting at addr at I)
  | SXM Register -- FX65 (store RAM into V0 to VX starting at addr at I)
  deriving (Eq, Show)
