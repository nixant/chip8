{-# LANGUAGE FlexibleInstances #-}

module Emulator.Chip8.Keyboard where
import qualified Data.Map.Strict as M
import Control.Concurrent.STM
import Data.Maybe (fromMaybe)
import Control.Concurrent
import SDL

data Key = K0
         | K1
         | K2
         | K3
         | K4
         | K5
         | K6
         | K7
         | K8
         | K9
         | KA
         | KB
         | KC
         | KD
         | KE
         | KF
         deriving (Eq, Ord, Enum, Show)

newtype Keyboard = Keyboard {keys :: TVar (M.Map Key Bool)} deriving (Eq)

class HasKeyboard k where
    press :: k -> Key -> IO ()
    release :: k -> Key -> IO ()
    keyStatus :: k -> Key -> IO Bool
    readPressed :: k -> IO Key
    getKeyboardState :: k -> IO Keyboard

instance HasKeyboard Keyboard where
    press (Keyboard k) key = do
        atomically $ modifyTVar k $ M.insert key True
    release (Keyboard k) key = do
        atomically $ modifyTVar k $ M.insert key False
    keyStatus (Keyboard k) key = M.findWithDefault False key <$> readTVarIO k
    readPressed (Keyboard k) = loop k
        where loop km = do
                kb <-  M.assocs . M.filter id <$> readTVarIO km
                if null kb
                    then loop km
                    else return . fst . head $ kb
    getKeyboardState = return

eventLoop :: Keyboard -> IO ()
eventLoop kb@(Keyboard k) = do
  event <- waitEvent
  case eventPayload event of
          KeyboardEvent keyboardEvent ->
            case (keysymKeycode (keyboardEventKeysym keyboardEvent), keyboardEventKeyMotion keyboardEvent) of
              (Keycode1, Released) -> (atomically $ modifyTVar k $ M.insert K0 False) >> eventLoop kb 
              (Keycode1, Pressed) -> (atomically $ modifyTVar k $ M.insert K0 True) >> eventLoop kb
              (Keycode2, Released) -> (atomically $ modifyTVar k $ M.insert K1 False) >> eventLoop kb 
              (Keycode2, Pressed) -> (atomically $ modifyTVar k $ M.insert K1 True) >> eventLoop kb
              (Keycode3, Released) -> (atomically $ modifyTVar k $ M.insert K2 False) >> eventLoop kb 
              (Keycode3, Pressed) -> (atomically $ modifyTVar k $ M.insert K2 True) >> eventLoop kb
              (Keycode4, Released) -> (atomically $ modifyTVar k $ M.insert K3 False) >> eventLoop kb 
              (Keycode4, Pressed) -> (atomically $ modifyTVar k $ M.insert K3 True) >> eventLoop kb
              (KeycodeQ, Released) -> (atomically $ modifyTVar k $ M.insert K4 False) >> eventLoop kb 
              (KeycodeQ, Pressed) -> (atomically $ modifyTVar k $ M.insert K4 True) >> eventLoop kb
              (KeycodeW, Released) -> (atomically $ modifyTVar k $ M.insert K5 False) >> eventLoop kb 
              (KeycodeW, Pressed) -> (atomically $ modifyTVar k $ M.insert K5 True) >> eventLoop kb
              (KeycodeE, Released) -> (atomically $ modifyTVar k $ M.insert K6 False) >> eventLoop kb 
              (KeycodeE, Pressed) -> (atomically $ modifyTVar k $ M.insert K6 True) >> eventLoop kb
              (KeycodeR, Released) -> (atomically $ modifyTVar k $ M.insert K7 False) >> eventLoop kb 
              (KeycodeR, Pressed) -> (atomically $ modifyTVar k $ M.insert K7 True) >> eventLoop kb
              (KeycodeA, Released) -> (atomically $ modifyTVar k $ M.insert K8 False) >> eventLoop kb 
              (KeycodeA, Pressed) -> (atomically $ modifyTVar k $ M.insert K8 True) >> eventLoop kb
              (KeycodeS, Released) -> (atomically $ modifyTVar k $ M.insert K9 False) >> eventLoop kb 
              (KeycodeS, Pressed) -> (atomically $ modifyTVar k $ M.insert K9 True) >> eventLoop kb
              (KeycodeD, Released) -> (atomically $ modifyTVar k $ M.insert KA False) >> eventLoop kb 
              (KeycodeD, Pressed) -> (atomically $ modifyTVar k $ M.insert KA True) >> eventLoop kb
              (KeycodeF, Released) -> (atomically $ modifyTVar k $ M.insert KB False) >> eventLoop kb 
              (KeycodeF, Pressed) -> (atomically $ modifyTVar k $ M.insert KB True) >> eventLoop kb
              (KeycodeZ, Released) -> (atomically $ modifyTVar k $ M.insert KC False) >> eventLoop kb 
              (KeycodeZ, Pressed) -> (atomically $ modifyTVar k $ M.insert KC True) >> eventLoop kb
              (KeycodeX, Released) -> (atomically $ modifyTVar k $ M.insert KD False) >> eventLoop kb 
              (KeycodeX, Pressed) -> (atomically $ modifyTVar k $ M.insert KD True) >> eventLoop kb
              (KeycodeC, Released) -> (atomically $ modifyTVar k $ M.insert KE False) >> eventLoop kb 
              (KeycodeC, Pressed) -> (atomically $ modifyTVar k $ M.insert KE True) >> eventLoop kb
              (KeycodeV, Released) -> (atomically $ modifyTVar k $ M.insert KF False) >> eventLoop kb 
              (KeycodeV, Pressed) -> (atomically $ modifyTVar k $ M.insert KF True) >> eventLoop kb
              (KeycodeEscape, Pressed) -> return ()
              (_,_) -> eventLoop kb
          _ -> eventLoop kb