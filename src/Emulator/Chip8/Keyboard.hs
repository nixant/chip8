{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns #-}

module Emulator.Chip8.Keyboard where
import qualified Data.Map.Strict as M
import Control.Concurrent.STM
import Data.Maybe (fromMaybe)
import Control.Concurrent

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

newtype Keyboard = Keyboard {keys :: TVar (M.Map Key Bool)} deriving (Eq, Show)

class HasKeyboard k where
    press :: k -> Key -> IO ()
    release :: k -> Key -> IO ()
    keyStatus :: k -> Key -> IO Bool
    readPressed :: k -> IO Key

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

