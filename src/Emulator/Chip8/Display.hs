{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}

module Emulator.Chip8.Display where

import SDL
import Control.Monad (unless)
import Control.Concurrent (threadDelay)
import Data.Word (Word8)
import Control.Concurrent.Chan
import Control.Concurrent.Async
import Control.Exception ()
import Data.Maybe (fromJust, fromMaybe)
import qualified Data.Map as M
import Control.Concurrent.STM
import Data.Default

type DisplayState = M.Map (Word8, Word8) Bool

data Display' = Display' { displayState :: TVar DisplayState, displayBuffer :: Chan DisplayCommand}

defDisplay :: IO Display'
defDisplay = Display' <$> initDisplayState <*> initDisplayBuffer

initDisplayState :: IO (TVar DisplayState)
initDisplayState = newTVarIO $ M.fromList $ [((x,y), False) | x <- [0 .. fromIntegral windowWidth], y <- [0 .. fromIntegral windowHeight]]

initDisplayBuffer :: IO (Chan DisplayCommand)
initDisplayBuffer = newChan

class HasDisplay a where
  writeBuffer :: a -> DisplayCommand -> IO ()
  writeManyBuffer :: a -> [DisplayCommand] -> IO ()
  getDisplayState :: a -> IO DisplayState
  setDisplayState :: a -> DisplayState -> IO ()
  updateDisplayState :: a -> (DisplayState -> DisplayState) -> IO ()
  updateDisplayState d f = getDisplayState d >>= setDisplayState d . f
  getDisplayPixel :: a -> (Word8, Word8) -> IO Bool 
  getDisplayPixel d k = do
    m <- getDisplayState d
    return $ fromMaybe False $ M.lookup k m
  setDisplayPixel :: a -> (Word8, Word8) -> Bool -> IO ()
  setDisplayPixel d k v = updateDisplayState d (M.insert k v)
  updateDisplayPixel :: a -> (Word8, Word8) -> (Bool -> Bool) -> IO ()
  updateDisplayPixel d p f = getDisplayPixel d p >>= setDisplayPixel d p . f
  flipDisplayPixel :: a -> (Word8, Word8) -> IO ()
  flipDisplayPixel d p = updateDisplayPixel d p not

instance HasDisplay Display' where
  writeBuffer Display' {displayBuffer} = writeChan displayBuffer
  writeManyBuffer Display' {displayBuffer} = writeList2Chan displayBuffer
  getDisplayState Display' {displayState} = readTVarIO displayState
  setDisplayState Display' {displayState} = atomically . writeTVar displayState
  updateDisplayState Display' {displayState} = atomically . modifyTVar' displayState

windowWidth = 64

windowHeight = 32

scale = 20

data DisplayCommand = Clear
                    | ClearC Color
                    | Present
                    | Draw Word8 Word8
                    | DrawC Word8 Word8 Color
                    | NoCommand

data Color = Color Word8 Word8 Word8 Word8 

colorToV4 (Color a r g b) = V4 a r g b

display :: Chan DisplayCommand -> IO ()
display chan = do
  initializeAll
  window <- createWindow "CHIP 8" defaultWindow {windowInitialSize = V2 (windowWidth * scale) (windowHeight * scale)}
  renderer <- createRenderer window (-1) defaultRenderer
  rendererScale renderer $= V2 (fromIntegral scale) (fromIntegral scale)
  gameLoop chan renderer window

gameLoop :: Chan DisplayCommand -> Renderer -> Window -> IO ()
gameLoop chan renderer window = concurrently_ (eventLoop renderer >> destroyWindow window)
                                              (concurrently_ (displayLoop chan renderer) (presentLoop renderer 20))

eventLoop :: Renderer -> IO ()
eventLoop renderer = do
  event <- waitEvent
  let eventIsEscPress event =
        case eventPayload event of
          KeyboardEvent keyboardEvent ->
            keyboardEventKeyMotion keyboardEvent == Pressed &&
            keysymKeycode (keyboardEventKeysym keyboardEvent) == KeycodeEscape
          _ -> False
      escPressed = eventIsEscPress event
  unless escPressed $ eventLoop renderer

displayLoop :: Chan DisplayCommand -> Renderer -> IO ()
displayLoop chan renderer = do
  cmd <- readChan chan
  case cmd of
    Clear -> do
      rendererDrawColor renderer $= V4 0 0 0 0
      clear renderer
    ClearC c -> do
      rendererDrawColor renderer $= colorToV4 c
      clear renderer
    Draw x y -> do
      rendererDrawColor renderer $= V4 0 255 255 255
      drawPoint renderer (P $ V2 (fromIntegral x) (fromIntegral y))
    DrawC x y c -> do
      rendererDrawColor renderer $= colorToV4 c
      drawPoint renderer (P $ V2 (fromIntegral x) (fromIntegral y))
    Present ->  present renderer
    NoCommand -> return ()
  displayLoop chan renderer

presentLoop renderer fps = present renderer >> threadDelay (1000000 `div` fps) >> presentLoop renderer fps 
