{-# LANGUAGE NamedFieldPuns #-}

module Main where

import Control.Concurrent
import Control.Monad
import Data.Vector ((!))
import Graphics.UI.SDL as SDL

import Hemokit.Start
import Hemokit


main :: IO ()
main = do

  emotivStateMvar <- newEmptyMVar
  _ <- forkIO $ do
    putMVar emotivStateMvar Nothing
    putStrLn "Waiting for EEG data..."

    -- Connect to any device
    m'device <- getEmotivDeviceFromArgs =<< parseArgs "FFT on Emotiv data" emotivArgsParser
    case m'device of
      Left  _      -> print "No device"
      Right device -> forever $ do m'st <- readEmotiv device
                                   case m'st of
                                    Nothing -> print "no data"
                                    Just (x,_) -> void $ swapMVar emotivStateMvar (Just x)
                                   

    -- Old
    -- withDataFromLastEEG Consumer (void . swapMVar emotivStateMvar . fst)

  -- Initialise SDL
  SDL.init [InitVideo]

  -- Create window
  screen <- setVideoMode 640 480 16 [SWSurface]
  setCaption "Test" ""

  forever $ do
    
    -- Obtain surface
    screen <- getVideoSurface

    -- Paint screen green
    let format = surfaceGetPixelFormat screen
    green <- mapRGB format 0 0xFF 0
    fillRect screen Nothing green

    state <-  readMVar emotivStateMvar
    case state of
      (Just (EmotivState{ counter, sensors})) -> do
        forM_ allSensors $ \s -> do
          drawSSensor screen format (sensorHPosition s) s (sensors ! (fromEnum s))
      _ -> return ()

    -- Double buffering
    SDL.flip screen


drawSSensor screen format pos s qual = do
    red <- mapRGB format 0xFF 0 0
    let side = 10
        x = pos * w
        y = h
        w = surfaceGetWidth screen `div` 14
        step :: Double
        step = fromIntegral (surfaceGetHeight screen) / 16384
        h = round $ step * fromIntegral qual
    drawGradientVect screen format x y w h -- fillRect screen (Just (Rect x y w h)) red

drawGradientVect screen format x y w h = do
    forM_ [0..y] $ \v -> drawLine screen format x v w h

drawLine screen format x y w h = do
    let screenH = surfaceGetHeight screen
        redL    = 256 - (fromIntegral $ 256 * y `div` screenH)
        y'      = surfaceGetHeight screen - y
    color <- mapRGB format redL 0 0
    fillRect screen (Just (Rect x y' w 1)) color

sensorHPosition :: Sensor -> Int
sensorHPosition sensor = case sensor of
  -- Left side
  AF3 -> 0
  F3  -> 1
  F7  -> 2
  FC5 -> 3
  T7  -> 4
  P7  -> 5
  O1  -> 6

  -- Right side
  AF4 -> 7
  F4  -> 8
  F8  -> 9
  FC6 -> 10
  T8  -> 11
  P8  -> 12
  O2  -> 13
