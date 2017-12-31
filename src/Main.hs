module Main where

import           Data.Char                     as C
import           Data.List                     as L
import           Data.Maybe                    as M

import           Control.Monad                 (when)

import           Control.Concurrent            (threadDelay)
import           Control.Concurrent.STM        (atomically)
import           Control.Concurrent.STM.TChan  (tryReadTChan)

import           Graphics.Vty
import           Graphics.Vty.Image
import           Graphics.Vty.Output.Interface

fpsToDelay :: Int -> Int
fpsToDelay fps = 1000000 `div` fps

generate :: Int -> Int -> Int -> Image
generate frame width height =
  let line =
        horizCat $
        map (\i -> char (attr (i + frame)) (c (i + frame))) [0 .. width]
  in vertCat $ L.replicate height line
  where
    c n = C.chr $ (n `mod` 52) + 48
    attr n =
      currentAttr `withBackColor` Color240 (fromIntegral $ n `mod` 240) `withForeColor`
      Color240 (fromIntegral $ abs (240 - n) `mod` 240)

tryEvent :: Vty -> IO (Maybe Event)
tryEvent vty =
  let channel = _eventChannel $ inputIface vty
  in atomically $ tryReadTChan channel

main = do
  cfg <- standardIOConfig
  vty <- mkVty cfg
  loop vty 0

loop :: Vty -> Int -> IO ()
loop vty frame = do
  bounds <- displayBounds $ outputIface vty
  let height = regionHeight bounds
  let width = regionWidth bounds
  let pic = picForImage $ generate frame width height
  update vty pic
  e <- tryEvent vty
  when (M.isJust e) (shutdown vty)
  threadDelay (fpsToDelay 30)
  loop vty (frame + 1)
