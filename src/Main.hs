module Main where

import           Data.List                     as L

import           Graphics.Vty
import           Graphics.Vty.Image
import           Graphics.Vty.Output.Interface

generate :: Int -> Int -> Image
generate width height =
  let line =
        horizCat $
        map
          (\i ->
             string
               (currentAttr `withBackColor`
                Color240 (fromIntegral $ i `mod` 240))
               " ")
          [0 .. width]
  in vertCat $ L.replicate height line

main = do
  cfg <- standardIOConfig
  vty <- mkVty cfg
  let output = outputIface vty
  bounds <- displayBounds output
  let height = regionHeight bounds
  let width = regionWidth bounds
  let pic = picForImage $ generate width height
  update vty pic
  e <- nextEvent vty
  shutdown vty
  print ("Last event was: " ++ show e)
