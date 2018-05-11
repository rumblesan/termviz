module Generator where

import           Data.Char (chr)

type Colour = (Float, Float, Float)

type GeneratorFunc = Float -> Float -> (Colour, Colour, Char)

-- coordinates have 0, 0 in top left
-- range between 0.0 and 1.0
generate :: GeneratorFunc
generate xPos yPos =
  let br = (sin (xPos * yPos) + 1) / 2
      bg = (cos (sin (xPos * yPos)) + 1) / 2
      bb = (sin (xPos * yPos + 1) + 1) / 2
      bc = (br, bg, bb)
      cc = (0, 0, 0)
      c = chr $ round $ (xPos * 69) + 48
  in (bc, cc, c)
