{-# LANGUAGE TemplateHaskell #-}

module Gfx.TextRendering
  ( createTextRenderer
  , renderText
  , renderTextbuffer
  , resizeTextRendererScreen
  , changeTextColour
  , textCoordMatrix
  , TextRenderer
  ) where

import           Control.Monad             (forM_)
import           Data.Maybe                (listToMaybe)
import           GHC.Int                   (Int32)

import           Foreign.Marshal.Array     (withArray)
import           Foreign.Marshal.Utils     (fromBool, with)
import           Foreign.Ptr               (castPtr, nullPtr)
import           Foreign.Storable          (peek, sizeOf)

import qualified Data.Map.Strict           as M

import           Data.FileEmbed            (embedFile)
import           Gfx.Util.FontHandling     (Character (..), Font (..),
                                            getCharacter, loadFont)
import           Gfx.Util.LoadShaders      (ShaderInfo (..), ShaderSource (..),
                                            loadShaders)
import           Gfx.Util.VertexBuffers    (VBO (..), createVBO, drawVBO,
                                            setAttribPointer)
import qualified Graphics.GL               as GLRaw
import           Graphics.Rendering.OpenGL (ArrayIndex, AttribLocation (..),
                                            BlendEquation (FuncAdd),
                                            BlendingFactor (One, OneMinusSrcAlpha, SrcAlpha, Zero),
                                            BufferObject,
                                            BufferTarget (ArrayBuffer),
                                            BufferUsage (DynamicDraw),
                                            Capability (Enabled),
                                            ClearBuffer (ColorBuffer),
                                            Color4 (..), Color4,
                                            DataType (Float),
                                            FramebufferTarget (Framebuffer),
                                            GLfloat, IntegerHandling (ToFloat),
                                            NumArrayIndices,
                                            PrimitiveMode (Triangles), Program,
                                            ShaderType (FragmentShader, VertexShader),
                                            TextureTarget2D (Texture2D),
                                            TextureUnit (..),
                                            TransferDirection (WriteToBuffer),
                                            UniformLocation (..),
                                            VertexArrayDescriptor (..),
                                            VertexArrayObject, ($=))
import qualified Graphics.Rendering.OpenGL as GL

import           Gfx.PostProcessing        (Savebuffer (..), createSavebuffer,
                                            deleteSavebuffer)

import           Data.Vec                  (Mat44, multmm)
import           Generator                 (GeneratorFunc)
import           Gfx.Util.Matrices         (orthographicMat, translateMat)

data TextRenderer = TextRenderer
  { textFont        :: Font
  , charSize        :: Int
  , screenColumns   :: Int
  , screenRows      :: Int
  , pMatrix         :: Mat44 GLfloat
  , textprogram     :: Program
  , bgprogram       :: Program
  , characterQuad   :: VBO
  , characterBGQuad :: VBO
  , textColour      :: Color4 GLfloat
  , textBGColour    :: Color4 GLfloat
  , outbuffer       :: Savebuffer
  } deriving (Show)

textCoordMatrix :: Floating f => f -> f -> f -> f -> f -> f -> Mat44 f
textCoordMatrix left right top bottom near far =
  let o = orthographicMat left right top bottom near far
      t = translateMat (-1) 1 0
  in multmm t o

createCharacterTextQuad :: IO VBO
createCharacterTextQuad =
  let vertexSize = fromIntegral $ sizeOf (0 :: GLfloat)
      posVSize = 2
      texVSize = 2
      numVertices = 6
      firstPosIndex = 0
      firstTexIndex = posVSize * vertexSize
      vPosition = AttribLocation 0
      vTexCoord = AttribLocation 1
      numElements = numVertices * (posVSize + texVSize)
      size = fromIntegral (numElements * vertexSize)
      stride = fromIntegral ((posVSize + texVSize) * vertexSize)
      quadConfig = do
        GL.bufferData ArrayBuffer $= (size, nullPtr, DynamicDraw)
        setAttribPointer vPosition posVSize stride firstPosIndex
        setAttribPointer vTexCoord texVSize stride firstTexIndex
  in createVBO [quadConfig] Triangles firstPosIndex numVertices

createCharacterBGQuad :: IO VBO
createCharacterBGQuad =
  let vertexSize = fromIntegral $ sizeOf (0 :: GLfloat)
      posVSize = 2
      numVertices = 6
      firstPosIndex = 0
      vPosition = AttribLocation 0
      numElements = numVertices * posVSize
      size = fromIntegral (numElements * vertexSize)
      stride = 0
      quadConfig = do
        GL.bufferData ArrayBuffer $= (size, nullPtr, DynamicDraw)
        setAttribPointer vPosition posVSize stride firstPosIndex
  in createVBO [quadConfig] Triangles firstPosIndex numVertices

createTextRenderer ::
     Float -> Float -> Int -> Int -> Maybe FilePath -> Int -> IO TextRenderer
createTextRenderer front back width height fontPath charSize = do
  let textColour = Color4 0 0 0 0
  let bgColour = Color4 1 1 1 1
  cq <- createCharacterTextQuad
  cbq <- createCharacterBGQuad
  tprogram <-
    loadShaders
      [ ShaderInfo
          VertexShader
          (ByteStringSource $(embedFile "assets/shaders/textrenderer.vert"))
      , ShaderInfo
          FragmentShader
          (ByteStringSource $(embedFile "assets/shaders/textrenderer.frag"))
      ]
  bgshaderprogram <-
    loadShaders
      [ ShaderInfo
          VertexShader
          (ByteStringSource $(embedFile "assets/shaders/textrenderer-bg.vert"))
      , ShaderInfo
          FragmentShader
          (ByteStringSource $(embedFile "assets/shaders/textrenderer-bg.frag"))
      ]
  font <- loadFont fontPath charSize
  let columns = width `div` (fontAdvance font)
      rows = height `div` (fontAscender font)
      projectionMatrix =
        textCoordMatrix
          0
          (fromIntegral width)
          0
          (fromIntegral height)
          front
          back
  buffer <- createSavebuffer (fromIntegral width) (fromIntegral height)
  return $
    TextRenderer
      font
      charSize
      columns
      rows
      projectionMatrix
      tprogram
      bgshaderprogram
      cq
      cbq
      textColour
      bgColour
      buffer

resizeTextRendererScreen ::
     Float -> Float -> Int -> Int -> TextRenderer -> IO TextRenderer
resizeTextRendererScreen front back width height trender =
  let projectionMatrix =
        textCoordMatrix
          0
          (fromIntegral width)
          0
          (fromIntegral height)
          front
          back
  in do deleteSavebuffer $ outbuffer trender
        nbuffer <- createSavebuffer (fromIntegral width) (fromIntegral height)
        return trender {pMatrix = projectionMatrix, outbuffer = nbuffer}

changeTextColour :: Color4 GLfloat -> TextRenderer -> TextRenderer
changeTextColour newColour trender = trender {textColour = newColour}

renderText :: Int -> Int -> TextRenderer -> GeneratorFunc -> IO ()
renderText xpos ypos renderer genFunc = do
  let (Savebuffer fbo _ _ _ _) = outbuffer renderer
  GL.bindFramebuffer Framebuffer $= fbo
  GL.blend $= Enabled
  GL.blendEquationSeparate $= (FuncAdd, FuncAdd)
  GL.blendFuncSeparate $= ((SrcAlpha, OneMinusSrcAlpha), (One, Zero))
  GL.depthFunc $= Nothing
  GL.clearColor $= Color4 0.0 0.0 0.0 0.0
  GL.clear [ColorBuffer]
  renderCharacters xpos ypos renderer genFunc

renderTextbuffer :: TextRenderer -> IO ()
renderTextbuffer renderer = do
  GL.bindFramebuffer Framebuffer $= GL.defaultFramebufferObject
  let (Savebuffer _ text _ program quadVBO) = outbuffer renderer
  GL.currentProgram $= Just program
  GL.activeTexture $= TextureUnit 0
  GL.textureBinding Texture2D $= Just text
  drawVBO quadVBO

renderCharacters :: Int -> Int -> TextRenderer -> GeneratorFunc -> IO ()
renderCharacters xpos ypos renderer genFunc =
  let font = textFont renderer
      maxCol = fromIntegral (screenColumns renderer - 1) :: Float
      maxRow = fromIntegral (screenRows renderer - 1) :: Float
      xDelta = 1 / maxCol
      yDelta = 1 / maxRow
      characters = do
        x <- [0.0 .. maxCol]
        y <- [0.0 .. maxRow]
        let xP = x * xDelta
        let yP = y * xDelta
        return $ (round x, round y, (genFunc xP yP))
  in forM_
       characters
       (\(x, y, (bgc, fgc, c)) ->
          maybe (return ()) (\c -> renderChar c x y font) (getCharacter font c))
  where
    renderChar char xp yp f = do
      let xPos = (xp * fontAdvance f)
      let yPos = (yp * fontAscender f)
      renderCharacterBGQuad renderer char xPos yPos f
      renderCharacterTextQuad renderer char xPos yPos f

sendProjectionMatrix :: Program -> Mat44 GLfloat -> IO ()
sendProjectionMatrix program mat = do
  (UniformLocation projU) <- GL.get $ GL.uniformLocation program "projection"
  with mat $ GLRaw.glUniformMatrix4fv projU 1 (fromBool True) . castPtr

sendVertices :: [GLfloat] -> IO ()
sendVertices verts =
  let vertSize = sizeOf (head verts)
      numVerts = length verts
      size = fromIntegral (numVerts * vertSize)
  in withArray verts $ \ptr ->
       GL.bufferSubData ArrayBuffer WriteToBuffer 0 size ptr

renderCharacterQuad ::
     Program -> Mat44 GLfloat -> VBO -> IO () -> [GLfloat] -> IO ()
renderCharacterQuad program pMatrix character charDrawFunc charVerts =
  let (VBO arrayObject arrayBuffers primMode firstIndex numTriangles) =
        character
  in do GL.currentProgram $= Just program
        GL.bindVertexArrayObject $= Just arrayObject
        GL.bindBuffer ArrayBuffer $= listToMaybe arrayBuffers
        charDrawFunc
        sendProjectionMatrix program pMatrix
        sendVertices charVerts
        GL.drawArrays primMode firstIndex numTriangles

renderCharacterTextQuad ::
     TextRenderer -> Character -> Int -> Int -> Font -> IO ()
renderCharacterTextQuad renderer (Character c width height adv xBearing yBearing text) x y font =
  let baseline = fromIntegral (y + fontAscender font)
      gX1 = fromIntegral (x + xBearing)
      gX2 = gX1 + fromIntegral width
      gY1 = baseline - fromIntegral yBearing
      gY2 = gY1 + fromIntegral height
      charVerts =
        [ gX1
        , gY1
        , 0.0
        , 0.0 -- coord 1
        , gX1
        , gY2
        , 0.0
        , 1.0 -- coord 2
        , gX2
        , gY1
        , 1.0
        , 0.0 -- coord 3
        , gX1
        , gY2
        , 0.0
        , 1.0 -- coord 4
        , gX2
        , gY2
        , 1.0
        , 1.0 -- coord 5
        , gX2
        , gY1
        , 1.0
        , 0.0 -- coord 6
        ] :: [GLfloat]
      charDrawFunc = do
        GL.activeTexture $= TextureUnit 0
        GL.textureBinding Texture2D $= Just text
        textColourU <-
          GL.get $ GL.uniformLocation (textprogram renderer) "textColor"
        GL.uniform textColourU $= textColour renderer
        textBGColourU <-
          GL.get $ GL.uniformLocation (textprogram renderer) "textBGColor"
        GL.uniform textBGColourU $= textBGColour renderer
  in do renderCharacterQuad
          (textprogram renderer)
          (pMatrix renderer)
          (characterQuad renderer)
          charDrawFunc
          charVerts

renderCharacterBGQuad ::
     TextRenderer -> Character -> Int -> Int -> Font -> IO ()
renderCharacterBGQuad renderer (Character _ _ _ adv _ _ _) x y font =
  let x1 = fromIntegral x
      x2 = fromIntegral $ x + adv
      y1 = fromIntegral y
      y2 = fromIntegral $ y + fontHeight font
      charVerts = [x1, y1, x1, y2, x2, y1, x1, y2, x2, y2, x2, y1] :: [GLfloat]
      charDrawFunc = do
        textBGColourU <-
          GL.get $ GL.uniformLocation (bgprogram renderer) "textBGColor"
        GL.uniform textBGColourU $= textBGColour renderer
  in renderCharacterQuad
       (bgprogram renderer)
       (pMatrix renderer)
       (characterBGQuad renderer)
       charDrawFunc
       charVerts
