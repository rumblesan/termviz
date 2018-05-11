module Main where

import           Control.Concurrent.STM
import           System.Exit            (exitSuccess)

import qualified Graphics.UI.GLFW       as GLFW
import           Lens.Simple            ((^.))

import           AppState               (AppState (..), generatorFunc,
                                         makeAppState)
import           Configuration
import           Gfx.TextRendering
import           Gfx.Windowing          (setupWindow)

main :: IO ()
main = getConfig >>= app

app :: AppConfig -> IO ()
app cfg = do
  asTVar <- newTVarIO (makeAppState)
  trenderTVar <- newEmptyTMVarIO
  let initCB = initApp trenderTVar cfg
  let resizeCB = resize trenderTVar
  let displayCB = display asTVar trenderTVar
  setupWindow
    (cfg ^. screenWidth)
    (cfg ^. screenHeight)
    (cfg ^. fullscreenDisplay)
    initCB
    resizeCB
    displayCB
  exitSuccess

initApp :: TMVar TextRenderer -> AppConfig -> Int -> Int -> IO ()
initApp trenderVar cfg width height =
  let front = 0.1
      back = 100
  in do textRenderer <-
          createTextRenderer
            front
            back
            width
            height
            (cfg ^. fontConfig . fontFilePath)
            (cfg ^. fontConfig . fontSize)
        atomically $ putTMVar trenderVar textRenderer

resize :: TMVar TextRenderer -> GLFW.WindowSizeCallback
resize trenderVar window newWidth newHeight = do
  (fbWidth, fbHeight) <- GLFW.getFramebufferSize window
  textRenderer <- atomically $ readTMVar trenderVar
  newTrender <- resizeTextRendererScreen 0.1 100 fbWidth fbHeight textRenderer
  atomically $ do putTMVar trenderVar newTrender

display :: TVar AppState -> TMVar TextRenderer -> Double -> IO ()
display appState trenderVar time = do
  as <- readTVarIO appState
  textRenderer <- atomically $ readTMVar trenderVar
  renderText 50 50 textRenderer (as ^. generatorFunc)
  renderTextbuffer textRenderer
