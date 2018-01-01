{-# LANGUAGE TemplateHaskell #-}

module AppState where

import           Lens.Simple

data AppState = AppState
  { _programText :: String
  , _startTime   :: Float
  } deriving (Show)

makeLenses ''AppState

makeAppState :: AppState
makeAppState = AppState {_programText = "this is a test", _startTime = 0}
