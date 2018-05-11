{-# LANGUAGE TemplateHaskell #-}

module AppState where

import           Generator
import           Lens.Simple

data AppState = AppState
  { _generatorFunc :: GeneratorFunc
  , _startTime     :: Float
  }

makeLenses ''AppState

makeAppState :: AppState
makeAppState = AppState {_generatorFunc = generate, _startTime = 0}
