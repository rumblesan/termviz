{-# LANGUAGE TemplateHaskell #-}

module Configuration
  ( AppConfig(..)
  , screenWidth
  , screenHeight
  , fullscreenDisplay
  , debug
  , fontConfig
  , serverPort
  , AppFontConfig(..)
  , fontFilePath
  , fontSize
  , getConfig
  ) where

import           Control.Applicative      ((<|>))
import           Data.Maybe               (fromMaybe)
import           Options.Applicative      (execParser)

import           Lens.Simple

import           Configuration.CLIConfig  (AppCLIConfig (..), cliopts)
import           Configuration.YamlConfig (AppYAMLConfig (..),
                                           AppYAMLFontConfig (..),
                                           readConfigFile)

data AppFontConfig = AppFontConfig
  { _fontFilePath :: Maybe FilePath
  , _fontSize     :: Int
  } deriving (Show)

makeLenses ''AppFontConfig

data AppConfig = AppConfig
  { _screenWidth       :: Int
  , _screenHeight      :: Int
  , _fullscreenDisplay :: Maybe Int
  , _debug             :: Bool
  , _fontConfig        :: AppFontConfig
  , _serverPort        :: Int
  } deriving (Show)

makeLenses ''AppConfig

defaultConfigFile :: FilePath
defaultConfigFile = "./termviz.yaml"

defaultConfig :: AppConfig
defaultConfig =
  AppConfig
  { _screenWidth = 640
  , _screenHeight = 480
  , _fullscreenDisplay = Nothing
  , _debug = False
  , _fontConfig = AppFontConfig {_fontFilePath = Nothing, _fontSize = 36}
  , _serverPort = 3000
  }

getFontConfig :: AppFontConfig -> Maybe AppYAMLFontConfig -> AppFontConfig
getFontConfig defaultFontCfg yamlFontCfg =
  AppFontConfig
  { _fontFilePath =
      (yamlFontCfg >>= yamlFontFilePath) <|> _fontFilePath defaultFontCfg
  , _fontSize =
      fromMaybe (_fontSize defaultFontCfg) (yamlFontCfg >>= yamlFontSize)
  }

getConfig :: IO AppConfig
getConfig = do
  cliCfg <- execParser cliopts
  yamlCfgOpt <-
    readConfigFile $ fromMaybe defaultConfigFile $ cliConfigFilePath cliCfg
  return
    AppConfig
    { _screenWidth =
        fromMaybe (_screenWidth defaultConfig) (yamlCfgOpt >>= yamlScreenWidth)
    , _screenHeight =
        fromMaybe
          (_screenHeight defaultConfig)
          (yamlCfgOpt >>= yamlScreenHeight)
    , _fullscreenDisplay =
        cliFullscreenDisplay cliCfg <|> (yamlCfgOpt >>= yamlFullscreenDisplay)
    , _debug = cliDebug cliCfg || maybe False yamlDebug yamlCfgOpt
    , _fontConfig =
        getFontConfig (_fontConfig defaultConfig) (yamlFontCfg <$> yamlCfgOpt)
    , _serverPort =
        fromMaybe (_serverPort defaultConfig) (yamlCfgOpt >>= yamlServerPort)
    }
