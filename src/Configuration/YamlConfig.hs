{-# LANGUAGE OverloadedStrings #-}

module Configuration.YamlConfig where

import           Data.Yaml (FromJSON (..), (.!=), (.:), (.:?))
import qualified Data.Yaml as Y

data AppYAMLFontConfig = AppYAMLFontConfig
  { yamlFontFilePath :: Maybe FilePath
  , yamlFontSize     :: Maybe Int
  } deriving (Show)

instance FromJSON AppYAMLFontConfig where
  parseJSON (Y.Object v) =
    AppYAMLFontConfig <$> v .:? "filepath" <*> v .:? "size"
  parseJSON _ = fail "Expected Object for Config value"

data AppYAMLConfig = AppYAMLConfig
  { yamlScreenWidth       :: Maybe Int
  , yamlScreenHeight      :: Maybe Int
  , yamlFullscreenDisplay :: Maybe Int
  , yamlDebug             :: Bool
  , yamlFontCfg           :: AppYAMLFontConfig
  , yamlServerPort        :: Maybe Int
  }

instance FromJSON AppYAMLConfig where
  parseJSON (Y.Object v) =
    AppYAMLConfig <$> v .:? "screenwidth" <*> v .:? "screenheight" <*>
    v .:? "fullscreen" <*>
    v .:? "debug" .!= False <*>
    v .:? "font" .!= AppYAMLFontConfig Nothing Nothing <*>
    v .:? "serverPort"
  parseJSON _ = fail "Expected Object for Config value"

readConfigFile :: FilePath -> IO (Maybe AppYAMLConfig)
readConfigFile cfgFilePath = do
  yaml <- Y.decodeFileEither cfgFilePath
  case yaml of
    Left err         -> print err >> return Nothing
    Right yamlConfig -> return (Just yamlConfig)
