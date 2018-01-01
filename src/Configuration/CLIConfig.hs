module Configuration.CLIConfig where

import           Options.Applicative

import           Data.Semigroup      ((<>))

data AppCLIConfig = AppCLIConfig
  { cliFullscreenDisplay :: Maybe Int
  , cliDebug             :: Bool
  , cliConfigFilePath    :: Maybe FilePath
  }

cliopts :: ParserInfo AppCLIConfig
cliopts =
  info
    (cliparser <**> helper)
    (fullDesc <> progDesc "An ascii graphics display environment" <>
     header "Termviz")

cliparser :: Parser AppCLIConfig
cliparser =
  AppCLIConfig <$>
  optional
    (option
       auto
       (long "fullscreen" <> short 'f' <>
        help "Which screen to fullscreen the app to" <>
        metavar "INT")) <*>
  switch (long "debug" <> short 'd' <> help "Put termviz in debug mode") <*>
  optional
    (option
       str
       (long "config" <> short 'c' <> help "Path to a configuration YAML file" <>
        metavar "FilePath"))
