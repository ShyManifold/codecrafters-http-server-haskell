{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module ConfigurationParser ( ServerOptions (..),
                             getServerOptions) where

import Options.Applicative

data ServerOptions = ServerOptions {serverDirectory::FilePath}

parseServerOptions :: Parser ServerOptions
parseServerOptions =
    ServerOptions <$>
        strOption
            (  long "directory"
            <> short 'd'
            <> value "./"
            <> help "Absolute path to the static files directory")

opts = info (parseServerOptions <**> helper)
      ( fullDesc
     <> progDesc "Runs the http server"
     <> header "directory - the path to the directory with the static files " )


getServerOptions :: IO ServerOptions
getServerOptions = execParser opts