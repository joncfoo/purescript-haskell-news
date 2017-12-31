{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE NoImplicitPrelude  #-}
{-# OPTIONS_GHC -fno-cse #-}

module Args
  (Args(..), evalArgs)
where

import Protolude

import Control.Logger.Simple  (LogLevel (..), setLogLevel)
import Data.String            (String)
import System.Console.CmdArgs (Data, Verbosity (..), cmdArgs, explicit, getVerbosity, help, name, program, summary, typ,
                               typFile, verbosity, (&=))
import System.Directory       (XdgDirectory (XdgConfig), createDirectoryIfMissing, getXdgDirectory)
import System.FilePath        ((</>))

data Args =
  Args
  { db   :: String
  , host :: String
  , port :: Int
  }
  deriving (Show, Data, Typeable)

sampleArgs :: IO Args
sampleArgs = do
  configDir <- getXdgDirectory XdgConfig "silly-planet"
  createDirectoryIfMissing True configDir
  let defDb = configDir </> "data.db"
      defInterface = "127.0.0.1"
      defPort = 3210
  pure $
    Args
    { db =
        defDb
        &= explicit &= name "db"
        &= help ("Location of database\n  Default: " <> defDb)
        &= typFile
    , host =
        defInterface
        &= explicit &= name "host"
        &= help ("Network host on which the web server will listen\n  Default: " <> defInterface)
        &= typ "HOST"
    , port =
        defPort
        &= explicit &= name "port"
        &= help ("Network port on which the web server will listen\n  Default: " <> show defPort)
        &= typ "INT"
    } &= program "silly-planet"
      &= help "Simple Feed Aggregator"
      &= summary "silly-planet v0.0.0"
      &= verbosity

evalArgs :: IO Args
evalArgs = do
  args <- cmdArgs =<< sampleArgs
  verbosity' <- getVerbosity
  setLogLevel $
    case verbosity' of
      Quiet  -> LogError
      Normal -> LogWarn
      Loud   -> LogDebug
  pure args
