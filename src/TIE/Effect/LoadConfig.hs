{-# LANGUAGE TemplateHaskell #-}

module TIE.Effect.LoadConfig where

import Control.Monad.Catch (catchAll, catchIOError)
import Dhall (FromDhall, auto, input)
import Effectful (Eff, Effect, IOE, type (:>))
import Effectful.Dispatch.Dynamic (interpret)
import Effectful.TH (makeEffect_)
import TIE.Data.Config (ConfigForVersion, VersionedConfig)
import TIE.Effect.Fatal (Fatal, fatalE)
import TIE.Effect.Log (Log, Severity (Info), log)
import TIE.Effect.Path
  ( AbsolutePath (unAbsolutePath),
    Path,
    makeAbsolute,
  )

data LoadConfig :: Effect where
  LoadConfig :: FromDhall (ConfigForVersion version) => LoadConfig m (VersionedConfig version)

makeEffect_ ''LoadConfig

-- | Load the config
loadConfig :: (FromDhall (ConfigForVersion version), LoadConfig :> es) => Eff es (VersionedConfig version)
runLoadConfig :: (Fatal :> es, IOE :> es, Log :> es, Path :> es) => Eff (LoadConfig ': es) a -> Eff es a
runLoadConfig = interpret \_ LoadConfig -> do
  configPath <- unAbsolutePath <$> makeAbsolute ".TIE.dhall"
  log Info $ "Reading config from " <> show configPath
  configFile <-
    decodeUtf8With lenientDecode
      <$> liftIO (readFileBS configPath)
      `catchIOError` fatalE "Could not read config file"
  liftIO (input auto configFile) `catchAll` fatalE "Could not parse config file"
