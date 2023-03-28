{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module TIE.Data.Config.Internal where

import Control.Lens (makeLenses, makeWrapped)
import Dhall (FromDhall, ToDhall)
import Dhall.Deriving (Codec (Codec), DropPrefix, Field)
import TIE.Effect.Path (AbsolutePath (AbsolutePath))

data ConfigVersion = V1

type Current = 'V1

type Config = VersionedConfig Current

newtype VersionedConfig (version :: ConfigVersion) = VersionedConfig (ConfigForVersion version)

deriving newtype instance FromDhall (ConfigForVersion version) => FromDhall (VersionedConfig version)

type family ConfigForVersion (version :: ConfigVersion) where
  ConfigForVersion 'V1 = ConfigV1

newtype ConfigV1 = ConfigV1
  { _c1elmSrcFolder :: AbsolutePath
  }
  deriving stock (Generic)
  deriving (FromDhall, ToDhall) via Codec (Field (DropPrefix "c1")) ConfigV1

makeLenses ''ConfigV1
makeWrapped ''VersionedConfig
