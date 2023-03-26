{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

module TIE.Effect.Path where

import Control.Monad.Catch (catchIOError)
import Dhall (FromDhall, ToDhall)
import Effectful (Eff, Effect, type (:>))
import Effectful.Dispatch.Dynamic (interpret)
import Effectful.FileSystem (FileSystem)
import qualified Effectful.FileSystem as FS
import Effectful.TH (makeEffect_)
import TIE.Effect.Fatal (Fatal, fatalE)

-- | Represents a path which is absolute
newtype AbsolutePath = AbsolutePath {unAbsolutePath :: FilePath}
  deriving newtype (FromDhall, ToDhall)

data Path :: Effect where
  MakeAbsolute :: FilePath -> Path m AbsolutePath

makeEffect_ ''Path

-- | Get an `AbsolutePath` from any `FilePath`
makeAbsolute :: Path :> es => FilePath -> Eff es AbsolutePath
runPath :: (Fatal :> es, FileSystem :> es) => Eff (Path ': es) a -> Eff es a
runPath = interpret \_ (MakeAbsolute p) ->
  AbsolutePath
    <$> FS.makeAbsolute p
    `catchIOError` fatalE ("Could not make path " <> show p <> " absolute")
