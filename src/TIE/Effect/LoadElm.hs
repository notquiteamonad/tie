{-# LANGUAGE TemplateHaskell #-}

module TIE.Effect.LoadElm where

import Control.Lens
import Effectful (Eff, Effect, (:>))
import Effectful.Dispatch.Dynamic (interpret)
import Effectful.Reader.Static (Reader, ask, asks)
import Effectful.TH (makeEffect_)
import TIE.Data.Config
import TIE.Data.ElmFile
import TIE.Effect.Log

data LoadElm :: Effect where
  LoadMainElm :: LoadElm m RawElmFile

makeEffect_ ''LoadElm

-- | Load the Main.elm file from the elm source directory
loadMainElm :: LoadElm :> es => Eff es RawElmFile
runLoadElm :: (Log :> es, Reader Config :> es) => Eff (LoadElm ': es) a -> Eff es a
runLoadElm = interpret \_ LoadMainElm -> do
  basePath <- view configElmSrcFolder <$> ask
  -- log Debug $ "Reading config from " <> show configPath
  -- configFile <-
  --   decodeUtf8With lenientDecode
  --     <$> liftIO (readFileBS configPath)
  --     `catchIOError` fatalE "Could not read config file"
  undefined
