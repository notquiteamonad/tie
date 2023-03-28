{-# LANGUAGE TemplateHaskell #-}

module TIE.Data.Config
  ( Config,
    Current,
    configElmSrcFolder,
  )
where

import Control.Lens (_Wrapped)
import TIE.Data.Config.Internal
  ( Config,
    ConfigV1 (ConfigV1),
    Current,
    c1elmSrcFolder,
  )
import TIE.Data.Config.Internal.TH (makeConfigLenses)

makeConfigLenses 'ConfigV1
