{-# LANGUAGE TemplateHaskell #-}

module TestUtils (testDataDir) where

import           Language.Haskell.TH (litE, runIO, stringL)
import           System.Directory    (getCurrentDirectory)
import           System.FilePath     ((</>))

testDataDir :: FilePath
testDataDir = $(runIO getCurrentDirectory >>= \dir -> litE . stringL $ dir </> "test" </> "test-data")
