module TIE (interoperate) where

import Effectful (Eff, IOE, runEff)
import Effectful.FileSystem (FileSystem, runFileSystem)
import Effectful.Reader.Static (Reader, runReader)
import TIE.Data.Config (Config, Current)
import TIE.Effect.Fatal (Fatal, runFatal)
import TIE.Effect.LoadConfig (loadConfig, runLoadConfig)
import TIE.Effect.Log (Log, runLog)
import TIE.Effect.Path (Path, runPath)

interoperate :: IO ()
interoperate = runBaseEffects do
  error "todo: write this function"

runBaseEffects :: Eff '[Reader Config, Path, Fatal, Log, FileSystem, IOE] a -> IO a
runBaseEffects action = do
  let runInitialEffects = runEff . runFileSystem . runLog . runFatal . runPath
  cfg <- runInitialEffects . runLoadConfig $ loadConfig @Current
  runInitialEffects $ runReader cfg action
