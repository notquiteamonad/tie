{-# LANGUAGE TemplateHaskell #-}

module TIE.Effect.Fatal where

import Effectful (Eff, Effect, IOE, type (:>))
import Effectful.Dispatch.Dynamic (interpret)
import Effectful.TH (makeEffect_)
import TIE.Effect.Log (Log, Severity (Error), log)

data Fatal :: Effect where
  FatalE :: Exception e => Text -> e -> Fatal m a

makeEffect_ ''Fatal

-- | Logs the message, a colon and space, then the exception. Then, exits
-- the program with status 1.
fatalE :: (Exception e, Fatal :> es) => Text -> e -> Eff es a
runFatal :: forall es a. (IOE :> es, Log :> es) => Eff (Fatal ': es) a -> Eff es a
runFatal = interpret \_ (FatalE msg e) ->
  log Error (msg <> ": " <> toText (displayException e)) *> liftIO exitFailure
