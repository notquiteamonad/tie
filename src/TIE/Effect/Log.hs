{-# LANGUAGE TemplateHaskell #-}

module TIE.Effect.Log where

import qualified Data.Text as T
import Data.Text.IO (hPutStr, hPutStrLn)
import Effectful (Eff, Effect, IOE, type (:>))
import Effectful.Dispatch.Dynamic (interpret)
import Effectful.TH (makeEffect_)
import System.Console.ANSI
  ( Color (Blue, Red, Yellow),
    ColorIntensity (Vivid),
    ConsoleLayer (Foreground),
    SGR (Reset, SetColor),
    hSetSGR,
  )

data Severity = Info | Warning | Error
  deriving stock (Show)

data Log :: Effect where
  Log :: Severity -> Text -> Log m ()

makeEffect_ ''Log

-- | Log the given message
log :: Log :> es => Severity -> Text -> Eff es ()
runLog :: forall es a. IOE :> es => Eff (Log ': es) a -> Eff es a
runLog = interpret \_ (Log severity msg) -> liftIO (putSeverity severity *> hPutStrLn stderr msg)
  where
    putSeverity :: Severity -> IO ()
    putSeverity severity = do
      hSetSGR stderr [SetColor Foreground Vivid $ severityColour severity]
      hPutStr stderr . T.justifyLeft 8 ' ' . T.toUpper $ show severity
      hSetSGR stderr [Reset]

severityColour :: Severity -> Color
severityColour = \case
  Info -> Blue
  Warning -> Yellow
  Error -> Red
