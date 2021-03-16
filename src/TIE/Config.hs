{-# LANGUAGE LambdaCase #-}

module TIE.Config (getConfig, Config(cnfOverrides), Override(ovrPath, ovrType)) where

import qualified Data.Text       as T
import           System.IO.Error (catchIOError)
import           TIE.Response    (Response (..))
import           TIE.TypeScript  (TSType (TLiteral))
import           Toml            (TomlCodec, (.=))
import qualified Toml

getConfig :: IO (Response Text (Config, Maybe Text))
getConfig =
  catchIOError
    (Toml.decodeFileEither configCodec ".TIE.toml")
    (const . pure $ Right defaultConfig) >>= \case
      Left errs ->
        pure . Failed . mconcat . intersperse "\n" . ordNub $ mapError <$> errs
      Right config ->
        pure $ Ok
          ( config
          , if config == defaultConfig then Just "Using default config as no .TIE.toml was found."
            else Nothing
          )
  where
    mapError :: Toml.TomlDecodeError -> Text
    mapError = \case
      (Toml.KeyNotFound (Toml.Key ("override" :| ["property"]))) ->
        "One or more of the overrides in your TIE config is missing a value for \"property\"."
      (Toml.KeyNotFound (Toml.Key ("override" :| ["type"]))) ->
        "One or more of the overrides in your TIE config is missing a value for \"type\"."
      (Toml.ParseError (Toml.TomlParseError e)) ->
        "Error parsing your TIE config at " <> mconcat (intersperse ":" . take 2 $ T.splitOn ":" e)
      (Toml.NotExactDecode extra) ->
        "The following section of your TIE config was not parsed: \n" <> Toml.pretty extra
      e ->
        "An error occurred when parsing your TIE config:\n" <> Toml.prettyTomlDecodeError e


newtype Config = Config
  { cnfOverrides :: [Override]
  } deriving (Eq, Show)

defaultConfig :: Config
defaultConfig = Config []

configCodec :: TomlCodec Config
configCodec = Config . sortNub
  <$> (Toml.list overrideCodec "override" .= cnfOverrides)

data Override = Override
  { ovrPath :: NonEmpty Text
  , ovrType :: TSType
  } deriving (Show)

instance Eq Override where
  a == b = ovrPath a == ovrPath b

instance Ord Override where
  a <= b = ovrPath a <= ovrPath b

overrideCodec :: TomlCodec Override
overrideCodec = Override
  <$> Toml.dimap (mconcat . intersperse "." . toList) splitPath  (Toml.text "property") .= ovrPath
  <*> (Toml.dimap (\(TLiteral l) -> l) TLiteral  (Toml.text "type") .= ovrType)

splitPath :: Text -> NonEmpty Text
splitPath = fromMaybe ("" :| []) . nonEmpty . T.splitOn "."
