{-# LANGUAGE LambdaCase #-}

{-|
Module: TIE.Config

Configuration for the TIE utility.
-}
module TIE.Config (getConfig, Config(cfgOverrides), Override(Override, ovrPath, ovrType)) where

import qualified Data.Text       as T
import           System.IO.Error (catchIOError)
import           TIE.Response    (Response (..))
import           TIE.TypeScript  (TSType (TLiteral))
import           Toml            (TomlCodec, (.=))
import qualified Toml

{-|
  Returns the config specified in .TIE.toml or the default config if that file is nor present.

  If the default config was returned due to the file not being present or not containing any
  config properties, a warning is also emitted in the `Ok` response.

  If the confg file can't be parsed or has invalid config in it, a `Failed` response is produced
  with a user-readable message.
-}
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


-- |Configuration data for the TIE utility
newtype Config = Config
  { cfgOverrides :: [Override] -- ^the `Override`s to be applied to the generated type definitions
  } deriving (Eq, Show)

defaultConfig :: Config
defaultConfig = Config []

configCodec :: TomlCodec Config
configCodec = Config . sortNub
  <$> (Toml.list overrideCodec "override" .= cfgOverrides)

-- |A declaration that a generated type should be overriden with a custom type
data Override = Override
  { ovrPath :: NonEmpty Text
    -- ^The path to the item to be overriden. Should have length 1 for an alias or 2 for an interface's property.
  , ovrType :: TSType
    -- ^The user-defined replacement type to be used in the type definition file instead of the TIE-generated type.
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
