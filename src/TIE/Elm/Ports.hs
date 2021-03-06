{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase     #-}
{-# LANGUAGE NamedFieldPuns #-}

{-|
Module: TIE.Elm.Ports

Extracts port definitions from Elm code.
-}
module TIE.Elm.Ports (generatePortProperties) where

import qualified Data.Text          as T (dropEnd, dropWhileEnd, isInfixOf,
                                          isPrefixOf, null, strip, takeWhile)
import           Data.Text.IO       (hGetLine)
import           GHC.IO.Handle      (hIsEOF, hSetEncoding)
import           System.IO          (mkTextEncoding)
import           TIE.Elm.Expression (readNextExpression)
import           TIE.Elm.Types      (ElmType (..), NeededCustomType,
                                     elmTypeFromText, elmTypeToTSType,
                                     getCustomTypes)
import           TIE.Response       (Response (..), catResponses)
import           TIE.TypeScript     (Argument (Argument),
                                     ArgumentName (ArgumentName),
                                     Function (Function),
                                     FunctionName (FunctionName),
                                     Member (MFunction, MPropertyGroup),
                                     Members, PrimitiveName (PVoid),
                                     PropertyName (PropertyName),
                                     TSType (TFunction, TPrimitive))

data Port = Port
  { direction :: PortDirection
  , name      :: PortName
  , elmType   :: ElmType
  } deriving (Eq, Show)

data PortDirection = In | Out deriving (Eq, Show)

newtype PortName = PortName {unPortName :: Text} deriving (Eq, Show)

{-|
  Gets a PropertyGroup for each port containing the appropriate function for the
  `PortDirection`. Also gets a list of custom types which must be declared to make
  the declaration complete.
-}
generatePortProperties :: [FilePath] -> IO (Response Text (Members, [NeededCustomType]))
generatePortProperties paths = getPortsFromPaths paths >>= \case
  Ok ports ->
    pure $ pure
      ( toMember <$> ports
      , getCustomTypes (elmType <$> ports)
      )
  Failed e -> pure $ Failed e

getPortsFromPaths :: [FilePath] -> IO (Response Text [Port])
getPortsFromPaths paths = do
  portBodies <- concat <$> forM paths \path -> withFile path ReadMode \h -> getPortsFromModule h False []
  pure . catResponses $ parsePort <$> portBodies

toMember :: Port -> Member
toMember Port {direction, name, elmType} =
  let functionName = FunctionName $ if direction == In then "send" else "subscribe"
      args = case direction of
        In  -> [ Argument (ArgumentName "data") (elmTypeToTSType elmType) ]
        Out -> [ Argument (ArgumentName "callback") $ callbackTypeFor elmType ]
  in
  MPropertyGroup (PropertyName $ unPortName name) [ MFunction . Function functionName args $ TPrimitive PVoid ]

{-|
  Returns a TypeScript Function definition for the callback parameter of an Out (Elm -> TypeScript) port.
-}
callbackTypeFor :: ElmType -> TSType
callbackTypeFor t = TFunction [Argument (ArgumentName "data") $ elmTypeToTSType t] $ TPrimitive PVoid

{-|
  Given a file handle, returns a list of the port definitions in the module.

  It considers the beginning of a port definition to be the word "port" (except
  where this is part of the expression "port module")

  It considers a port definition to have ended when it reaches a blank line
  or the end of the file.
-}
getPortsFromModule :: Handle -> Bool -> [Text] -> IO [Text]
getPortsFromModule h knownPortModule acc = do
  enc <- mkTextEncoding "UTF-8//IGNORE"
  hSetEncoding h enc
  go acc
  where
    go acc' = do
      eof <- hIsEOF h
      if eof then pure acc'
      else do
        l <- hGetLine h
        if not knownPortModule then do
          case nonEmpty $ words l of
            Just w ->
              if head w == "module" then
                -- Not a port module
                pure []
              else if last w == "port" then do
                l' <- hGetLine h
                case nonEmpty $ words l' of
                  Just w' ->
                    if head w' == "module" then getPortsFromModule h True acc'
                    else pure []
                  Nothing ->
                    pure []
              else if take 2 (toList w) == ["port", "module"] then getPortsFromModule h True acc'
              else getPortsFromModule h knownPortModule acc'
            Nothing ->
              getPortsFromModule h knownPortModule acc'
        else
          if "port" `T.isPrefixOf` l then do
            next <- go' [l]
            getPortsFromModule h knownPortModule $ next : acc'
          else getPortsFromModule h knownPortModule acc'
          where go' :: [Text] -> IO Text
                go' acc'' = do
                  eof' <- hIsEOF h
                  if eof' then pure . mconcat $ reverse acc''
                  else do
                    l' <- hGetLine h
                    if T.null l' then pure . mconcat $ reverse acc''
                    else go' (l' : acc'')

parsePort :: Text -> Response Text Port
parsePort t =
  let w = drop 1 . words $ T.strip t
      identifier = mconcat $ take 1 w
      relevantTypeString = mconcat . intersperse " " $ drop 2 w
  in
    if T.null $ T.strip identifier then
      Failed $ "Could not parse incomplete port definition: " <> t
    else
      case readNextExpression $ "(" <> T.dropEnd 1 (T.dropWhileEnd (/= '-') relevantTypeString) <> ")" of
        Just relevantType ->
          let direction = if "->" `T.isInfixOf` relevantType then In else Out
              elmType = elmTypeFromText $ T.takeWhile (/= '-') relevantType
          in Port direction (PortName identifier) <$> elmType
        Nothing -> Failed $ "Could not parse type " <> relevantTypeString <> " in port definition " <> t
