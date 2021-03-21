{-# LANGUAGE BlockArguments #-}

{-|
Module: TIE.Elm.Types

Extract type expressions from Elm code and convert them to TypeScript types.
-}
module TIE.Elm.Types
  (ElmType(..)
  , NeededCustomType(..)
  , elmTypeFromText
  , elmTypeToTSType
  , findType
  , getCustomTypes
  ) where

import           Data.Text          (replace, strip, stripPrefix)
import qualified Data.Text          as T (drop, dropEnd, dropWhile, filter,
                                          find, head, null, reverse, take,
                                          takeWhile)
import           Data.Text.IO       (hGetLine)
import           GHC.IO.Handle      (hIsEOF, hSetEncoding)
import           System.IO          (mkTextEncoding)
import           TIE.Elm.Expression (readNextExpression)
import           TIE.Response       (Response (..), catFailures, catSuccesses)
import           TIE.TypeScript     (AliasName (AliasName), Exported (Exported),
                                     Interface (..),
                                     InterfaceName (InterfaceName),
                                     Member (MProperty, MPropertyGroup),
                                     Members,
                                     NamespaceMember (NMAlias, NMInterface),
                                     PrimitiveName (PBoolean, PNull, PNumber, PString, PUnknown, PVoid),
                                     PropertyName (PropertyName),
                                     ReferenceName (ReferenceName),
                                     TSType (TArray, TPrimitive, TReference))

{-|
  Represents a type alias or record name encountered when processing a type
  expression in Elm which TIE must find a defnition for to complete its type
  definitions.
-}
newtype NeededCustomType = NeededCustomType Text deriving (Eq, Ord, Show)

{-|
  Represents a type defined in Elm code.
-}
data ElmType
  = ElmPrimitiveType TSType
  -- ^ Refers to any Elm type which is represented in TypeScript by a primitive type
  | CustomType TSType NeededCustomType
  -- ^ A reference to a custom Elm type alias or record type
  | ElmArrayType ElmType
  -- ^ An Array or List type as defined in Elm
  | ETUnion ElmType ElmType
  -- ^ A union between two types, where either may be appropriate
  deriving (Show)

instance Eq ElmType where
  ElmPrimitiveType a == ElmPrimitiveType b = a == b
  CustomType a b == CustomType c d = a == c && b == d
  ElmArrayType a == ElmArrayType b = a == b
  ETUnion a b == ETUnion c d = (a == c && b == d) || (a == d && b == c)
  _ == _ = False

instance Semigroup ElmType where
  ElmPrimitiveType a <> ElmPrimitiveType b = ElmPrimitiveType $ a <> b
  a <> b                                   = a `ETUnion` b

{-|
  Reads an Elm type expression to extract the type specified and converts it to
  an `ElmType`.

  Note: This will only extract the first type given as it uses `readNextExpression`.

  >>> elmTypeFromText "Maybe String"
  Ok (ElmPrimitiveType (TPrimitive PString <> TPrimitive PVoid <> TPrimitive PNull ))
-}
elmTypeFromText :: Text -> Response Text ElmType
elmTypeFromText t = case readNextExpression $ "(" <> t <> ")" of
  Just "String"            -> Ok . ElmPrimitiveType $ TPrimitive PString
  Just "Int"               -> Ok . ElmPrimitiveType $ TPrimitive PNumber
  Just "Float"             -> Ok . ElmPrimitiveType $ TPrimitive PNumber
  Just "Bool"              -> Ok . ElmPrimitiveType $ TPrimitive PBoolean
  Just "Json.Decode.Value" -> Ok . ElmPrimitiveType $ TPrimitive PUnknown
  Just "Json.Encode.Value" -> Ok . ElmPrimitiveType $ TPrimitive PUnknown
  Just "Value" -> Ok . ElmPrimitiveType $ TPrimitive PUnknown
  Just "()"                -> Ok . ElmPrimitiveType $ TPrimitive PNull
  Just expr                   ->
    case stripPrefix "Maybe " expr of
      Just mValue -> case elmTypeFromText mValue of
        Ok (ElmPrimitiveType p) -> Ok . ElmPrimitiveType $ p <> TPrimitive PVoid <> TPrimitive PNull
        Ok (CustomType c nct)   -> Ok $ CustomType (c <> TPrimitive PVoid <> TPrimitive PNull) nct
        Ok (ElmArrayType a) -> Ok $ ElmArrayType a <> ElmPrimitiveType (TPrimitive PVoid <> TPrimitive PNull)
        Ok (t1 `ETUnion ` t2) -> Ok $ t1 <> t2 <> ElmPrimitiveType (TPrimitive PVoid <> TPrimitive PNull)
        Failed f -> Failed f
      Nothing ->
        case stripPrefix "List " expr of
          Just lValue -> handleListValue lValue
          Nothing ->
            case stripPrefix "Array " expr of
              Just aValue -> handleListValue aValue
              Nothing     -> Ok . CustomType (TReference (ReferenceName qualifiedName)) $ NeededCustomType qualifiedName
                              where qualifiedName = "Elm.Main." <> expr
          where handleListValue v = ElmArrayType <$> elmTypeFromText v
  Nothing ->
    Failed $ "Could not parse type " <> strip t

{-|
  Converts a given `ElmType` to its equivalent `TSType`.
-}
elmTypeToTSType :: ElmType -> TSType
elmTypeToTSType (ElmPrimitiveType t) = t
elmTypeToTSType (CustomType t _)     = t
elmTypeToTSType (ElmArrayType t)     = TArray $ elmTypeToTSType t
elmTypeToTSType (t `ETUnion` u)      = elmTypeToTSType t <> elmTypeToTSType u

{-|
  Recursively searches the list of `ElmType` values to get a list of `NeededCustomType`s
  required to ensure all types referenced are defined.
-}
getCustomTypes :: [ElmType] -> [NeededCustomType]
getCustomTypes xs = go xs []
  where
    go :: [ElmType] -> [NeededCustomType] -> [NeededCustomType]
    go [] acc = acc
    go (t:ts) acc =
      case t of
        ElmPrimitiveType _ -> go ts acc
        CustomType _ nct   -> go ts $ nct : acc
        ElmArrayType a     -> go (a : ts) acc
        t1 `ETUnion` t2    -> go (t1 : t2 : ts) acc

{-|
  Searches within the list of files given for a definition for the specified
  `NeededCustomType`.

  This may return additional `NeededCustomType` values if the definition found is
  an alias or record which references other non-primitive types.
-}
findType :: [FilePath] -> NeededCustomType -> IO (Response Text (NamespaceMember, [NeededCustomType]))
findType paths nct@(NeededCustomType c) = do
  case nonEmpty paths of
    Just paths' ->
        maybe (findType (tail paths') nct) (pure . parseCustomType recordName) =<<
          withFile (head paths') ReadMode \h -> findCustomTypeInFile h recordName 0 []
    Nothing ->
      pure . Failed $ "Could not find custom type "
      <> recordName
      <> ". Is it a type alias defined within the directory specified?"
    where recordName = replace "Elm.Main." "" c

{-|
  Searches within a file for the definition of the requested type alias or record.
-}
findCustomTypeInFile ::
  Handle -- ^The file handle to read from
  -> Text -- ^The identifier to search for
  -> Int -- ^The current level of nesting. Specify 0 at the top level.
  -> [Text] -- ^Accumulator. Specify [] at the top level.
  -> IO (Maybe Text) -- ^A `Just` value of the complete definition of the result, if found.
findCustomTypeInFile h identifierToFind nestingLevel acc = do
  enc <- mkTextEncoding "UTF-8//IGNORE"
  hSetEncoding h enc
  go identifierToFind nestingLevel acc
    where
      go identifierToFind' nestingLevel' acc' = do
        eof <- hIsEOF h
        if eof then
          if null acc' then pure Nothing
          else wrapUp acc'
        else do
          l <- hGetLine h
          if null acc' then
            if ["type", "alias", identifierToFind'] == (take 3 . words . strip) l then
              go identifierToFind' (nextNestingLevel l - 1) $ l : acc'
            else go identifierToFind' nestingLevel' acc'
          else do
            let nnl = nextNestingLevel l
            if nnl < 0 then
              let finalLine = case T.find (== '}') l of
                                Just _ ->
                                  T.reverse . T.dropWhile (/= '}') $ T.reverse l
                                Nothing ->
                                  l
              in wrapUp $ finalLine  : acc'
            else
              go identifierToFind' nnl $ l : acc'
        where wrapUp = pure . pure . mconcat . intersperse "\n" . reverse
              nextNestingLevel t = nestingLevel' + length (filter (== '{') s) - length (filter (== '}') s)
                where s = toString t

parseCustomType :: Text -> Text -> Response Text (NamespaceMember, [NeededCustomType])
parseCustomType name elmCode =
  if T.null $ T.filter (\c -> c == '{' || c == '}') elmCode then
    case elmTypeFromText . T.drop 1 $ T.dropWhile (/= '=') elmCode of
      Ok elmType ->
        pure (NMAlias (AliasName name) (elmTypeToTSType elmType), getCustomTypes [elmType])
      Failed e -> Failed e
  else first (NMInterface . Interface Exported (InterfaceName name)) <$> recordMembers (removeOneBraceLayer elmCode)

removeOneBraceLayer :: Text -> Text
removeOneBraceLayer s =
          T.dropEnd 1 . T.drop 1 . T.reverse . T.dropWhile (/= '}') . T.reverse $ T.dropWhile (/= '{') s

recordMembers :: Text -> Response Text (Members, [NeededCustomType])
recordMembers t = do
  let responses = recordMember <$> splitIntoMembers 0 [] "" t
  let failures = catFailures responses
  if null failures then
    Ok . foldr (\(member, ncts) (accMembers, accNcts) ->
        (member : accMembers, accNcts <> ncts)
      ) ([], []) $ catSuccesses responses
  else
    Failed . mconcat $ intersperse "\n" failures

recordMember :: Text -> Response Text (Member, [NeededCustomType])
recordMember m =
  let identifier = strip $ T.takeWhile (/= ':') m
      rest = T.drop 1 $ T.dropWhile (/= ':') m
  in case removeOneBraceLayer m of
        "" -> case elmTypeFromText rest of
            Ok elmType ->
              Ok (MProperty (PropertyName identifier) (elmTypeToTSType elmType), getCustomTypes [elmType])
            Failed e -> Failed e
        _ -> first (MPropertyGroup (PropertyName identifier)) <$> recordMembers (removeOneBraceLayer rest)

splitIntoMembers :: Int -> [Text] -> Text -> Text -> [Text]
splitIntoMembers innerLevel acc buf t =
  if T.null t then
    reverse $ buf : acc
  else
    let next = T.head t
        endOfMember nextLevel = splitIntoMembers nextLevel (buf : acc) "" $ T.drop 1 t
        endOfChar nextLevel = splitIntoMembers nextLevel acc (buf <> T.take 1 t) $ T.drop 1 t
        skipToNextLevelChange nextLevel = splitIntoMembers nextLevel acc
          (buf <> T.takeWhile notALevelChange t) $ T.dropWhile notALevelChange t
          where notALevelChange c = c /= '{' && c /= '}'
    in
      if innerLevel == 0 && next == ',' then
        -- End of member, move on to next one
        endOfMember 0
      else if next == '{' then
        -- Going down a level
        skipToNextLevelChange (innerLevel + 1)
      else if next == '}' then
        -- Going up a level
        if innerLevel == 1 then endOfChar (innerLevel - 1)
        else skipToNextLevelChange (innerLevel - 1)
      else
        endOfChar innerLevel
