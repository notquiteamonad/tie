{-# LANGUAGE BlockArguments #-}

module TIE.Elm.Types (ElmType(ElmPrimitiveType, CustomType), NeededCustomType(NeededCustomType), elmTypeFromText, elmTypeToTSType, findType) where

import           Data.Text      (replace, strip, stripPrefix)
import qualified Data.Text      as T (drop, dropEnd, dropWhile, head, null,
                                      reverse, take, takeWhile)
import           Data.Text.IO   (hGetLine)
import           GHC.IO.Handle  (hIsEOF)
import           TIE.TypeScript (Exported (Exported), Interface (..),
                                 InterfaceName (InterfaceName),
                                 Member (MProperty, MPropertyGroup), Members,
                                 PrimitiveName (PBoolean, PNull, PNumber, PString, PUnknown),
                                 PropertyName (PropertyName),
                                 TSType (TInterface, TPrimitive))

data ElmType
  = ElmPrimitiveType TSType
  | CustomType TSType NeededCustomType
  deriving (Eq, Show)

newtype NeededCustomType = NeededCustomType Text deriving (Eq, Ord, Show)

elmTypeFromText :: Text -> ElmType
elmTypeFromText t = case strip t of
  "String"            -> ElmPrimitiveType $ TPrimitive PString
  "Int"               -> ElmPrimitiveType $ TPrimitive PNumber
  "Float"             -> ElmPrimitiveType $ TPrimitive PNumber
  "Bool"              -> ElmPrimitiveType $ TPrimitive PBoolean
  "Json.Decode.Value" -> ElmPrimitiveType $ TPrimitive PUnknown
  "Json.Encode.Value" -> ElmPrimitiveType $ TPrimitive PUnknown
  "()"                -> ElmPrimitiveType $ TPrimitive PNull
  _                   ->
    case stripPrefix "Maybe " (strip t) of
      Just mValue -> case elmTypeFromText mValue of
        ElmPrimitiveType p -> ElmPrimitiveType $ p <> TPrimitive PNull
        CustomType c nct   -> CustomType (c <> TPrimitive PNull) nct
      Nothing     -> CustomType (TInterface (InterfaceName qualifiedName)) $ NeededCustomType qualifiedName
                      where qualifiedName = "Elm.Main." <> strip t

elmTypeToTSType :: ElmType -> TSType
elmTypeToTSType (ElmPrimitiveType t) = t
elmTypeToTSType (CustomType t _)     = t

findType :: [FilePath] -> NeededCustomType -> IO Interface
findType paths nct@(NeededCustomType c) = do
  case nonEmpty paths of
    Just paths' ->
        maybe (findType (tail paths') nct) (pure . parseRecordType recordName) =<<
          withFile (head paths') ReadMode \h -> findRecordTypeInFile h recordName 0 []
    Nothing -> error $ "Could not find custom type " <> recordName <> ". Is it a record type defined within your app?"
    where recordName = replace "Elm.Main." "" c

findRecordTypeInFile :: Handle -> Text -> Int -> [Text] -> IO (Maybe Text)
findRecordTypeInFile h recordName nestingLevel acc = do
  eof <- hIsEOF h
  if eof then
    if null acc then pure Nothing
    else wrapUp acc
  else do
    l <- hGetLine h
    if null acc then
      if ["type", "alias", recordName] == (take 3 . words . strip) l then findRecordTypeInFile h recordName (nextNestingLevel l - 1) $ l : acc
      else findRecordTypeInFile h recordName nestingLevel acc
    else do
      let nnl = nextNestingLevel l
      if nnl < 0 then
        wrapUp $ (T.reverse . T.dropWhile (/= '}') $ T.reverse l) : acc
      else
        findRecordTypeInFile h recordName nnl $ l : acc
  where wrapUp = pure . pure . mconcat . intersperse "\n" . reverse
        nextNestingLevel t = nestingLevel + length (filter (== '{') s) - length (filter (== '}') s)
          where s = toString t

parseRecordType :: Text -> Text -> Interface
parseRecordType name elmCode = Interface Exported (InterfaceName name) . recordMembers $ removeOneBraceLayer elmCode

removeOneBraceLayer :: Text -> Text
removeOneBraceLayer s =
          T.dropEnd 1 . T.drop 1 . T.reverse . T.dropWhile (/= '}') . T.reverse $ T.dropWhile (/= '{') s

recordMembers :: Text -> Members
recordMembers t = recordMember <$> splitIntoMembers 0 [] "" t

recordMember :: Text -> Member
recordMember m =
  let identifier = T.takeWhile (/= ':') m
      rest = T.drop 1 $ T.dropWhile (/= ':') m
  in case removeOneBraceLayer m of
        "" -> MProperty (PropertyName identifier) . elmTypeToTSType $ elmTypeFromText rest
        _  -> MPropertyGroup (PropertyName identifier) . recordMembers $ removeOneBraceLayer rest

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
