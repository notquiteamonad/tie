module TIE.Elm.Types (elmTypeToTSType) where

import           Data.Text      (strip, stripPrefix)
import           TIE.TypeScript (PrimitiveName (PBoolean, PNull, PNumber, PString, PUnknown),
                                 TSType (TPrimitive))

elmTypeToTSType :: Text -> Maybe TSType
elmTypeToTSType t = case strip t of
  "String"        -> pure $ TPrimitive PString
  "Int"  -> pure $ TPrimitive PNumber
  "Float"  -> pure $ TPrimitive PNumber
  "Bool"          -> pure $ TPrimitive PBoolean
  "Json.Decode.Value"          -> pure $ TPrimitive PUnknown
  "Json.Encode.Value"          -> pure $ TPrimitive PUnknown
  "()"            -> pure $ TPrimitive PNull
  _               -> case stripPrefix "Maybe " t of
    Just mValue -> elmTypeToTSType mValue >>= \lhs -> Just $ lhs <> TPrimitive PNull
    Nothing     -> Nothing
