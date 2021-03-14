{-|
Module: TIE.Elm.Expression

Extract complete expressions from Elm code.
-}
module TIE.Elm.Expression (readNextExpression) where

import           Data.Text                   as T (drop, strip, take)
import           TIE.Elm.Internal.Expression (bracketedExpression)

{-|
  Gets the next complete expression from an Elm type signature.

  >>> readNextExpression "() a b"
  Just "()"

  >>> readNextExpression "String a b"
  Just "String"

  >>> readNextExpression "(String) a b"
  Just "String"

  >>> readNextExpression "(Maybe String) a b)"
  Just "Maybe String"
-}
readNextExpression :: Text -> Maybe Text
readNextExpression t = do
  let stripped = T.strip t
  if T.take 1 stripped == "(" then
    if T.take 2 stripped == "()" then pure "()"
    else T.strip <$> bracketedExpression (T.drop 1 stripped)
  else viaNonEmpty head $ words stripped
