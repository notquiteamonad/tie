module TIE.Elm.Expression (readNextExpression) where

import           Data.Text                   as T (drop, strip, take)
import           TIE.Elm.Internal.Expression (bracketedExpression)

readNextExpression :: Text -> Maybe Text
readNextExpression t = do
  let stripped = T.strip t
  if T.take 1 stripped == "(" then
    if T.take 2 stripped == "()" then pure "()"
    else T.strip <$> bracketedExpression (T.drop 1 stripped)
  else viaNonEmpty head $ words stripped
