module TIE.Elm.Expression (readNextExpression) where

import           Data.Text as T (drop, dropWhile, null, strip, take, takeWhile)

readNextExpression :: Text -> Maybe Text
readNextExpression t = do
  let stripped = T.strip t
  if T.take 1 stripped == "(" then
    if T.take 2 stripped == "()" then pure "()"
    else T.strip <$> bracketedExpression (T.drop 1 stripped)
  else viaNonEmpty head $ words stripped

bracketedExpression :: Text -> Maybe Text
bracketedExpression t =
  if T.null t then Nothing
  else pure $ go 0 t ""
  where
    go :: Int -> Text -> Text -> Text
    go innerBracketLevel remaining acc =
      if innerBracketLevel < 0 then acc
      else case T.take 1 remaining of
            "(" -> case T.take 2 remaining of
                    "()" -> go innerBracketLevel (T.drop 2 remaining) $ acc <> "()"
                    _ -> go (innerBracketLevel + 1) (T.drop 1 remaining) acc
            ")" -> go (innerBracketLevel - 1) (T.drop 1 remaining) acc
            _ -> go innerBracketLevel (T.dropWhile notABracket remaining) $ acc <> T.takeWhile notABracket remaining
              where notABracket c = c /= '(' && c /= ')'
