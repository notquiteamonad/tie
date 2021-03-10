module TIE.Elm.Internal.Expression (bracketedExpression) where

import           Data.Text as T (drop, dropWhile, filter, length, null, take,
                                 takeWhile)

-- NOTE: This expects a mismatched number of brackets, specifically
-- one more closing than opening.
bracketedExpression :: Text -> Maybe Text
bracketedExpression t =
  if T.length (T.filter (== ')') t) == 1 + T.length (T.filter (== '(') t) then
    if T.null t then Nothing
    else pure $ go 0 t ""
  else
    -- would be an infinite loop
    Nothing
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
