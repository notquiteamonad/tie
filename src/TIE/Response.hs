{-|
Module: TIE.Response

Defined the Response data type.
-}
module TIE.Response (Response(Ok, Failed), catSuccesses, catFailures, catResponses) where

import           Control.Monad (ap)

-- |A data type similar to `Either` for representing the result of an action.
data Response e a
  = Failed e -- ^Indicate that the operation failed, providing additional information
  | Ok a -- ^Indicate that the operation succeeded, providing its outcome
  deriving (Eq, Show)

instance Functor (Response e) where
  fmap _ (Failed e) = Failed e
  fmap f (Ok a)     = Ok (f a)

instance Applicative (Response e) where
  pure = Ok
  (<*>) = ap

instance Monad (Response e) where
  Failed e >>= _ = Failed e
  Ok a >>= f     = f a

{-|
  Concatenates `Ok` responses or returns the first `Failed` one.

  >>> catResponses [Ok 1, Ok 2, Ok 3]
  Ok [1, 2, 3]

  >>> catResponses [Ok 1, Failed 2, Ok 3, Failed 4]
  Failed 2
-}
catResponses :: [Response e a] -> Response e [a]
catResponses responses =
  case viaNonEmpty head $ catFailures responses of
    Just failure -> Failed failure
    Nothing      -> Ok $ fromOk <$> responses

{-|
  Returns only the values of the `Ok` responses.

  >>> catSuccesses [Ok 1, Failed 2, Ok 3]
  [1, 3]
-}
catSuccesses :: [Response e a] -> [a]
catSuccesses responses = do
  let successes = filter (not . isFailed) responses
  fromOk <$> successes

{-|
  Returns only the values of the `Failed` responses.

  >>> catSuccesses [Failed 1, Failed 2, Ok 3]
  [1, 2]
-}
catFailures :: [Response e a] -> [e]
catFailures responses = do
  let failures = filter isFailed responses
  fromFailed <$> failures

isFailed :: Response e a -> Bool
isFailed (Failed _) = True
isFailed _          = False

fromFailed :: Response e a -> e
fromFailed (Failed e) = e
fromFailed _          = error "Internal: fromFailed contained on an OK value"

fromOk :: Response e a -> a
fromOk (Ok a) = a
fromOk _      = error "Internal: fromOk contained on a Failed value"
