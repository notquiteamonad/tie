module TIE.Response (Response(Ok, Failed), catSuccesses, catFailures, catResponses) where

import           Control.Monad (ap)

data Response e a = Failed e | Ok a deriving (Eq, Show)

instance Functor (Response e) where
  fmap _ (Failed e) = Failed e
  fmap f (Ok a)     = Ok (f a)

instance Applicative (Response e) where
  pure = Ok
  (<*>) = ap

instance Monad (Response e) where
  Failed e >>= _ = Failed e
  Ok a >>= f     = f a

catResponses :: [Response e a] -> Response e [a]
catResponses responses =
  case viaNonEmpty head $ catFailures responses of
    Just failure -> Failed failure
    Nothing      -> Ok $ fromOk <$> responses

catSuccesses :: [Response e a] -> [a]
catSuccesses responses = do
  let successes = filter (not . isFailed) responses
  fromOk <$> successes

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
