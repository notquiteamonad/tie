module TIE.Response (Response(Ok, Failed), catSuccessess, catFailures) where

data Response e a = Failed e | Ok a deriving (Eq, Show)

instance Functor (Response e) where
  fmap _ (Failed e) = Failed e
  fmap f (Ok a)     = Ok (f a)

instance Applicative (Response e) where
  pure = Ok
  Failed e <*> _ = Failed e
  Ok a <*> b     = fmap a b

instance Monad (Response e) where
  Failed e >>= _ = Failed e
  Ok a >>= f     = f a

catSuccessess :: [Response e a] -> [a]
catSuccessess responses = do
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