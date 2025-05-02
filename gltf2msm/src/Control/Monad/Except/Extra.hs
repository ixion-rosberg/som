module Control.Monad.Except.Extra where

import SOM.Prelude

import Control.Monad.Except (MonadError, liftEither)

import Data.Either.Extra (maybeToEither)

liftMaybe ∷ MonadError ε μ ⇒ ε → Maybe α → μ α
liftMaybe e a = liftEither (maybeToEither e a)
