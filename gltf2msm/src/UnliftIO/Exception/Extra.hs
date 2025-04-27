module UnliftIO.Exception.Extra where

import SOM.Prelude

import Control.Monad.IO.Class (MonadIO)

import Data.Either.Extra (maybeToEither)

import UnliftIO.Exception (Exception, fromEither)

fromMaybe ∷ (Exception ε, MonadIO μ) ⇒ ε → Maybe α → μ α
fromMaybe e a = fromEither (maybeToEither e a)
