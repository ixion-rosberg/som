module Data.Time.Clock.Lifted (getCurrentTime) where

import Control.Monad.IO.Class (MonadIO, liftIO)

import Data.Time.Clock (UTCTime)
import Data.Time.Clock qualified as Unlifted (getCurrentTime)

getCurrentTime ∷ MonadIO μ ⇒ μ UTCTime
getCurrentTime = liftIO Unlifted.getCurrentTime
