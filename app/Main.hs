module Main where

import SOM.Prelude

import SOM.Window (Window, shouldClose, update)
import SOM.Window qualified as Window (create)

import Control.Arrow (arr)
import Control.Monad.IO.Class (MonadIO)

import Data.IORef (IORef)
import Data.IORef.Lifted (newIORef, readIORef, writeIORef)
import Data.Time.Clock (UTCTime, diffUTCTime)
import Data.Time.Clock.Lifted (getCurrentTime)

import FRP.Yampa (reactimate)

main ∷ IO ()
main = do
  w ← Window.create "Sword of Moonlight λ" (width, height)
  t ← newIORef ⤛ getCurrentTime

  reactimate (pure ()) (sense w t) (actuate w) (arr id)

  where width  = 800
        height = 600

sense ∷ MonadIO μ ⇒ Window → IORef UTCTime → Bool → μ (Double, Maybe α)
sense w r _ = do

  update w

  t ← getCurrentTime
  t₀ ← readIORef r
  writeIORef r t

  let δt = realToFrac (diffUTCTime t t₀)

  pure (δt, Nothing)

actuate ∷ MonadIO μ ⇒ Window → Bool → α → μ Bool
actuate w _ _ = shouldClose w
