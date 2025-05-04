module Main where

import SOM.Prelude

import SOM.Controller (Button (..), ButtonName (..), Controller (..), Dpad (..), controller)
import SOM.Viewport (Viewport, clear)
import SOM.Viewport qualified as Viewport (create)
import SOM.Window (Window, shouldClose, update, inputs)
import SOM.Window qualified as Window (create)

import Control.Monad.IO.Class (MonadIO, liftIO)

import Data.Map.Strict (Map, (!?))
import Data.Time.Clock (UTCTime, diffUTCTime)
import Data.Time.Clock.Lifted (getCurrentTime)

import FRP.Yampa (Event (..), catEvents, maybeToEvent, reactimate, returnA)

import Graphics.UI.GLFW (Key (..), KeyState (..))

import UnliftIO.IORef (IORef, newIORef, readIORef, writeIORef)

main ∷ IO ()
main = do
  w ← Window.create "Sword of Moonlight λ" (width, height)
  v ← Viewport.create width height
  t ← newIORef =≪ getCurrentTime

  reactimate (pure init) (sense w v t) (actuate w) sf

  where width  = 800
        height = 600

        init = const NoEvent

        sf = proc i → do
          c ← controller ⤙ i

          let
            up = "Up pressed" <$ c.dpad.up.pressed
            down = "Down pressed" <$ c.dpad.down.pressed

          returnA ⤙ catEvents [ up, down ]

sense ∷ MonadIO μ ⇒ Window → Viewport → IORef UTCTime → Bool → μ (Double, Maybe (ButtonName → Event Bool))
sense w v r _ = do

  update w
  clear v

  t ← getCurrentTime
  t₀ ← readIORef r
  writeIORef r t

  let δt = realToFrac (diffUTCTime t t₀)

  i ← keyMap <$> inputs w

  pure (δt, Just i)

actuate ∷ MonadIO μ ⇒ Window → Bool → Event [String] → μ Bool
actuate w _ (Event x) = liftIO (print x) *> shouldClose w
actuate w _ _ = shouldClose w

keyMap ∷ Map Key KeyState → ButtonName → Event Bool
keyMap ks b = maybeToEvent $ find =≪ case b of
  DpadUp    → Just Key'W
  DpadDown  → Just Key'S
  DpadLeft  → Just Key'A
  DpadRight → Just Key'D
  _ → Nothing

  where find = (ks !?) ↣ \ case
          KeyState'Pressed  → Just True
          KeyState'Released → Just False
          _                 → Nothing
