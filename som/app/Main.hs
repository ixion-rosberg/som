module Main where

import SOM.Prelude

import SOM.Binary.Piece (Model (..))
import SOM.Controller (ButtonName (..), controller)
import SOM.Game (Game, game)
import SOM.Map (Orientation (..), PieceSetup (..))
import SOM.Map qualified as Map (create)
import SOM.Viewport qualified as Viewport (create)
import SOM.Window (Window, shouldClose, update, inputs)
import SOM.Window qualified as Window (create)
import SOM.Renderer (Renderer, draw)
import SOM.Renderer qualified as Renderer (create)
import SOM.Renderer.Model qualified as Model (load)
import SOM.Renderer.Program (ShaderType (..))
import SOM.Renderer.Texture qualified as Texture (load)

import Control.Monad.IO.Class (MonadIO)

import Data.Binary (decodeFile)
import Data.Map.Strict (Map, (!?))
import Data.Time.Clock (UTCTime, diffUTCTime)
import Data.Time.Clock.Lifted (getCurrentTime)

import FRP.Yampa (Event (..), maybeToEvent, reactimate)

import Graphics.UI.GLFW (Key (..), KeyState (..))

import Linear.V3 (V3 (..))

import UnliftIO (MonadUnliftIO)
import UnliftIO.IORef (IORef, newIORef, readIORef, writeIORef)

main ∷ IO ()
main = do
  w ← Window.create "Sword of Moonλight" (width, height)
  v ← Viewport.create width height
  r ← Renderer.create v [ (Vertex, "som/shaders/shader.vert")
                        , (Fragment, "som/shaders/shader.frag")
                        ]
  t ← newIORef =≪ getCurrentTime

  m ← loadMap

  reactimate (pure init) (sense w t) (actuate w r) (controller ⋙ game m p₀)

  where width  = 800
        height = 600
        init = const NoEvent
        p₀ = V3 2 0 -1

        loadMap = do
          f ← loadPiece "som/bin/floor.msm" "som/bin/floor.txr"

          pure $ Map.create [ f 0 0 South
                            , f 0 1 South
                            , f 0 2 South
                            , f 1 0 South
                            , f 1 1 South
                            , f 1 2 South
                            , f 2 0 South
                            , f 2 1 South
                            , f 2 2 South
                            ]


        loadPiece m t = do
          (Model vs is) ← decodeFile m

          PieceSetup <$> Model.load vs is <*> Texture.load t

sense ∷ MonadIO μ ⇒ Window → IORef UTCTime → Bool → μ (Double, Maybe (ButtonName → Event Bool))
sense w r _ = do

  update w

  t ← getCurrentTime
  t₀ ← readIORef r
  writeIORef r t

  let δt = realToFrac (diffUTCTime t t₀)

  i ← keyMap <$> inputs w

  pure (δt, Just i)

actuate ∷ MonadUnliftIO μ ⇒ Window → Renderer → Bool → Game → μ Bool
actuate w r _ g = draw r g *> shouldClose w

keyMap ∷ Map Key KeyState → ButtonName → Event Bool
keyMap ks b = maybeToEvent $ find =≪ case b of
  DpadUp    → Just Key'W
  DpadDown  → Just Key'S
  DpadLeft  → Just Key'A
  DpadRight → Just Key'D
  Circle    → Just Key'Space
  L1        → Just Key'Left
  L2        → Just Key'Up
  R1        → Just Key'Right
  R2        → Just Key'Down
  _ → Nothing

  where find = (ks !?) ↣ \ case
          KeyState'Pressed  → Just True
          KeyState'Released → Just False
          _                 → Nothing
