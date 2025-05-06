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
import SOM.Renderer.Model (load)
import SOM.Renderer.Program (ShaderType (..))

import Control.Monad.IO.Class (MonadIO)

import Data.Binary (decodeFile)
import Data.Map.Strict (Map, (!?))
import Data.Time.Clock (UTCTime, diffUTCTime)
import Data.Time.Clock.Lifted (getCurrentTime)

import FRP.Yampa (Event (..), maybeToEvent, reactimate)

import Graphics.UI.GLFW (Key (..), KeyState (..))

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

  reactimate (pure init) (sense w t) (actuate w r) (controller ⋙ game m)

  where width  = 800
        height = 600
        init = const NoEvent

        loadMap = do
          f ← PieceSetup <$> loadPiece "som/bin/floor.msm"

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


        loadPiece f = do
          (Model vs is) ← decodeFile f

          load vs is

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
  _ → Nothing

  where find = (ks !?) ↣ \ case
          KeyState'Pressed  → Just True
          KeyState'Released → Just False
          _                 → Nothing
