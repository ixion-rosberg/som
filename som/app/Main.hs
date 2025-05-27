module Main where

import SOM.Prelude

import SOM.Animation (Skin (..))
import SOM.Binary.Animated (Joint (..), Model (..))
import SOM.Controller (ButtonName (..), controller)
import SOM.Game (Game, game)
import SOM.Map (Orientation (..), PieceSetup (..))
import SOM.Map qualified as Map (create)
import SOM.Object (chest)
import SOM.Viewport qualified as Viewport (create)
import SOM.Window (Window, shouldClose, update, inputs)
import SOM.Window qualified as Window (create)
import SOM.Renderer (ProgramList (..), Renderer, draw, loadAnimated, loadPiece)
import SOM.Renderer qualified as Renderer (create)
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
  r ← Renderer.create v sources
  t ← newIORef =≪ getCurrentTime

  m ← loadMap r
  os ← loadObjects r

  reactimate (pure init) (sense w t) (actuate w r) (controller ⋙ game m os p₀)

  where width  = 800
        height = 600
        init = const NoEvent
        p₀ = V3 4 0 -4

        sources = ProgramList
          [ (Vertex,   "som/shaders/piece.vert")
          , (Fragment, "som/shaders/piece.frag")
          ]
          [ (Vertex,   "som/shaders/animated.vert")
          , (Fragment, "som/shaders/animated.frag")
          ]

        loadMap r = do
          f ← createPieceSetup r "som/bin/floor.msm" "som/bin/floor.mhm" "som/bin/set.txr"
          w ← createPieceSetup r "som/bin/wall.msm" "som/bin/wall.mhm" "som/bin/set.txr"
          c ← createPieceSetup r "som/bin/corner.msm" "som/bin/corner.mhm" "som/bin/set.txr"

          pure $ Map.create [ c 0 0 East
                            , w 0 1 South
                            , w 0 2 South
                            , w 0 3 South
                            , c 0 4 South
                            , w 1 0 East
                            , f 1 1 South
                            , f 1 2 South
                            , f 1 3 South
                            , w 1 4 West
                            , w 2 0 East
                            , f 2 1 South
                            , f 2 2 South
                            , f 2 3 South
                            , w 2 4 West
                            , w 3 0 East
                            , f 3 1 South
                            , f 3 2 South
                            , f 3 3 South
                            , w 3 4 West
                            , c 4 0 North
                            , w 4 1 North
                            , w 4 2 North
                            , w 4 3 North
                            , c 4 4 West
                            ]


        createPieceSetup r fm fc ft = do
          m ← decodeFile fm
          c ← decodeFile fc
          t ← Texture.load ft

          d ← loadPiece r m t

          pure (PieceSetup d c)

        loadObjects r = do
          c ← loadChest r "som/bin/chest.mdl" "som/bin/chest.txr"

          pure [ c (V3 6 0.01 -6) ]


        loadChest r fm ft = do
          m ← decodeFile fm
          t ← Texture.load ft
          d ← loadAnimated r m t

          pure $ chest (skin m) m.animation d

        skin m = Skin ((.transformation) <$> m.joints) ((.inverseBindMatrix) <$> m.joints)



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
