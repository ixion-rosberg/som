module Main where

import SOM.Prelude

import SOM.Binary.Animated (Joint (..), Model (..))
import SOM.Binary.Piece (Model (..))
import SOM.Binary.Static (Model (..))
import SOM.Controller (ButtonName (..), controller)
import SOM.Game (Game, game)
import SOM.Game.Animation (Skin (..))
import SOM.Game.Item (Item (..))
import SOM.Game.Map (Orientation (..), PieceSetup (..))
import SOM.Game.Map qualified as Map (create)
import SOM.Game.Object (chest)
import SOM.Gauge (Gauge)
import SOM.Renderer (ProgramList (..), Renderer, draw, loadAnimated, loadGauge, loadPiece, loadStatic)
import SOM.Renderer qualified as Renderer (create)
import SOM.Renderer.Program (ShaderType (..))
import SOM.Renderer.Texture qualified as Texture (load)
import SOM.Viewport (Viewport)
import SOM.Viewport qualified as Viewport (create)
import SOM.Window (Window, shouldClose, update, inputs)
import SOM.Window qualified as Window (create)

import Control.Monad.IO.Class (MonadIO)

import Data.Binary (decodeFile)
import Data.Map.Strict (Map, (!?))
import Data.Time.Clock (UTCTime, diffUTCTime)
import Data.Time.Clock.Lifted (getCurrentTime)

import FRP.Yampa (Event (..), maybeToEvent, reactimate)

import Graphics.UI.GLFW (Key (..), KeyState (..))

import Linear.V2 (V2 (..))
import Linear.V3 (V3 (..))

import UnliftIO (MonadUnliftIO)
import UnliftIO.IORef (IORef, newIORef, readIORef, writeIORef)

main ∷ IO ()
main = do
  w ← Window.create "Sword of Moonλight" (width, height)
  v ← Viewport.create width height
  r ← Renderer.create sources
  t ← newIORef =≪ getCurrentTime

  g ← loadPowerGauge r
  m ← loadMap r
  os ← loadObjects r

  reactimate (pure init) (sense w t) (actuate w v r g) (controller ⋙ game m os p₀)

  where width  = 800
        height = 600
        init = const NoEvent
        p₀ = V3 4 0 -4

        sources = ProgramList
          [ (Vertex,   "som/shaders/piece.vert")
          , (Fragment, "som/shaders/piece.frag")
          ]
          [ (Vertex,   "som/shaders/static.vert")
          , (Fragment, "som/shaders/static.frag")
          ]
          [ (Vertex,   "som/shaders/animated.vert")
          , (Fragment, "som/shaders/animated.frag")
          ]
          [ (Vertex,   "som/shaders/gauge.vert")
          , (Fragment, "som/shaders/gauge.frag")
          ]

        loadPowerGauge r = do
          t ← Texture.load (binDir <> "gauge.txr")
          loadGauge r t (V2 20 20) (V2 240 16)

        loadMap r = do
          f ← createPieceSetup r "floor.msm" "floor.mhm"
          w ← createPieceSetup r "wall.msm" "wall.mhm"
          c ← createPieceSetup r "corner.msm" "corner.mhm"

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

        createPieceSetup r fm fc = do
          m ← decodeFile (binDir <> fm)
          c ← decodeFile (binDir <> fc)
          t ← Texture.load (binDir <> m.texture)

          d ← loadPiece r m t

          pure (PieceSetup d c)

        loadObjects r = do
          c ← loadChest r "chest.mdl"
          p ← loadPotion r "potion.mdo"

          pure [ c (V3 6 0.01 -6) p
               ]

        loadChest r f = do
          m ← decodeFile (binDir <> f)
          t ← Texture.load (binDir <> m.texture)
          d ← loadAnimated r m t

          pure $ chest m.bounds (skin m) m.animation d

        loadPotion r f = do
          m ← decodeFile (binDir <> f)
          t ← Texture.load (binDir <> m.texture)
          d ← loadStatic r m t

          pure $ Item "Potion" d

        skin m = Skin ((.transformation) <$> m.joints) ((.inverseBindMatrix) <$> m.joints)

        binDir = "som/bin/"

sense ∷ MonadIO μ ⇒ Window → IORef UTCTime → Bool → μ (Double, Maybe (ButtonName → Event Bool))
sense w r _ = do

  update w

  t ← getCurrentTime
  t₀ ← readIORef r
  writeIORef r t

  let δt = realToFrac (diffUTCTime t t₀)

  i ← keyMap <$> inputs w

  pure (δt, Just i)

actuate ∷ MonadUnliftIO μ ⇒ Window → Viewport → Renderer → Gauge → Bool → Game → μ Bool
actuate w v r gu _ gm = draw v r gu gm *> shouldClose w

keyMap ∷ Map Key KeyState → ButtonName → Event Bool
keyMap ks b = maybeToEvent $ find =≪ case b of
  DpadUp    → Just Key'W
  DpadDown  → Just Key'S
  DpadLeft  → Just Key'A
  DpadRight → Just Key'D
  Circle    → Just Key'Space
  X         → Just Key'Tab
  L1        → Just Key'Left
  L2        → Just Key'Up
  R1        → Just Key'Right
  R2        → Just Key'Down
  _ → Nothing

  where find = (ks !?) ↣ \ case
          KeyState'Pressed  → Just True
          KeyState'Released → Just False
          _                 → Nothing
