module SOM.Renderer (Draw (..), ProgramList (..), Renderer, create, draw, loadAnimated, loadGauge, loadPiece, loadStatic) where

import SOM.Prelude

import SOM.Binary.Animated qualified as Animated (Model (..))
import SOM.Binary.Piece qualified as Piece (Model (..))
import SOM.Binary.Static qualified as Static (Model (..))
import SOM.Draw (Draw (..))
import SOM.Game (Game (..), Inspection (..))
import SOM.Game.Animation (Skin (..))
import SOM.Game.Map (Piece (..), pieces)
import SOM.Game.Object (Object (..))
import SOM.Game.Player (Player (..))
import SOM.Game.Player.Head (Head (..))
import SOM.Game.Player.Power (pmax)
import SOM.Game.Model (Model (..))
import SOM.Game.World (World (..))
import SOM.Gauge (Gauge)
import SOM.Renderer.Program (Program, Source, enable)
import SOM.Renderer.Program qualified as Program (create)
import SOM.Renderer.Texture (Texture (..), bind)
import SOM.Renderer.Uniform (setUniform)
import SOM.Renderer.VAO (format)
import SOM.Renderer.VAO qualified as VAO (create, draw)
import SOM.Viewport ( Viewport
                    , clear
                    , disableDepthMask
                    , disableDepthTest
                    , enableDepthMask
                    , enableDepthTest
                    , orthographic
                    , perspective
                    )

import Data.Foldable (toList)
import Data.List (partition, sortOn)

import Graphics.GL (GLfloat, GLushort)

import Linear.Matrix (M44)
import Linear.Metric (distance)
import Linear.V2 (V2 (..))
import Linear.V3 (V3 (..))
import Linear.V4 (V4 (..))

import UnliftIO (MonadUnliftIO)

data ProgramList α = ProgramList { piece    ∷ α
                                 , static   ∷ α
                                 , animated ∷ α
                                 , gauge    ∷ α
                                 } deriving (Functor, Foldable, Traversable)

data Renderer = Renderer { programs ∷ ProgramList Program }

create ∷ MonadUnliftIO μ ⇒ ProgramList [Source] → μ Renderer
create = fmap Renderer ∘ mapM Program.create

draw ∷ MonadUnliftIO μ ⇒ Viewport → Renderer → Gauge → Game → μ ()
draw v r gauge g = clear v *> case g of
  MainGame w         → drawWorld w *> drawHUD w
  InspectingItem w i → drawWorld w *> drawItem i

  where
    drawWorld w = do

      enable r.programs.piece
      setUniform r.programs.piece "view" w.player.head.view
      setUniform r.programs.piece "projection" (perspective v)

      enable r.programs.static
      setUniform r.programs.static "view" w.player.head.view
      setUniform r.programs.static "projection" (perspective v)

      enable r.programs.animated
      setUniform r.programs.animated "view" w.player.head.view
      setUniform r.programs.animated "projection" (perspective v)

      enable r.programs.gauge
      setUniform r.programs.gauge "projection" (orthographic v)

      drawMap w.map

      drawObjects w.player w.objects

    drawMap = mapM_ drawPiece ∘ pieces
    drawPiece = (\ (Draw d) → d) ∘ (.draw)

    drawObjects p xs = do
      mapM_ drawObject os
      disableDepthMask v
      mapM_ drawObject (sortByDistance ts)
      enableDepthMask v
      where (ts, os)       = (partition (.model.transparent) ∘ toList) xs
            sortByDistance = reverse ∘ sortOn (\ o → distance p.position o.position)

    drawObject = drawModel ∘ (.model)

    drawModel = (\ (Draw d) → d) ∘ (.draw)

    drawHUD w = disableDepthTest v *> drawGauge w *> enableDepthTest v

    drawGauge w = ((\ (Draw d) → d) ∘ gauge ∘ (÷ pmax)) w.player.power

    drawItem i = do
      disableDepthTest v

      enable r.programs.static

      setUniform r.programs.static "view" i.view

      drawModel i.model

      enableDepthTest v

loadPiece ∷ MonadUnliftIO μ ⇒ Renderer → Piece.Model → Texture → μ (M44 Float → Draw)
loadPiece r m t = do
  enable p

  v ← VAO.create fs m.vertices m.indices

  pure \ tr → Draw do
    enable p
    bind t
    setUniform p "model" tr
    VAO.draw v

  where p  = r.programs.piece
        fs = [ format (type (V3 GLfloat))
             , format (type (V3 GLfloat))
             , format (type (V2 GLfloat))
             ]

loadStatic ∷ MonadUnliftIO μ ⇒ Renderer → Static.Model → Texture → μ (M44 Float → Model)
loadStatic r m t = do
  enable p

  v ← VAO.create fs m.vertices m.indices

  pure \ tr → Model t.transparent $ Draw do
    enable p
    bind t
    setUniform p "model" tr
    VAO.draw v

    where p  = r.programs.static
          fs = [ format (type (V3 GLfloat))
               , format (type (V3 GLfloat))
               , format (type (V2 GLfloat))
               ]

loadAnimated ∷ MonadUnliftIO μ ⇒ Renderer → Animated.Model → Texture → μ (M44 Float → Skin → Model)
loadAnimated r m t = do
  enable p

  v ← VAO.create fs m.vertices m.indices

  pure \ tr s → Model t.transparent $ Draw do
    enable p
    bind t
    setUniform p "model" tr
    setUniform p "joint_transformations" s.transformations
    setUniform p "joint_ibms" s.inverseBindMatrices
    VAO.draw v

  where p  = r.programs.animated
        fs = [ format (type (V3 GLfloat))
             , format (type (V3 GLfloat))
             , format (type (V2 GLfloat))
             , format (type (V4 GLushort))
             , format (type (V4 GLfloat))
             ]

loadGauge ∷ MonadUnliftIO μ ⇒ Renderer → Texture → V2 Float → V2 Float → μ (Float → Draw)
loadGauge r t (V2 x y) (V2 w h) = do
  enable p

  v ← VAO.create fs vs is

  pure \ val → Draw do
    enable p
    bind t
    setUniform p "max" (x + (val × w))
    VAO.draw v

  where
    p  = r.programs.gauge
    vs = [ tl, bl, br, tr ]
    is = [ 0, 1, 2, 2, 3, 0 ]
    tl = V4  x       y      0 0
    tr = V4 (x + w)  y      1 0
    bl = V4  x      (y + h) 0 1
    br = V4 (x + w) (y + h) 1 1
    fs = [ format (type (V2 GLfloat))
         , format (type (V2 GLfloat))
         ]
