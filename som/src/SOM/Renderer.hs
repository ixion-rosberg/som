module SOM.Renderer (Draw (..), ProgramList (..), Renderer, create, draw, loadAnimated, loadGauge, loadPiece) where

import SOM.Prelude

import SOM.Animation (Skin (..))
import SOM.Binary.Animated qualified as Animated (Model (..))
import SOM.Binary.Piece qualified as Piece (Model (..))
import SOM.Game (Game (..))
import SOM.Map (Piece (..), pieces)
import SOM.Object (Object (..))
import SOM.Player (Player (..))
import SOM.Renderer.Draw (Draw (..))
import SOM.Renderer.Program (Program, Source, enable)
import SOM.Renderer.Program qualified as Program (create)
import SOM.Renderer.Texture (Texture, bind)
import SOM.Renderer.Uniform (setUniform)
import SOM.Renderer.VAO (format)
import SOM.Renderer.VAO qualified as VAO (create, draw)
import SOM.Viewport (Viewport, clear, disableDepthTest, enableDepthTest, orthographic, perspective)

import Graphics.GL (GLfloat, GLushort)

import Linear.Matrix (M44)
import Linear.V2 (V2 (..))
import Linear.V3 (V3)
import Linear.V4 (V4 (..))

import UnliftIO (MonadUnliftIO)

data ProgramList α = ProgramList { piece    ∷ α
                                 , animated ∷ α
                                 , gauge    ∷ α
                                 } deriving (Functor, Foldable, Traversable)

data Renderer = Renderer { programs ∷ ProgramList Program }

create ∷ MonadUnliftIO μ ⇒ ProgramList [Source] → μ Renderer
create = fmap Renderer ∘ mapM Program.create

draw ∷ MonadUnliftIO μ ⇒ Viewport → Renderer → Game → μ ()
draw v r g = do
  clear v
  enableDepthTest v

  enable r.programs.piece
  setUniform r.programs.piece "view" g.player.view
  setUniform r.programs.piece "projection" (perspective v)
  drawMap g.map

  enable r.programs.animated
  setUniform r.programs.animated "view" g.player.view
  setUniform r.programs.animated "projection" (perspective v)
  drawObjects g.objects

  disableDepthTest v

  enable r.programs.gauge
  setUniform r.programs.gauge "projection" (orthographic v)
  drawGauge g.powerGauge

  where
    drawMap = mapM_ drawPiece ∘ pieces
    drawPiece = (\ (Draw d) → d) ∘ (.draw)

    drawObjects = mapM_ drawObject
    drawObject = (\ (Draw d) → d) ∘ (.draw)

    drawGauge = (\ (Draw d) → d)

loadPiece ∷ MonadUnliftIO μ ⇒ Renderer → Piece.Model → Texture → μ (M44 Float → Draw)
loadPiece r m t = do
  enable r.programs.piece

  v ← VAO.create fs m.vertices m.indices

  pure \ tr → Draw do
    bind t
    setUniform r.programs.piece "model" tr
    VAO.draw v

  where fs = [ format (V3 GLfloat), format (V3 GLfloat), format (type (V2 GLfloat)) ]

loadAnimated ∷ MonadUnliftIO μ ⇒ Renderer → Animated.Model → Texture → μ (M44 Float → Skin → Draw)
loadAnimated r m t = do
  enable r.programs.animated

  v ← VAO.create fs m.vertices m.indices

  pure \ tr s → Draw do
    bind t
    setUniform r.programs.animated "model" tr
    setUniform r.programs.animated "joint_transformations" s.transformations
    setUniform r.programs.animated "joint_ibms" s.inverseBindMatrices
    setUniform r.programs.animated "model" tr
    VAO.draw v

  where fs = [ format (V3 GLfloat)
             , format (V3 GLfloat)
             , format (type (V2 GLfloat))
             , format (type (V4 GLushort))
             , format (type (V4 GLfloat))
             ]

loadGauge ∷ MonadUnliftIO μ ⇒ Renderer → Texture → V2 Float → V2 Float → μ (Float → Draw)
loadGauge r t (V2 x y) (V2 w h) = do
  enable r.programs.gauge

  v ← VAO.create fs vs is

  pure \ val → Draw do
    bind t
    setUniform r.programs.gauge "max" (x + (val × w))
    VAO.draw v

  where
    vs = [ tl, bl, br, tr ]
    is = [ 0, 1, 2, 2, 3, 0 ]
    tl = V4  x       y      0 0
    tr = V4 (x + w)  y      1 0
    bl = V4  x      (y + h) 0 1
    br = V4 (x + w) (y + h) 1 1
    fs = [ format (type (V2 GLfloat))
         , format (type (V2 GLfloat))
         ]
