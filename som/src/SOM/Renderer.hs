module SOM.Renderer (Draw (..), ProgramList (..), Renderer, create, draw, loadAnimated, loadPiece) where

import SOM.Prelude

import SOM.Animation (Skin (..))
import SOM.Binary.Animated qualified as Animated (Joint (..), Model (..))
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
import SOM.Viewport (Viewport, clear, perspective)

import Graphics.GL (GLfloat, GLushort)

import Linear.Matrix (M44)
import Linear.V2 (V2)
import Linear.V3 (V3)
import Linear.V4 (V4)

import UnliftIO (MonadUnliftIO)

data ProgramList α = ProgramList { piece ∷ α, animated ∷ α } deriving (Functor, Foldable, Traversable)

data Renderer = Renderer { viewport ∷ Viewport
                         , programs ∷ ProgramList Program
                         }

create ∷ MonadUnliftIO μ ⇒ Viewport → ProgramList [Source] → μ Renderer
create v ss = Renderer v <$> (mapM Program.create ss)

draw ∷ MonadUnliftIO μ ⇒ Renderer → Game → μ ()
draw r g = do
  clear r.viewport

  enable r.programs.piece
  setUniform r.programs.piece "view" g.player.view
  setUniform r.programs.piece "projection" (perspective r.viewport)
  drawMap g.map

  enable r.programs.animated
  setUniform r.programs.animated "view" g.player.view
  setUniform r.programs.animated "projection" (perspective r.viewport)
  drawObjects g.objects
  where
    drawMap = mapM_ drawPiece ∘ pieces
    drawPiece = (\ (Draw d) → d) ∘ (.draw)

    drawObjects = mapM_ drawObject
    drawObject = (\ (Draw d) → d) ∘ (.draw)

loadPiece ∷ MonadUnliftIO μ ⇒ Renderer → Piece.Model → Texture → μ (M44 Float → Draw)
loadPiece r m t = do
  enable r.programs.piece

  v ← VAO.create fs m.vertices m.indices

  pure \ tr → Draw do
    bind t
    setUniform r.programs.piece "model" tr
    VAO.draw v

  where fs = [ format (V3 GLfloat), format (V3 GLfloat), format (V2 GLfloat) ]

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
             , format (V2 GLfloat)
             , format (V4 GLushort)
             , format (V4 GLfloat)
             ]
