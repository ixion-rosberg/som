module SOM.Renderer (Renderer, create, draw) where

import SOM.Prelude

import SOM.Map (Piece (..), pieces)

import SOM.Game (Game (..))
import SOM.Renderer.Model qualified as Model (draw)
import SOM.Renderer.Program (Program, Source, enable)
import SOM.Renderer.Program qualified as Program (create)
import SOM.Renderer.Texture (bind)
import SOM.Renderer.Uniform (setUniform)
import SOM.Viewport (Viewport, clear, perspective)

import Graphics.GL (GLfloat)

import Linear.Matrix (M44)
import Linear.Projection (lookAt)
import Linear.V3 (V3 (..))

import UnliftIO (MonadUnliftIO)

data Renderer = Renderer { viewport ∷ Viewport, program ∷ Program }

create ∷ MonadUnliftIO μ ⇒ Viewport → [Source] → μ Renderer
create v ss = Renderer v <$> Program.create ss

draw ∷ MonadUnliftIO μ ⇒ Renderer → Game → μ ()
draw r g = do
  clear r.viewport
  enable r.program
  setUniform r.program "view" view
  setUniform r.program "projection" (perspective r.viewport)
  drawMap g.map
  where
    drawMap m = mapM_ drawPiece (pieces m)

    drawPiece p = do
      bind p.texture
      setUniform r.program "model" p.transformation
      Model.draw p.model

    view ∷ M44 GLfloat
    view = lookAt (V3 0 1.8 -1) (V3 1 1.8 -1) (V3 0 1 0)
