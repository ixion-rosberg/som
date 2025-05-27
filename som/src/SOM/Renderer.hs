module SOM.Renderer (Draw (..), Renderer, create, draw, loadPiece) where

import SOM.Prelude

import SOM.Binary.Piece (Model (..))
import SOM.Game (Game (..))
import SOM.Map (Piece (..), pieces)
import SOM.Player (Player (..))
import SOM.Renderer.Draw (Draw (..))
import SOM.Renderer.Program (Program, Source, enable)
import SOM.Renderer.Program qualified as Program (create)
import SOM.Renderer.Texture (bind)
import SOM.Renderer.Texture qualified as Texture (load)
import SOM.Renderer.Uniform (setUniform)
import SOM.Renderer.VAO (format)
import SOM.Renderer.VAO qualified as VAO (create, draw)
import SOM.Viewport (Viewport, clear, perspective)

import Data.Binary.Lifted (decodeFile)

import Graphics.GL (GLfloat)

import Linear.Matrix (M44)
import Linear.V2 (V2)
import Linear.V3 (V3)

import UnliftIO (MonadUnliftIO)

data Renderer = Renderer { viewport ∷ Viewport, program ∷ Program }

create ∷ MonadUnliftIO μ ⇒ Viewport → [Source] → μ Renderer
create v ss = Renderer v <$> Program.create ss

draw ∷ MonadUnliftIO μ ⇒ Renderer → Game → μ ()
draw r g = do
  clear r.viewport
  enable r.program
  setUniform r.program "view" g.player.view
  setUniform r.program "projection" (perspective r.viewport)
  drawMap g.map
  where
    drawMap m = mapM_ drawPiece (pieces m)

    drawPiece p = case p.draw of
      Draw d → d

loadPiece ∷ MonadUnliftIO μ ⇒ Renderer → FilePath → FilePath → μ (M44 Float → Draw)
loadPiece r fm ft = do
  (Model vs is) ← decodeFile fm
  t ← Texture.load ft

  v ← VAO.create fs vs is

  pure \ tr → Draw do
    bind t
    setUniform r.program "model" tr
    VAO.draw v

  where fs = [ format (V3 GLfloat), format (V3 GLfloat), format (V2 GLfloat) ]
