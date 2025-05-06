module SOM.Viewport (Viewport, clear, create, perspective) where

import SOM.Prelude

import Control.Monad.IO.Class (MonadIO)

import Data.Bits ((.|.))

import Graphics.GL
  ( pattern GL_COLOR_BUFFER_BIT
  , pattern GL_DEPTH_BUFFER_BIT
  , pattern GL_DEPTH_TEST
  , glClear
  , glClearColor
  , glEnable
  , glViewport
  )

import Linear.Matrix (M44)
import Linear.Projection qualified as Projection (perspective)

data Viewport = Viewport Int Int

create ∷ MonadIO μ ⇒ Int → Int → μ Viewport
create w h = init $> Viewport w h
  where init = do
          glViewport 0 0 (fromIntegral w) (fromIntegral h)
          glClearColor 0 0 0 1
          glEnable GL_DEPTH_TEST

clear ∷ MonadIO μ ⇒ Viewport → μ ()
clear = const ∘ glClear $ GL_COLOR_BUFFER_BIT .|. GL_DEPTH_BUFFER_BIT

perspective ∷ Viewport → M44 Float
perspective (Viewport w h) = Projection.perspective fov aspect near far
  where fov    = 45 × π ÷ 180
        aspect = fromIntegral w ÷ fromIntegral h
        near   = 0.1
        far    = 10
