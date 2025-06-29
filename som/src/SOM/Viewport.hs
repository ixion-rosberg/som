module SOM.Viewport
  ( Viewport
  , clear
  , create
  , disableDepthMask
  , disableDepthTest
  , enableDepthMask
  , enableDepthTest
  , orthographic
  , perspective
  ) where

import SOM.Prelude

import Control.Monad.IO.Class (MonadIO)

import Data.Bits ((.|.))

import Graphics.GL
  ( pattern GL_BLEND
  , pattern GL_COLOR_BUFFER_BIT
  , pattern GL_DEPTH_BUFFER_BIT
  , pattern GL_DEPTH_TEST
  , pattern GL_FALSE
  , pattern GL_ONE_MINUS_SRC_ALPHA
  , pattern GL_SRC_ALPHA
  , pattern GL_TRUE
  , glBlendFunc
  , glClear
  , glClearColor
  , glDepthMask
  , glDisable
  , glEnable
  , glViewport
  )

import Linear.Matrix (M44)
import Linear.Projection qualified as Projection (ortho, perspective)

data Viewport = Viewport ℕ ℕ

create ∷ MonadIO μ ⇒ ℕ → ℕ → μ Viewport
create w h = init $> Viewport w h
  where init = do
          glViewport 0 0 (fromIntegral w) (fromIntegral h)
          glClearColor 0 0 0 1
          glEnable GL_DEPTH_TEST
          glEnable GL_BLEND
          glBlendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA

clear ∷ MonadIO μ ⇒ Viewport → μ ()
clear = const ∘ glClear $ GL_COLOR_BUFFER_BIT .|. GL_DEPTH_BUFFER_BIT

perspective ∷ Viewport → M44 Float
perspective (Viewport w h) = Projection.perspective fov aspect near far
  where fov    = 45 × π ÷ 180
        aspect = fromIntegral w ÷ fromIntegral h
        near   = 0.1
        far    = 20

orthographic ∷ Viewport → M44 Float
orthographic (Viewport w h) = Projection.ortho 0 (fromIntegral w) (fromIntegral h) 0 -1 1

enableDepthTest ∷ MonadIO μ ⇒ Viewport → μ ()
enableDepthTest = const $ glEnable GL_DEPTH_TEST

disableDepthTest ∷ MonadIO μ ⇒ Viewport → μ ()
disableDepthTest = const $ glDisable GL_DEPTH_TEST

enableDepthMask ∷ MonadIO μ ⇒ Viewport → μ ()
enableDepthMask = const $ glDepthMask GL_TRUE

disableDepthMask ∷ MonadIO μ ⇒ Viewport → μ ()
disableDepthMask = const $ glDepthMask GL_FALSE
