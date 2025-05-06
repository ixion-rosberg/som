module SOM.Renderer.Uniform (UniformValue (..)) where

import SOM.Prelude

import SOM.Renderer.Program (Program, uniformLocation)

import Data.Foldable (toList)

import UnliftIO (MonadUnliftIO)
import UnliftIO.Foreign (withArray)

import Graphics.GL (pattern GL_TRUE, GLfloat, glUniformMatrix4fv)

import Linear.Matrix (M44)

class UniformValue α where
  setUniform ∷ MonadUnliftIO μ ⇒ Program → String → α → μ ()

instance UniformValue (M44 GLfloat) where
  setUniform p u v = uniformLocation p u ≫= \ l → withArray (flatten v) (glUniformMatrix4fv l 1 GL_TRUE)
    where flatten = toList ↢ toList
