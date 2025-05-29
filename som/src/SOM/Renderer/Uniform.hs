module SOM.Renderer.Uniform (UniformValue (..)) where

import SOM.Prelude

import SOM.Renderer.Program (Program, uniformLocation)

import Data.Foldable (toList)

import UnliftIO (MonadUnliftIO)
import UnliftIO.Foreign (withArray)

import Graphics.GL (pattern GL_TRUE, GLfloat, glUniform1f, glUniformMatrix4fv)

import Linear.Matrix (M44)

class UniformValue α where
  setUniform ∷ MonadUnliftIO μ ⇒ Program → String → α → μ ()

instance UniformValue GLfloat where
  setUniform p u v = uniformLocation p u ≫= flip glUniform1f v

instance UniformValue (M44 GLfloat) where
  setUniform p u v = uniformLocation p u ≫= \ l → withArray (flatten v) (glUniformMatrix4fv l 1 GL_TRUE)
    where flatten = toList ↢ toList

instance UniformValue α ⇒ UniformValue [α] where
  setUniform p u vs = mapM_ set' (zip [0..] vs)
    where set' (i, v) = setUniform p (u <> "[" <> show @Int i <> "]") v
