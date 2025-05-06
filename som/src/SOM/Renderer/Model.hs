module SOM.Renderer.Model (Model, draw, load) where

import SOM.Prelude

import SOM.Binary.Piece qualified as Piece (Vertex)

import Control.Monad.IO.Class (MonadIO)
import Control.Monad.State (evalStateT, get, put)

import Data.Vector.Storable (fromList)
import Data.Vector.Storable.Lifted (unsafeWith)

import Foreign (Storable, castPtr, nullPtr)
import Foreign.Extra (query, sizeOf)

import Graphics.GL
  ( pattern GL_DYNAMIC_STORAGE_BIT
  , pattern GL_FALSE
  , pattern GL_FLOAT
  , pattern GL_TRIANGLES
  , pattern GL_UNSIGNED_SHORT
  , GLenum
  , GLfloat
  , GLint
  , GLsizei
  , GLushort
  , GLuint
  , glBindVertexArray
  , glCreateBuffers
  , glCreateVertexArrays
  , glDrawElements
  , glEnableVertexArrayAttrib
  , glNamedBufferStorage
  , glVertexArrayAttribBinding
  , glVertexArrayAttribFormat
  , glVertexArrayElementBuffer
  , glVertexArrayVertexBuffer
  )

import Linear.V2 (V2)
import Linear.V3 (V3)

import UnliftIO (MonadUnliftIO)

data Model = Model { vao ∷ GLuint, count ∷ GLsizei }

class Storable α ⇒ Vertex α where
  attributeFormats ∷ ∀ α' → α' ~ α ⇒ [VertexAttributeFormat]

instance Vertex Piece.Vertex where
  attributeFormats _ = [ format (V3 GLfloat), format (V3 GLfloat), format (V2 GLfloat) ]

type Index = GLushort

data VertexAttributeFormat = VertexAttributeFormat { count ∷ GLint, aType ∷ GLenum, size ∷ GLuint }

data VertexAttributeIndex = VertexAttributeIndex { index ∷ GLuint, offset ∷ GLuint }

class VertexAttribute α where
  format ∷ ∀ α' → α' ~ α ⇒ VertexAttributeFormat

instance VertexAttribute GLfloat where
  format _ = VertexAttributeFormat 1 GL_FLOAT (fromIntegral (sizeOf GLfloat))

instance (Storable α, VertexAttribute α) ⇒ VertexAttribute (V2 α) where
  format _ = VertexAttributeFormat (2 × f.count) f.aType (2 × f.size)
    where f = format α

instance (Storable α, VertexAttribute α) ⇒ VertexAttribute (V3 α) where
  format _ = VertexAttributeFormat (3 × f.count) f.aType (3 × f.size)
    where f = format α

load ∷ ∀ α μ. (Vertex α, MonadUnliftIO μ) ⇒ [α] → [Index] → μ Model
load vs is = do

  vbo ← createBuffer vs
  ebo ← createBuffer is

  vao ← query $ glCreateVertexArrays 1

  glVertexArrayVertexBuffer vao 0 vbo 0 (fromIntegral (sizeOf α))
  glVertexArrayElementBuffer vao ebo

  evalStateT (mapM_ (setAttribute vao) (attributeFormats α)) (VertexAttributeIndex 0 0)

  pure $ Model vao count

  where
    createBuffer ∷ ∀ β. Storable β ⇒ [β] → μ GLuint
    createBuffer xs = do
      b ← query $ glCreateBuffers 1

      unsafeWith (fromList xs) (buffer b)

      pure b
      where
        buffer b ptr = glNamedBufferStorage b size (castPtr ptr) GL_DYNAMIC_STORAGE_BIT
        size = (fromIntegral ∘ (length xs ×)) (sizeOf β)

    setAttribute vao f = do
      i ← get

      glEnableVertexArrayAttrib vao i.index
      glVertexArrayAttribFormat vao i.index f.count f.aType GL_FALSE i.offset
      glVertexArrayAttribBinding vao i.index 0

      put (i { index = i.index + 1, offset = i.offset + f.size })

    count = (fromIntegral ∘ length) is

draw ∷ MonadIO μ ⇒ Model → μ ()
draw m = glBindVertexArray m.vao
  *> glDrawElements GL_TRIANGLES m.count GL_UNSIGNED_SHORT nullPtr
