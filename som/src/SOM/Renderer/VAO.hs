module SOM.Renderer.VAO (VAO, VertexAttribute (..), create, draw) where

import SOM.Prelude

import Control.Monad (foldM_)
import Control.Monad.IO.Class (MonadIO)

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
import Linear.V4 (V4)

import UnliftIO (MonadUnliftIO)
import UnliftIO.Foreign (withArray)

data VAO = VAO { id ∷ GLuint, count ∷ GLsizei }

type Index = GLushort

data VertexAttributeFormat = VertexAttributeFormat { count ∷ GLint, attributeType ∷ GLenum, size ∷ GLuint }

data VertexAttributeIndex = VertexAttributeIndex { index ∷ GLuint, offset ∷ GLuint }

class VertexAttribute α where
  format ∷ ∀ α' → α' ~ α ⇒ VertexAttributeFormat

instance VertexAttribute GLfloat where
  format _ = VertexAttributeFormat 1 GL_FLOAT (fromIntegral (sizeOf GLfloat))

instance VertexAttribute GLushort where
  format _ = VertexAttributeFormat 1 GL_UNSIGNED_SHORT (fromIntegral (sizeOf GLushort))

instance (Storable α, VertexAttribute α) ⇒ VertexAttribute (V2 α) where
  format _ = VertexAttributeFormat (2 × f.count) f.attributeType (2 × f.size)
    where f = format α

instance (Storable α, VertexAttribute α) ⇒ VertexAttribute (V3 α) where
  format _ = VertexAttributeFormat (3 × f.count) f.attributeType (3 × f.size)
    where f = format α

instance (Storable α, VertexAttribute α) ⇒ VertexAttribute (V4 α) where
  format _ = VertexAttributeFormat (4 × f.count) f.attributeType (4 × f.size)
    where f = format α

create ∷ ∀ α μ. (Storable α, MonadUnliftIO μ) ⇒ [VertexAttributeFormat] → [α] → [Index] → μ VAO
create fs vs is = do
  vbo ← createBuffer vs
  ebo ← createBuffer is

  vao ← query $ glCreateVertexArrays 1

  glVertexArrayVertexBuffer vao 0 vbo 0 (fromIntegral (sizeOf α))
  glVertexArrayElementBuffer vao ebo

  foldM_ (setAttribute vao) (VertexAttributeIndex 0 0) fs

  pure $ VAO vao count

  where
    createBuffer ∷ ∀ β. Storable β ⇒ [β] → μ GLuint
    createBuffer xs = do
      b ← query $ glCreateBuffers 1

      withArray xs (buffer b)

      pure b
      where
        buffer b ptr = glNamedBufferStorage b size (castPtr ptr) GL_DYNAMIC_STORAGE_BIT
        size = (fromIntegral ∘ (length xs ×)) (sizeOf β)

    setAttribute vao i f = do

      glEnableVertexArrayAttrib vao i.index
      glVertexArrayAttribFormat vao i.index f.count f.attributeType GL_FALSE i.offset
      glVertexArrayAttribBinding vao i.index 0

      pure (i { index = i.index + 1, offset = i.offset + f.size })

    count = (fromIntegral ∘ length) is

draw ∷ MonadIO μ ⇒ VAO → μ ()
draw v = glBindVertexArray v.id
  *> glDrawElements GL_TRIANGLES v.count GL_UNSIGNED_SHORT nullPtr
