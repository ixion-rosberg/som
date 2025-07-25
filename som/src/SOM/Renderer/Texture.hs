module SOM.Renderer.Texture (Texture (..), bind, load) where

import SOM.Prelude

import SOM.Binary.Texture qualified as B (Texture (..))

import Control.Monad.IO.Class (MonadIO)

import Data.Binary.Lifted (decodeFile)

import Foreign (castPtr)
import Foreign.Extra (query)

import Graphics.GL
  ( pattern GL_CLAMP_TO_EDGE
  , pattern GL_LINEAR
  , pattern GL_RGBA
  , pattern GL_RGBA8
  , pattern GL_TEXTURE_2D
  , pattern GL_TEXTURE_MAG_FILTER
  , pattern GL_TEXTURE_MIN_FILTER
  , pattern GL_TEXTURE_WRAP_S
  , pattern GL_TEXTURE_WRAP_T
  , pattern GL_UNSIGNED_BYTE
  , GLuint
  , glBindTextureUnit
  , glCreateTextures
  , glGenerateTextureMipmap
  , glTextureParameteri
  , glTextureStorage2D
  , glTextureSubImage2D
  )

import UnliftIO (MonadUnliftIO)
import UnliftIO.Foreign (withArray)

data Texture = Texture { id ∷ GLuint, transparent ∷ Bool }

load ∷ MonadUnliftIO μ ⇒ FilePath → μ Texture
load f = do
  (B.Texture w h t xs) ← decodeFile f

  i ← query $ glCreateTextures GL_TEXTURE_2D 1

  mapM_ (uncurry (glTextureParameteri i)) parameters

  glTextureStorage2D i 1 GL_RGBA8 w h

  withArray xs $ glTextureSubImage2D i 0 0 0 w h GL_RGBA GL_UNSIGNED_BYTE ∘ castPtr

  glGenerateTextureMipmap i

  pure (Texture i t)

  where parameters = [ (GL_TEXTURE_WRAP_S,     GL_CLAMP_TO_EDGE)
                     , (GL_TEXTURE_WRAP_T,     GL_CLAMP_TO_EDGE)
                     , (GL_TEXTURE_MAG_FILTER, GL_LINEAR)
                     , (GL_TEXTURE_MIN_FILTER, GL_LINEAR)
                     ]

bind ∷ MonadIO μ ⇒ Texture → μ ()
bind t = glBindTextureUnit 0 t.id
