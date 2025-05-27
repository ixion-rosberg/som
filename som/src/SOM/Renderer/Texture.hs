module SOM.Renderer.Texture (Texture, bind, load) where

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

newtype Texture = Texture GLuint

load ∷ MonadUnliftIO μ ⇒ FilePath → μ Texture
load f = do
  (B.Texture w h xs) ← decodeFile f

  t ← query $ glCreateTextures GL_TEXTURE_2D 1

  mapM_ (uncurry (glTextureParameteri t)) parameters

  glTextureStorage2D t 1 GL_RGBA8 w h

  withArray xs $ glTextureSubImage2D t 0 0 0 w h GL_RGBA GL_UNSIGNED_BYTE ∘ castPtr

  glGenerateTextureMipmap t

  pure (Texture t)

  where parameters = [ (GL_TEXTURE_WRAP_S,     GL_CLAMP_TO_EDGE)
                     , (GL_TEXTURE_WRAP_T,     GL_CLAMP_TO_EDGE)
                     , (GL_TEXTURE_MAG_FILTER, GL_LINEAR)
                     , (GL_TEXTURE_MIN_FILTER, GL_LINEAR)
                     ]

bind ∷ MonadIO μ ⇒ Texture → μ ()
bind (Texture t) = glBindTextureUnit 0 t
