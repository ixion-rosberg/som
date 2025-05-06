module SOM.Renderer.Program (Program, ShaderType (..), Source, create, enable, uniformLocation) where

import SOM.Prelude

import Control.Monad (void)
import Control.Monad.Extra (whenM)
import Control.Monad.IO.Class (MonadIO)

import Data.List (singleton)

import Foreign.Extra (queries)
import Foreign.Lifted (peek)

import GHC.TypeLits.Printf (printf)

import Graphics.GL
  ( pattern GL_COMPILE_STATUS
  , pattern GL_LINK_STATUS
  , pattern GL_FALSE
  , pattern GL_FRAGMENT_SHADER
  , pattern GL_VERTEX_SHADER
  , GLint
  , GLuint
  , glAttachShader
  , glCompileShader
  , glCreateProgram
  , glCreateShader
  , glDeleteShader
  , glGetProgramInfoLog
  , glGetProgramiv
  , glGetShaderInfoLog
  , glGetShaderiv
  , glGetUniformLocation
  , glLinkProgram
  , glShaderSource
  , glUseProgram
  )

import System.IO.Lifted (readFile)

import UnliftIO (MonadUnliftIO)
import UnliftIO.Exception (throwString)
import UnliftIO.Foreign (castCCharToChar, free, malloc, mallocArray, newArray, newCStringLen, peekArray, withCString)

newtype Program = Program GLuint

data ShaderType = Vertex | Fragment

type Source = (ShaderType, FilePath)

create ∷ MonadUnliftIO μ ⇒ [Source] → μ Program
create = createProgram ↢ mapM createShader

  where
    createProgram ss = do
      p ← glCreateProgram

      mapM_ (glAttachShader p) ss

      glLinkProgram p

      mapM_ glDeleteShader ss

      checkSuccess (glGetProgramiv p GL_LINK_STATUS) (glGetProgramInfoLog p) error

      pure (Program p)

      where error = printf "Failed to link program: %s"

    createShader (t, f) = do
      (srcPtr, srcLen) ← newCStringLen =≪ readFile f

      srcPtrArr ← (newArray ∘ singleton) srcPtr
      srcLenArr ← (newArray ∘ singleton ∘ fromIntegral) srcLen

      s ← glCreateShader typ

      glShaderSource s 1 srcPtrArr srcLenArr
      glCompileShader s

      free srcPtr
      free srcPtrArr
      free srcLenArr

      checkSuccess (glGetShaderiv s GL_COMPILE_STATUS) (glGetShaderInfoLog s) error

      pure s

      where (typ, name) = case t of
              Vertex   → (GL_VERTEX_SHADER, "vertex")
              Fragment → (GL_FRAGMENT_SHADER, "fragment")
            error = printf "Failed to compile %s shader %s: %s" name f

    checkSuccess q l e = whenM (queries (≡ GL_FALSE) q) (throwInfoLog l e)

    throwInfoLog f e = do
      lenPtr ← malloc
      infoLogPtr ← mallocArray maxLen

      void (f (fromIntegral maxLen) lenPtr infoLogPtr)

      len ← peek lenPtr
      infoLog ← peekArray (fromIntegral len) infoLogPtr

      free lenPtr
      free infoLogPtr

      (throwString ∘ e ∘ fmap castCCharToChar) infoLog

      where maxLen = 1024

uniformLocation ∷ MonadUnliftIO μ ⇒ Program → String → μ GLint
uniformLocation (Program p) u = withCString u (glGetUniformLocation p)

enable ∷ MonadIO μ ⇒ Program → μ ()
enable (Program p) = glUseProgram p
