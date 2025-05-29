module SOM.Window (Window, create, inputs, shouldClose, update) where

import SOM.Prelude

import Control.Monad (unless)
import Control.Monad.IO.Class (MonadIO)

import Data.Either.Extra (maybeToEither)
import Data.Map.Strict (Map, insert)

import Graphics.UI.GLFW (Key, KeyState, OpenGLProfile (..), WindowHint (..))
import Graphics.UI.GLFW qualified as GLFW (Window)
import Graphics.UI.GLFW.Lifted
  ( createWindow
  , init
  , makeContextCurrent
  , pollEvents
  , setKeyCallback
  , swapBuffers
  , windowHint
  , windowShouldClose
  )

import UnliftIO.Exception (fromEither, stringException, throwString)
import UnliftIO.IORef (IORef, modifyIORef, newIORef, readIORef, writeIORef)

type Dimensions = (ℕ, ℕ)

data Window = Window GLFW.Window (IORef Inputs)

type Inputs = Map Key KeyState

create ∷ MonadIO μ ⇒ String → Dimensions → μ Window
create t (w, h) = do
  initialized ← init

  unless initialized (throwString "Failed to initialize GLFW.")

  mapM_ windowHint hints

  win ← fromEither ∘ explain =≪ createWindow (fromIntegral w) (fromIntegral h) t Nothing Nothing

  makeContextCurrent (Just win)

  r ← newIORef mempty

  setKeyCallback win (Just (keyCallback r))

  pure (Window win r)

  where
    hints = [ WindowHint'ContextVersionMajor 4
            , WindowHint'ContextVersionMinor 5
            , WindowHint'OpenGLProfile       OpenGLProfile'Core
            , WindowHint'Resizable           False
            ]

    explain = maybeToEither (stringException "Failed to create window.")

    keyCallback r _ k _ s _  = modifyIORef r (insert k s)

update ∷ MonadIO μ ⇒ Window → μ ()
update (Window w r) = writeIORef r mempty *> swapBuffers w *> pollEvents

inputs ∷ MonadIO μ ⇒ Window → μ Inputs
inputs (Window _ r) = readIORef r

shouldClose ∷ MonadIO μ ⇒ Window → μ Bool
shouldClose (Window w _) = windowShouldClose w
