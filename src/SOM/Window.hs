module SOM.Window (create, shouldClose, update) where

import SOM.Prelude

import Control.Monad (unless)
import Control.Monad.IO.Class (MonadIO)

import Data.Either.Extra (maybeToEither)

import Graphics.UI.GLFW (OpenGLProfile (..), WindowHint (..))
import qualified Graphics.UI.GLFW as GLFW (Window)
import Graphics.UI.GLFW.Lifted
  ( createWindow
  , init
  , makeContextCurrent
  , pollEvents
  , swapBuffers
  , windowHint
  , windowShouldClose
  )

import UnliftIO.Exception (fromEither, stringException, throwString)

type Dimensions = (Int, Int)

newtype Window = Window GLFW.Window

create ∷ MonadIO μ ⇒ String → Dimensions → μ Window
create t (w, h) = do
  initialized ← init

  unless initialized (throwString "Failed to initialize GLFW.")

  mapM_ windowHint hints

  win ← fromEither ∘ explain ⤛ createWindow w h t Nothing Nothing

  makeContextCurrent (Just win)

  pure (Window win)

    where
      hints = [ WindowHint'ContextVersionMajor 4
              , WindowHint'ContextVersionMinor 5
              , WindowHint'OpenGLProfile       OpenGLProfile'Core
              , WindowHint'Resizable           False
              ]

      explain = maybeToEither (stringException "Failed to create window.")

update ∷ MonadIO μ ⇒ Window → μ ()
update (Window w) = swapBuffers w ≫ pollEvents

shouldClose ∷ MonadIO μ ⇒ Window → μ Bool
shouldClose (Window w) = windowShouldClose w
