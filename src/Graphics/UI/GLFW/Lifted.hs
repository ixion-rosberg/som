module Graphics.UI.GLFW.Lifted
  ( createWindow
  , init
  , makeContextCurrent
  , pollEvents
  , swapBuffers
  , terminate
  , windowHint
  , windowShouldClose
  ) where

import SOM.Prelude

import Control.Monad.IO.Class (MonadIO, liftIO)

import Graphics.UI.GLFW (Monitor, Window, WindowHint)
import qualified Graphics.UI.GLFW as Unlifted
  ( createWindow
  , init
  , makeContextCurrent
  , pollEvents
  , swapBuffers
  , terminate
  , windowHint
  , windowShouldClose
  )

createWindow ∷ MonadIO μ ⇒ Int → Int → String → Maybe Monitor → Maybe Window → μ (Maybe Window)
createWindow = ((((liftIO ∘) ∘) ∘) ∘) ∘ Unlifted.createWindow

init ∷ MonadIO μ ⇒ μ Bool
init = liftIO Unlifted.init

makeContextCurrent ∷ MonadIO μ ⇒ Maybe Window → μ ()
makeContextCurrent = liftIO ∘ Unlifted.makeContextCurrent

pollEvents ∷ MonadIO μ ⇒ μ ()
pollEvents = liftIO Unlifted.pollEvents

swapBuffers ∷ MonadIO μ ⇒ Window → μ ()
swapBuffers = liftIO ∘ Unlifted.swapBuffers

terminate ∷ MonadIO μ ⇒ μ ()
terminate = liftIO Unlifted.terminate

windowHint ∷ MonadIO μ ⇒ WindowHint → μ ()
windowHint = liftIO ∘ Unlifted.windowHint

windowShouldClose ∷ MonadIO μ ⇒ Window → μ Bool
windowShouldClose = liftIO ∘ Unlifted.windowShouldClose
