module Data.IORef.Lifted (newIORef, readIORef, writeIORef) where

import SOM.Prelude

import Control.Monad.IO.Class (MonadIO, liftIO)

import Data.IORef (IORef)
import Data.IORef qualified as Unlifted (newIORef, readIORef, writeIORef)

newIORef ∷ MonadIO μ ⇒ α → μ (IORef α)
newIORef = liftIO ∘ Unlifted.newIORef

readIORef ∷ MonadIO μ ⇒ IORef α → μ α
readIORef = liftIO ∘ Unlifted.readIORef

writeIORef ∷ MonadIO μ ⇒ IORef α → α → μ ()
writeIORef = (liftIO ∘) ∘ Unlifted.writeIORef
