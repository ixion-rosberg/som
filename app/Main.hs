module Main where

import SOM.Prelude

import SOM.Window (shouldClose, update)
import qualified SOM.Window as Window (create)

import Control.Monad (when)

main ∷ IO ()
main = do
  w ← Window.create "Sword of Moonlight λ" (width, height)

  loop w

  where

    loop w = do
      continue ← not <$> shouldClose w

      update w

      when continue (loop w)

    width = 800
    height = 600
