module SOM.Game.Item (Item (..)) where

import SOM.Prelude

import SOM.Game.Model (Model)

import Data.Function (on)

import Linear.Matrix (M44)

data Item = Item { name ∷ String, model ∷ M44 Float → Model }

instance Eq Item where
  (==) = (≡) `on` (.name)
