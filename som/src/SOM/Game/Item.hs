module SOM.Game.Item (Item (..)) where

import SOM.Prelude

import SOM.Game.Model (Model)

import Linear.Matrix (M44)

data Item = Item { model ∷ M44 Float → Model }
