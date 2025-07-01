module SOM.Game.Model (Model (..)) where

import SOM.Prelude

import SOM.Draw (Draw)

data Model = Model { transparent ∷ Bool, draw ∷ Draw }
