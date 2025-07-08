module SOM.Game.Player.Inventory (Inventory (..), inventory) where

import SOM.Prelude

import SOM.Game.Item (Item (..))

import Control.Arrow (returnA)

import Data.Map.Strict (Map, fromList, insertWith)

import FRP.Yampa (Event, SF, accumHold)

data Inventory = Inventory (Map String ℕ)

inventory ∷ SF (Event Item) Inventory
inventory = proc ev → do

  is ← accumHold (fromList []) ⤙ addItem <$> ev

  returnA ⤙ Inventory is

  where addItem i = insertWith (+) i.name 1
