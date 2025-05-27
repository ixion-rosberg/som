module SOM.Objects (objects) where

import SOM.Prelude

import SOM.IdentityList (IdentityList, fromList)
import SOM.Object

import Control.Arrow (arr)

import FRP.Yampa (Event (..), SF, dpSwitchB)

objects ∷ [SF α Object] → SF α (IdentityList Object)
objects = objects' ∘ fromList
  where objects' os = dpSwitchB os (arr $ const NoEvent) (\ sfs f → objects' (f sfs))
