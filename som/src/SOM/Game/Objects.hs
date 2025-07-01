module SOM.Game.Objects (objects) where

import SOM.Prelude

import SOM.IdentityList (IdentityList, fromList, insert)
import SOM.Game.Object (Input (..), Object, Output (..), Request (..))

import Control.Arrow (arr)

import Data.Monoid (Endo (..))

import FRP.Yampa (SF, dpSwitchB, notYet)
import FRP.Yampa.Extra (foldMapEvents)

objects ∷ [SF Input Output] → SF Input (IdentityList Object)
objects = (fmap (.object) ^≪) ∘ objects' ∘ fromList
  where objects' os = dpSwitchB os (arr modify ⋙ notYet) (\ sfs f → objects' ((appEndo f) sfs))

        modify = foldMapEvents execute ∘ fmap (.request) ∘ snd
        execute (Spawn o) = Endo (insert o)
