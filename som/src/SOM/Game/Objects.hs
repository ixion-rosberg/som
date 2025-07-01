module SOM.Game.Objects (Inspect, Output (..), objects) where

import SOM.Prelude

import SOM.IdentityList (IdentityList, assocs, fromList, insert)
import SOM.Game.Item (Item)
import SOM.Game.Object (Input (..), Object, ObjectSF, Request (..))
import SOM.Game.Object qualified as Object (Output (..))

import Control.Arrow (arr)

import Data.Monoid (Endo (..))

import FRP.Yampa (Event (..), SF, dpSwitchB, mergeEvents, notYet)
import FRP.Yampa.Extra (foldMapEvents)

data Output = Output { objects ∷ IdentityList Object, inspect ∷ Event Inspect }

type Inspect = (ℕ, Item)

objects ∷ [ObjectSF] → SF Input Output
objects = (output ^≪) ∘ objects' ∘ fromList
  where objects' os       = dpSwitchB os (arr modify ⋙ notYet) continue
        modify            = foldMapEvents execute ∘ fmap (.request) ∘ snd
        execute (Spawn o) = Endo (insert o)
        continue sfs f    = objects' ((appEndo f) sfs)
        output os         = Output ((.object) <$> os) (inspectFirst os)
        inspectFirst      = mergeEvents ∘ fmap inspect ∘ assocs
        inspect (i, o)    = (i, ) <$> o.inspect
