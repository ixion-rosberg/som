module SOM.Game.Objects (Input (..), Output (..), objects) where

import SOM.Prelude

import SOM.IdentityList (IdentityList, assocs, delete, fromList, insert)
import SOM.Game.Object (InspectedItem (..), Object, ObjectSF, Request (..), looseItem)
import SOM.Game.Object qualified as Object (Input (..), Output (..))

import Control.Arrow (arr, returnA)

import Data.Monoid (Endo (..))

import FRP.Yampa (Event (..), SF, dpSwitch, iPre, notYet)
import FRP.Yampa.Extra (concatEvents, firstEvent, foldMapEvents)

data Input = Input { input      ∷ Object.Input
                   , returnItem ∷ Event InspectedItem
                   }

data Output = Output { objects ∷ IdentityList Object
                     , inspect ∷ Event InspectedItem
                     }

objects ∷ [ObjectSF] → SF Input Output
objects = (output ⋘) ∘ objects' ∘ fromList
  where objects' os = dpSwitch route os (arr modify ⋙ notYet) continue

        route i sfs = (i.input, ) <$> sfs

        continue sfs f = objects' ((appEndo f) sfs)

        output = proc os → do
          -- delay slightly to ensure that the object collection is updated first
          ev ← iPre NoEvent ⤙ inspectFirst os

          returnA ⤙ Output ((.object) <$> os) ev
          where inspectFirst = firstEvent (.inspect)

        modify (input, os) = concatEvents [ requests, removed, added ]
          where
            requests          = (foldMapEvents execute ∘ fmap (.request)) os
            execute (Spawn o) = Endo (insert o)
            removed           = (firstEvent remove ∘ assocs) os
            remove (i, o)     = Endo (delete i) <$ o.inspect
            added             = add <$> input.returnItem
            add i             = Endo (insert (looseItem i.item i.position))
