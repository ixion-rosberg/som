module SOM.Player (Player (..), player) where

import SOM.Prelude

import SOM.Controller (Controller)
import SOM.Physics (Position, displacement, velocity)
import SOM.Player.Movement (acceleration, movement)

import Control.Arrow (returnA)

import FRP.Yampa (SF)

import Linear.Matrix (M44)
import Linear.Projection (lookAt)
import Linear.V3 (V3 (..))

data Player = Player { view ∷ M44 Float }

player ∷ Position → SF Controller Player
player p₀ = proc c → do

  rec

    let m = movement c
        a = acceleration v m

    v ← velocity ⤙ a


  p ← (p₀ +) ^≪ displacement ⤙ v


  returnA ⤙ Player (lookAt p (p + dir) up)


  where up = V3 0 1 0
        dir = V3 0 0 -1
