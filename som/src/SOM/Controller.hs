module SOM.Controller
  ( Button (..)
  , ButtonName (..)
  , Controller (..)
  , Dpad (..)
  , controller
  , noInput
  ) where

-- The Controller keeps track of any button press events in addition to the state of each button.
-- This ensures that all inputs are handled properly.
-- Trying to detect button presses at a later point - especially inside
-- signal functions that can be switched out - might lead to
-- them being recognized multiple times or not at all.

import SOM.Prelude

import FRP.Yampa (Event (..), SF, edge, hold, returnA)

data ButtonName = Start
                | Select
                | DpadUp
                | DpadDown
                | DpadLeft
                | DpadRight
                | Circle
                | X
                | Square
                | Triangle
                | L1
                | L2
                | R1
                | R2

data Controller = Controller { start    ∷ Button
                             , select   ∷ Button
                             , dpad     ∷ Dpad
                             , circle   ∷ Button
                             , x        ∷ Button
                             , square   ∷ Button
                             , triangle ∷ Button
                             , l1       ∷ Button
                             , l2       ∷ Button
                             , r1       ∷ Button
                             , r2       ∷ Button
                             }

data Button = Button { held ∷ Bool, pressed ∷ Event () }

data Dpad = Dpad { up    ∷ Button
                 , down  ∷ Button
                 , left  ∷ Button
                 , right ∷ Button
                 }

controller ∷ SF (ButtonName → Event Bool) Controller
controller = Controller
  <$> button Start
  <*> button Select
  <*> ( Dpad
        <$> button DpadUp
        <*> button DpadDown
        <*> button DpadLeft
        <*> button DpadRight
      )
  <*> button Circle
  <*> button X
  <*> button Square
  <*> button Triangle
  <*> button L1
  <*> button L2
  <*> button R1
  <*> button R2

button ∷ ButtonName → SF (ButtonName → Event Bool) Button
button b = proc i → do
  h ← hold False ⤙ i b
  p ← edge ⤙ h
  returnA ⤙ Button h p

noInput ∷ Controller
noInput = Controller n n (Dpad n n n n) n n n n n n n n
  where n = Button False NoEvent
