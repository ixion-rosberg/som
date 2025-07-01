module SOM.Gauge (Gauge) where

import SOM.Prelude

import SOM.Draw (Draw)

type Gauge = Float → Draw
