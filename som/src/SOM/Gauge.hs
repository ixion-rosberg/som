module SOM.Gauge (Gauge) where

import SOM.Prelude

import SOM.Renderer.Draw (Draw)

type Gauge = Float → Draw
