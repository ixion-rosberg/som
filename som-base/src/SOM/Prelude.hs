module SOM.Prelude
  ( module E
  ) where

import Prelude as E hiding
  ( error
  , head
  , init
  , last
  , lookup
  , return
  , readFile
  , tail
  , (!!)
  , (>>)
  )

import Prelude.Unicode as E hiding ((‼))

import Control.Arrow.Unicode as E
import Control.Monad.Unicode as E hiding ((≫))

import Data.Functor as E (($>))

import Numeric.Natural.Unicode as E (ℕ)
