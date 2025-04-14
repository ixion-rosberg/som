module SOM.Prelude
  ( module E
  ) where

import Prelude as E hiding
  ( head
  , init
  , last
  , return
  , tail
  , (!!)
  , (>>)
  )

import Prelude.Unicode as E hiding ((‼))

import Control.Monad.Unicode as E hiding ((≫))

import Data.Functor as E (($>))
