module SOM.Prelude
  ( module P
  , module U
  ) where

import Prelude as P hiding
  ( head
  , init
  , last
  , return
  , tail
  , (!!)
  , (>>)
  )

import Prelude.Unicode as U hiding ((‼))

import Control.Monad.Unicode as U hiding ((≫))
