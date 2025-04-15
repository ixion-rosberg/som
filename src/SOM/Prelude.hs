module SOM.Prelude
  ( module E
  ) where

import Prelude as E hiding
  ( error
  , head
  , init
  , last
  , return
  , readFile
  , tail
  , (!!)
  , (>>)
  )

import Prelude.Unicode as E hiding ((‼))

import Control.Monad.Unicode as E hiding ((≫))

import Data.Functor as E (($>))
