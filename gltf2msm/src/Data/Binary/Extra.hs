module Data.Binary.Extra (IEEE (..), UnsignedShort (..), decodeMany) where

import SOM.Prelude

import Data.Binary (Binary (..))
import Data.Binary.Get (getFloatle, isEmpty, runGet, getWord16le)
import Data.Binary.Put (putFloatle, putWord16le)
import Data.ByteString.Lazy (ByteString)
import Data.Word (Word16)

decodeMany ∷ Binary α ⇒ ByteString → [α]
decodeMany = runGet getMany
  where getMany = isEmpty ≫= \ case
          True → pure []
          False → (:) <$> get <*> getMany

newtype IEEE = IEEE { unIEEE ∷ Float }

instance Binary IEEE where
  get = IEEE <$> getFloatle
  put = putFloatle ∘ unIEEE

newtype UnsignedShort = UnsignedShort { unUnsignedShort ∷ Word16 }

instance Binary UnsignedShort where
  get = UnsignedShort <$> getWord16le
  put = putWord16le ∘ unUnsignedShort
