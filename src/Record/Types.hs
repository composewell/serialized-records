module Record.Types
    ( Record(..)
    , HasField(..)
    , FieldPosition(..)
    , TypeHash(..)
    ) where

import Data.Proxy (Proxy(..))
import Data.Word (Word8)
import Streamly.Data.Array (Array)

newtype Record a = Record (Array Word8)

class FieldPosition k r where
    fieldPosition :: k -> r -> Int

class HasField k r v | k r -> v where
    getField :: k -> r -> v

class TypeHash k where
    typeHash :: Proxy k -> Array Word8
