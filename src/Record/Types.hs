module Record.Types
    ( Record(..)
    , HasField(..)
    , FieldPosition(..)
    , TypeHash(..)
    , getFieldGeneric
    , IsValue(..)
    , Value(..)
    ) where

--------------------------------------------------------------------------------
-- Imports
--------------------------------------------------------------------------------

import Data.Int (Int16, Int32)
import Data.Proxy (Proxy(..))
import Data.Word (Word8)
import Streamly.Data.Array (Array)
import Streamly.Internal.System.IO (unsafeInlineIO)
import Streamly.Internal.Data.MutByteArray (Serialize)

import qualified Streamly.Internal.Data.Array as Array
import qualified Streamly.Internal.Data.MutByteArray as Serialize

--------------------------------------------------------------------------------
-- Types
--------------------------------------------------------------------------------

newtype Record a = Record (Array Word8)

data Value
    = Text (Array Word8)
    | Rec (Array Word8)
    | R_Int Int
    | R_Double Double
    | Null
    | R_Bool Bool

class IsValue a where
    toValue :: a -> Value
    fromValue :: Value -> a

class FieldPosition k r where
    icFieldIndex :: k -> r -> Int

class HasField k r v | k r -> v where
    getField :: k -> r -> v

class TypeHash k where
    typeHash :: Proxy k -> Array Word8
    headerLength :: Proxy k -> Int

--------------------------------------------------------------------------------
-- Record Helpers
--------------------------------------------------------------------------------

{-# INLINE i32_i #-}
i32_i :: Int32 -> Int
i32_i = fromIntegral

{-# INLINE i16_i #-}
i16_i :: Int16 -> Int
i16_i = fromIntegral

{-# INLINE deserializeAt_ #-}
deserializeAt_ :: Serialize a => Int -> Array Word8 -> IO a
deserializeAt_ i arr =
    fmap snd
        $ Serialize.deserializeAt i
              (Array.arrContents arr)
              (Array.arrStart arr + Array.length arr)

findFieldIndex :: Int -> Array Word8 -> Int -> Array Word8 -> IO Int32
findFieldIndex headerLen _ icCurKeyLen _ | icCurKeyLen >= (40 + headerLen)
    = pure (-1)
findFieldIndex headerLen targerKey icCurKeyLen recArr = do
    curKeyLen <- i16_i <$> deserializeAt_ icCurKeyLen recArr
    let curKey = Array.getSliceUnsafe (icCurKeyLen + 2) curKeyLen recArr
    if (targerKey == curKey)
    then deserializeAt_ (icCurKeyLen + 2 + curKeyLen) recArr
    else
        findFieldIndex
            headerLen targerKey (icCurKeyLen + 2 + curKeyLen + 4) recArr

-- icFieldIndex_ is only reliable only if the typehash matches
{-# INLINE getFieldGeneric #-}
getFieldGeneric
    :: Serialize b
    => Array Word8
    -> Int
    -> Int
    -> Array Word8
    -> Array Word8
    -> Maybe b
getFieldGeneric typHash headerLen icFieldIndex_ key recArr = unsafeInlineIO $ do
    let encodedTypeHash = Array.getSliceUnsafe 6 32 recArr
    fieldIndex <-
        i32_i <$>
            if (typHash == encodedTypeHash)
            then deserializeAt_ icFieldIndex_ recArr  :: IO Int32
            else findFieldIndex headerLen key 40 recArr
    if fieldIndex > 0
    then Just <$> deserializeAt_ fieldIndex recArr
    else pure Nothing
