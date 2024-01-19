module Record.Types where

{-
    ( Record(..)
    , HasField(..)
    , FieldPosition(..)
    , RecordMeta(..)
    , getFieldGeneric
    , IsValue(..)
    , Value(..)
    ) where
-}

--------------------------------------------------------------------------------
-- Imports
--------------------------------------------------------------------------------

import Data.Int (Int16, Int32, Int64)
import Data.Proxy (Proxy(..))
import Data.Word (Word8)
import Streamly.Data.Array (Array)
import Streamly.Data.MutArray (MutArray)
import Streamly.Internal.System.IO (unsafeInlineIO)
import Streamly.Internal.Data.MutByteArray (Serialize)
import System.IO.Unsafe (unsafePerformIO)

import qualified Streamly.Data.Stream as Stream
import qualified Streamly.Unicode.Stream as Encoding
import qualified Streamly.Internal.Data.Array as Array
import qualified Streamly.Internal.Data.MutByteArray as Serialize

#include "MachDeps.h"

--------------------------------------------------------------------------------
-- Record Spec
--------------------------------------------------------------------------------

offsetVersion :: Int
offsetVersion = 0

offsetMessageLen :: Int
offsetMessageLen = 2

offsetTypeHash :: Int
offsetTypeHash = 6

typeHashLen :: Int
typeHashLen = 32

offsetHeaderLen :: Int
offsetHeaderLen = 38

offsetHeaderBody :: Int
offsetHeaderBody = 40

offsetMessageBody :: Int -> Int
offsetMessageBody headerBodyLen = offsetHeaderBody + headerBodyLen

getEncodedTypeHash :: Array Word8 -> Array Word8
getEncodedTypeHash = Array.getSliceUnsafe offsetTypeHash (offsetTypeHash + 32)

--------------------------------------------------------------------------------
-- Types
--------------------------------------------------------------------------------

-- Record a b
-- a == Is True iff the typehash matched
-- b == Record representation
data Record a = Record Bool (Array Word8)

newtype Utf8 = Utf8 (Array Word8)

-- Should we use MutByteArray here?
class IsRecordPrimitive a where
    recPrimHash :: Proxy a -> Array Word8
    recPrimSerializeAt :: Int -> MutArray Word8 -> a -> IO Int
    recPrimDeserializeAt :: Int -> MutArray Word8 -> IO (a, Int)

class IsRecordPrimitive b => ValueMapper a b | a -> b where
    valueType :: Proxy a -> Proxy b
    valueType _ = Proxy
    toValue :: a -> b
    fromValue :: b -> a

data FieldPos
    = ICOffsetToValue Int
    | ICValue Int

class HasField k r v | k r -> v where
    getField :: k -> r -> v
    fieldPosition :: k -> r -> FieldPos

class RecordMeta k where
    typeHash :: Proxy k -> Array Word8
    headerLength :: Proxy k -> Int

--------------------------------------------------------------------------------
-- Instances
--------------------------------------------------------------------------------

instance RecordMeta (Record a) => IsRecordPrimitive (Record a) where
    recPrimHash = typeHash
    recPrimSerializeAt = undefined
    recPrimDeserializeAt = undefined

instance IsRecordPrimitive Utf8 where
    recPrimHash _ = Array.getSliceUnsafe 0 1 arr0To9
    recPrimSerializeAt = undefined
    recPrimDeserializeAt = undefined

instance IsRecordPrimitive Int64 where
    recPrimHash _ = Array.getSliceUnsafe 1 2 arr0To9
    recPrimSerializeAt = undefined
    recPrimDeserializeAt = undefined

instance IsRecordPrimitive Double where
    recPrimHash _ = Array.getSliceUnsafe 2 3 arr0To9
    recPrimSerializeAt = undefined
    recPrimDeserializeAt = undefined

instance IsRecordPrimitive Bool where
    recPrimHash _ = Array.getSliceUnsafe 3 4 arr0To9
    recPrimSerializeAt = undefined
    recPrimDeserializeAt = undefined

instance ValueMapper Utf8 Utf8 where
    toValue = id
    fromValue = id

instance RecordMeta (Record a) => ValueMapper (Record a) (Record a) where
    toValue = id
    fromValue = id

instance ValueMapper Bool Bool where
    toValue = id
    fromValue = id

instance ValueMapper Int Int64 where
    toValue = fromIntegral
    fromValue = fromIntegral

instance ValueMapper Double Double where
    toValue = id
    fromValue = id

instance ValueMapper String Utf8 where
    toValue = undefined
    fromValue = undefined

--------------------------------------------------------------------------------
-- Helpers
--------------------------------------------------------------------------------

{-# INLINE encodeSimpleString #-}
encodeSimpleString :: String -> Array Word8
encodeSimpleString val =
    unsafePerformIO
        $ Array.fromStreamN (length val)
        $ Encoding.encodeLatin1' $ Stream.fromList val

{-# INLINE arr0To9 #-}
arr0To9 :: Array Word8
arr0To9 = encodeSimpleString "0123456789"

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
findFieldIndex headerLen _ ix _ | ix >= offsetMessageBody headerLen = pure 0
findFieldIndex headerLen targerKey ix recArr = do
    curKeyLen <- i16_i <$> deserializeAt_ ix recArr
    let curKey = Array.getSliceUnsafe (ix + 2) curKeyLen recArr
    if targerKey == curKey
    then deserializeAt_ (ix + 2 + curKeyLen) recArr
    else findFieldIndex headerLen targerKey (ix + 2 + curKeyLen + 4) recArr

{-# INLINE getFieldGeneric #-}
getFieldGeneric
    :: Serialize b
    => Array Word8
    -> Int
    -> FieldPos
    -> Array Word8
    -> Array Word8
    -> Maybe b
getFieldGeneric _ _ (ICValue ix) _ recArr =
    unsafeInlineIO $ Just <$> deserializeAt_ ix recArr
getFieldGeneric typHash headerLen (ICOffsetToValue ix) key recArr =
    unsafeInlineIO $ do
        let encodedTypeHash = getEncodedTypeHash recArr
        fieldIndex <-
            i32_i <$>
                if typHash == encodedTypeHash
                then deserializeAt_ ix recArr  :: IO Int32
                else findFieldIndex headerLen key 40 recArr
        if fieldIndex > 0
        then Just <$> deserializeAt_ fieldIndex recArr
        else pure Nothing
