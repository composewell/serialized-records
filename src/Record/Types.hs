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
import Streamly.Internal.System.IO (unsafeInlineIO)
import Streamly.Internal.Data.MutByteArray (MutByteArray)
import System.IO.Unsafe (unsafePerformIO)
import Streamly.Internal.Data.Array (Array(..))

import qualified Streamly.Data.Stream as Stream
import qualified Streamly.Unicode.Stream as Encoding
import qualified Streamly.Internal.Data.Array as Array
import qualified Streamly.Internal.Data.MutByteArray as Serialize

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
offsetMessageBody headerLen = offsetHeaderBody + headerLen

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
    recPrimAddSizeTo :: Int -> a -> Int
    recPrimSerializeAt :: Int -> MutByteArray -> a -> IO Int
    recPrimDeserializeAt :: Int -> MutByteArray -> Int -> IO (Int, a)

class IsRecordPrimitive b => ValueMapper a b | a -> b where
    toValueProxy :: Proxy a -> Proxy b
    toValueProxy _ = Proxy
    toValue :: a -> b
    fromValue :: b -> a

class HasField k r v | k r -> v where
    getField :: k -> r -> v

class RecordMeta k where
    typeHash :: Proxy k -> Array Word8

--------------------------------------------------------------------------------
-- Instances
--------------------------------------------------------------------------------

instance forall a. RecordMeta (Record a) => IsRecordPrimitive (Record a) where
    recPrimHash = typeHash
    recPrimAddSizeTo i (Record _ arr) = i + 4 + Array.length arr
    recPrimSerializeAt i target (Record _ (Array src start end)) = do
        let arrLen = end - start
        i1 <- Serialize.serializeAt i target (i_i32 arrLen)
        Serialize.putSliceUnsafe src start target i1 arrLen
        pure (i1 + arrLen)
    recPrimDeserializeAt i arr end = do
        (i1, len32) <- Serialize.deserializeAt i arr end :: IO (Int, Int32)
        let len = i32_i len32
            record = Array arr i1 (i1 + len)
            encodedTypeHash = getEncodedTypeHash record
            typeMatch = typeHash (Proxy :: Proxy (Record a)) == encodedTypeHash
        pure $ (4 + len, Record typeMatch record)

instance IsRecordPrimitive Utf8 where
    recPrimHash _ = Array.getSliceUnsafe 0 1 arr0To9
    recPrimAddSizeTo i (Utf8 arr) = i + 4 + Array.length arr
    recPrimSerializeAt i target (Utf8 (Array src start end)) = do
        let arrLen = end - start
        i1 <- Serialize.serializeAt i target (i_i32 arrLen)
        Serialize.putSliceUnsafe src start target i1 arrLen
        pure (i1 + arrLen)
    recPrimDeserializeAt i arr end = do
        (i1, len32) <- Serialize.deserializeAt i arr end :: IO (Int, Int32)
        let len = i32_i len32
            utf8 = Array arr i1 (i1 + len)
        pure $ (4 + len, Utf8 utf8)

instance IsRecordPrimitive Int64 where
    recPrimHash _ = Array.getSliceUnsafe 1 2 arr0To9
    recPrimAddSizeTo = Serialize.addSizeTo
    recPrimSerializeAt = Serialize.serializeAt
    recPrimDeserializeAt = Serialize.deserializeAt

instance IsRecordPrimitive Double where
    recPrimHash _ = Array.getSliceUnsafe 2 3 arr0To9
    recPrimAddSizeTo = Serialize.addSizeTo
    recPrimSerializeAt = Serialize.serializeAt
    recPrimDeserializeAt = Serialize.deserializeAt

instance IsRecordPrimitive Bool where
    recPrimHash _ = Array.getSliceUnsafe 3 4 arr0To9
    recPrimAddSizeTo = Serialize.addSizeTo
    recPrimSerializeAt = Serialize.serializeAt
    recPrimDeserializeAt = Serialize.deserializeAt

instance IsRecordPrimitive Int32 where
    recPrimHash _ = Array.getSliceUnsafe 5 6 arr0To9
    recPrimAddSizeTo = Serialize.addSizeTo
    recPrimSerializeAt = Serialize.serializeAt
    recPrimDeserializeAt = Serialize.deserializeAt

instance IsRecordPrimitive Int16 where
    recPrimHash _ = Array.getSliceUnsafe 6 7 arr0To9
    recPrimAddSizeTo = Serialize.addSizeTo
    recPrimSerializeAt = Serialize.serializeAt
    recPrimDeserializeAt = Serialize.deserializeAt

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
    toValue =
        Utf8
            . unsafePerformIO
            . Array.fromStream
            . Encoding.encodeUtf8
            . Stream.fromList
    fromValue (Utf8 arr) =
        unsafeInlineIO
            $ Stream.toList
            $ Encoding.decodeUtf8Chunks
            $ Stream.fromList [arr]


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

{-# INLINE i_i32 #-}
i_i32 :: Int -> Int32
i_i32 = fromIntegral

{-# INLINE i16_i #-}
i16_i :: Int16 -> Int
i16_i = fromIntegral

{-# INLINE deserializeAt_ #-}
deserializeAt_ :: IsRecordPrimitive a => Int -> Array Word8 -> IO a
deserializeAt_ i arr =
    fmap snd
        $ recPrimDeserializeAt i
              (Array.arrContents arr)
              (Array.arrStart arr + Array.length arr)

findFieldIndex :: Int -> Array Word8 -> Int -> Array Word8 -> IO Int32
findFieldIndex headerLen _ ix _ | ix >= offsetMessageBody headerLen = pure 0
findFieldIndex headerLen targetKey ix recArr = do
    curKeyLen <- i16_i <$> deserializeAt_ ix recArr
    let curKey = Array.getSliceUnsafe (ix + 2) curKeyLen recArr
    if targetKey == curKey
    then deserializeAt_ (ix + 2 + curKeyLen) recArr
    else findFieldIndex headerLen targetKey (ix + 2 + curKeyLen + 4) recArr

{-# INLINE getFieldTrustedStatic #-}
getFieldTrustedStatic
    :: (IsRecordPrimitive a, ValueMapper b a)
    => Int
    -> Array Word8
    -> b
getFieldTrustedStatic ix recArr =
    unsafeInlineIO $ fromValue <$> deserializeAt_ ix recArr

{-# INLINE getFieldTrustedDynamic #-}
getFieldTrustedDynamic
    :: (IsRecordPrimitive a, ValueMapper b a)
    => Int
    -> Array Word8
    -> b
getFieldTrustedDynamic ix recArr =
    unsafeInlineIO $ do
        ix1 <- i32_i <$> deserializeAt_ ix recArr
        fromValue <$> deserializeAt_ ix1 recArr

{-# INLINE getFieldTrustedDynamicNullable #-}
getFieldTrustedDynamicNullable
    :: (IsRecordPrimitive a, ValueMapper b a)
    => Int
    -> Array Word8
    -> Maybe b
getFieldTrustedDynamicNullable ix recArr =
    unsafeInlineIO $ do
        ix1 <- i32_i <$> deserializeAt_ ix recArr
        if ix1 == 0
        then pure Nothing
        else Just . fromValue <$> deserializeAt_ ix1 recArr

{-# INLINE getFieldUntrusted #-}
getFieldUntrusted
    :: (IsRecordPrimitive a, ValueMapper b a)
    => Int
    -> Array Word8
    -> Array Word8
    -> Maybe b
getFieldUntrusted headerLen key recArr =
    unsafeInlineIO $ do
        ix <- i32_i <$> findFieldIndex headerLen key 40 recArr
        if ix == 0
        then pure Nothing
        else Just . fromValue <$> deserializeAt_ ix recArr
