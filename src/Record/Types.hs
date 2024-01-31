{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE MagicHash #-}

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

#define DEBUG t
#undef DEBUG

#if defined(DEBUG)
#define DO(x) x
#else
#define DO(x)
#endif

--------------------------------------------------------------------------------
-- Imports
--------------------------------------------------------------------------------

import Data.Int (Int16, Int32, Int64)
import Data.Proxy (Proxy(..))
import Data.Word (Word8)
import Data.Maybe (fromMaybe)
import Streamly.Data.Array (Array)
import Streamly.Internal.System.IO (unsafeInlineIO)
import Streamly.Internal.Data.MutByteArray (MutByteArray)
import System.IO.Unsafe (unsafePerformIO)
import Streamly.Internal.Data.Array (Array(..))
import GHC.Base (IO(..))
import GHC.Exts (touch#)

import qualified Streamly.Data.Stream as Stream
import qualified Streamly.Unicode.Stream as Encoding
import qualified Streamly.Internal.Data.Array as Array
import qualified Streamly.Internal.Data.MutByteArray as Serialize

--------------------------------------------------------------------------------
-- Record Spec
--------------------------------------------------------------------------------

offsetVersion :: Int
offsetVersion = 0

lenVersion :: Int
lenVersion = 2

offsetMessageLen :: Int
offsetMessageLen = 2

lenMessageLen :: Int
lenMessageLen = 4

offsetTypeHash :: Int
offsetTypeHash = 6

lenTypeHash :: Int
lenTypeHash = 32

offsetHeaderLen :: Int
offsetHeaderLen = 38

lenHeaderLen :: Int
lenHeaderLen = 2

offsetHeaderBody :: Int
offsetHeaderBody = 40

offsetMessageBody :: Int -> Int
offsetMessageBody headerLen = offsetHeaderBody + headerLen

getEncodedTypeHash :: Array Word8 -> Array Word8
getEncodedTypeHash = Array.getSliceUnsafe offsetTypeHash 32

--------------------------------------------------------------------------------
-- Types
--------------------------------------------------------------------------------

-- Record a b
-- a == Is True iff the typehash matched
-- b == Record representation
data Record a = Record Bool (Array Word8) deriving (Show)

newtype Utf8 = Utf8 (Array Word8)

breakTrust :: Record a -> Record a
breakTrust (Record _ arr) = Record False arr

-- Should we use MutByteArray here?
class IsRecordPrimitive a where
    recPrimHash :: Proxy a -> Array Word8
    recPrimStaticSize :: Proxy a -> Maybe Int
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

class IsRecordable a where
    typeHash :: Proxy a -> Array Word8
    recStaticSize :: Proxy a -> Maybe Int
    createRecord :: a -> Record a
    parseRecord :: Record a -> a

class NullableMeta a where
    isNullable :: Proxy a -> Bool
    nullabilityLevel :: Proxy a -> Int

--------------------------------------------------------------------------------
-- Instances
--------------------------------------------------------------------------------

instance forall a. NullableMeta a => NullableMeta (Maybe a) where
    isNullable _ = True
    nullabilityLevel _ = 1 + nullabilityLevel (Proxy :: Proxy a)

instance {-# OVERLAPPABLE #-} NullableMeta a where
    isNullable _ = False
    nullabilityLevel _ = 0

instance forall a. IsRecordable a => IsRecordPrimitive (Record a) where
    recPrimHash _ = typeHash (Proxy :: Proxy a)
    recPrimStaticSize _ = recStaticSize (Proxy :: Proxy a)
    recPrimAddSizeTo i (Record _ arr) = i + 4 + Array.length arr
    recPrimSerializeAt i target (Record _ (Array src start end)) = do
        let arrLen = end - start
        Serialize.putSliceUnsafe src start target i arrLen
        pure (i + arrLen)
    recPrimDeserializeAt i arr end = do
        (_, len32) <-
            Serialize.deserializeAt (i + 2) arr end :: IO (Int, Int32)
        let len = i32_i len32
            record = Array arr i (i + len)
            encodedTypeHash = getEncodedTypeHash record
            typeMatch = typeHash (Proxy :: Proxy a) == encodedTypeHash
        pure $ (4 + len, Record typeMatch record)

instance IsRecordPrimitive Utf8 where
    recPrimHash _ = Array.getSliceUnsafe 0 1 arr0To9
    recPrimStaticSize _ = Nothing
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
    recPrimHash _ = Array.getSliceUnsafe 1 1 arr0To9
    recPrimStaticSize = Just . Serialize.sizeOf
    recPrimAddSizeTo = Serialize.addSizeTo
    recPrimSerializeAt = Serialize.serializeAt
    recPrimDeserializeAt = Serialize.deserializeAt

instance IsRecordPrimitive Double where
    recPrimHash _ = Array.getSliceUnsafe 2 1 arr0To9
    recPrimStaticSize = Just . Serialize.sizeOf
    recPrimAddSizeTo = Serialize.addSizeTo
    recPrimSerializeAt = Serialize.serializeAt
    recPrimDeserializeAt = Serialize.deserializeAt

instance IsRecordPrimitive Bool where
    recPrimHash _ = Array.getSliceUnsafe 3 1 arr0To9
    recPrimStaticSize = Just . Serialize.sizeOf
    recPrimAddSizeTo = Serialize.addSizeTo
    recPrimSerializeAt = Serialize.serializeAt
    recPrimDeserializeAt = Serialize.deserializeAt

instance IsRecordPrimitive Int32 where
    recPrimHash _ = Array.getSliceUnsafe 5 1 arr0To9
    recPrimStaticSize = Just . Serialize.sizeOf
    recPrimAddSizeTo = Serialize.addSizeTo
    recPrimSerializeAt = Serialize.serializeAt
    recPrimDeserializeAt = Serialize.deserializeAt

instance IsRecordPrimitive Int16 where
    recPrimHash _ = Array.getSliceUnsafe 6 1 arr0To9
    recPrimStaticSize = Just . Serialize.sizeOf
    recPrimAddSizeTo = Serialize.addSizeTo
    recPrimSerializeAt = Serialize.serializeAt
    recPrimDeserializeAt = Serialize.deserializeAt

-- NOTE: Maybe is a special type and is handled differently. The check for maybe
-- occurs in the header, then and only then the serialization proceeds further.
instance IsRecordPrimitive a => IsRecordPrimitive (Maybe a) where
    -- TODO: Use builder like combination
    recPrimHash _ =
        Array.getSliceUnsafe 7 1 arr0To9 <> recPrimHash (Proxy :: Proxy a)
    recPrimStaticSize _ = Nothing
    recPrimAddSizeTo i Nothing = i
    recPrimAddSizeTo i (Just a) = recPrimAddSizeTo i a
    recPrimSerializeAt _ _ _ = undefined
    recPrimDeserializeAt _ _ _ = undefined

instance ValueMapper a b => ValueMapper (Maybe a) (Maybe b) where
    toValue Nothing = Nothing
    toValue (Just a) = Just $ toValue a
    fromValue Nothing = Nothing
    fromValue (Just b) = Just $ fromValue b

instance ValueMapper Utf8 Utf8 where
    toValue = id
    fromValue = id

instance IsRecordable a => ValueMapper (Record a) (Record a) where
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

{-# INLINE readRecord #-}
readRecord :: IsRecordable a => Array Word8 -> Record a
readRecord (Array arr start end) =
    unsafeInlineIO $ fmap snd $ recPrimDeserializeAt start arr end

{-# INLINE writeRecord #-}
writeRecord :: Record a -> Array Word8
writeRecord (Record _ arr) = arr

{-# INLINE touch #-}
touch :: Array a -> IO ()
touch (Array (Serialize.MutByteArray contents) _ _) =
    IO $ \s -> case touch# contents s of s' -> (# s', () #)

fromJustErr :: String -> Maybe a -> a
fromJustErr str = fromMaybe (error str)

flattenNullable :: Maybe (Maybe a) -> Maybe a
flattenNullable Nothing = Nothing
flattenNullable (Just Nothing) = Nothing
flattenNullable (Just (Just a)) = Just a

{-# INLINE encodeSimpleString #-}
encodeSimpleString :: String -> Array Word8
encodeSimpleString val =
    unsafePerformIO
        $ Array.fromStreamN (length val)
        $ Encoding.encodeLatin1' $ Stream.fromList val

{-# INLINE unsafePutCompleteSlice #-}
unsafePutCompleteSlice :: Int -> MutByteArray -> Array Word8 -> IO Int
unsafePutCompleteSlice i target (Array src start end) = do
    let size = end - start
    Serialize.putSliceUnsafe src start target i size
    pure $ i + size

{-# INLINE unsafePutHeaderKey #-}
unsafePutHeaderKey :: Int -> MutByteArray -> String -> IO (Int, Int)
unsafePutHeaderKey i target key = do
    let (Array src start end) = encodeSimpleString key
        size = end - start
    i1 <- recPrimSerializeAt i target (i_i16 size :: Int16)
    Serialize.putSliceUnsafe src start target i1 size
    pure $ (i1 + size, i1 + size + 4)

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

{-# INLINE i_i16 #-}
i_i16 :: Int -> Int16
i_i16 = fromIntegral

{-# INLINE deserializeAt_ #-}
deserializeAt_ :: IsRecordPrimitive a => Int -> Array Word8 -> IO a
deserializeAt_ i arr =
    fmap snd
        $ recPrimDeserializeAt
              (Array.arrStart arr + i)
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
    unsafeInlineIO $ do
        DO(putStrLn $ "getFieldTrustedStatic:" ++ show ix)
        val <- fromValue <$> deserializeAt_ ix recArr
        touch recArr
        pure val

{-# INLINE getFieldTrustedDynamic #-}
getFieldTrustedDynamic
    :: (IsRecordPrimitive a, ValueMapper b a)
    => Int
    -> Array Word8
    -> b
getFieldTrustedDynamic ix recArr =
    unsafeInlineIO $ do
        DO(putStrLn $ "getFieldTrustedDynamic:" ++ show ix)
        ix1 <- i32_i <$> deserializeAt_ ix recArr
        val <- fromValue <$> deserializeAt_ ix1 recArr
        touch recArr
        pure val

{-# INLINE getFieldTrustedNullable #-}
getFieldTrustedNullable
    :: (IsRecordPrimitive a, ValueMapper b a)
    => Int
    -> Array Word8
    -> Maybe b
getFieldTrustedNullable ix recArr =
    unsafeInlineIO $ do
        DO(putStrLn $ "getFieldTrustedNullable:" ++ show ix)
        ix1 <- i32_i <$> deserializeAt_ ix recArr
        if ix1 == 0
        then pure Nothing
        else do
            val <- Just . fromValue <$> deserializeAt_ ix1 recArr
            touch recArr
            pure val

{-# INLINE getFieldUntrusted #-}
getFieldUntrusted
    :: (IsRecordPrimitive a, ValueMapper b a)
    => Int
    -> Array Word8
    -> Array Word8
    -> Maybe b
getFieldUntrusted headerLen key recArr =
    unsafeInlineIO $ do
        DO(putStrLn $ "getFieldUntrusted:" ++ show key)
        ix <- i32_i <$> findFieldIndex headerLen key 40 recArr
        if ix == 0
        then pure Nothing
        else do
            val <- Just . fromValue <$> deserializeAt_ ix recArr
            touch recArr
            pure val
