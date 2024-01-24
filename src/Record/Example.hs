{-# OPTIONS_GHC -Wno-orphans #-}

module Record.Example where

import Record.Types

import Control.Monad (void)
import Data.Int (Int16, Int32)
import Data.Maybe (fromJust)
import Data.Proxy (Proxy(..))
import Streamly.Internal.Data.Array (Array(..))
import System.IO.Unsafe (unsafePerformIO)

import qualified Streamly.Internal.Data.Array as Array
import qualified Streamly.Internal.Data.MutByteArray as Serialize

--------------------------------------------------------------------------------
-- Address
--------------------------------------------------------------------------------

data Address = Address
    { zipCode :: Int
    , country :: Maybe (Maybe (Maybe String))
    }

-- This should be derived via template-haskell helpers
instance IsRecordable Address where
    --   2 + length "zipCode" + 4 -- 13
    -- + 2 + length "country" + 4 -- 13
    -- headerLength _ = 26

    -- TODO: Use xxHash here and use a builder like combination
    typeHash _ = Array.getSliceUnsafe 0 32 $
           encodeSimpleString "zipCode" <> recPrimHash (toValueProxy (Proxy :: Proxy Int))
        <> encodeSimpleString "country" <> recPrimHash (toValueProxy (Proxy :: Proxy String))
        <> Array.fromList (replicate 32 0)

    isRecordStatic _ = False

    createRecord addr = unsafePerformIO $ do
        let zipCode_ = toValue $ zipCode addr
            country_ =
                toValue $ flattenNullable $ flattenNullable $ country addr
            size =
                recPrimAddSizeTo (recPrimAddSizeTo 0 zipCode_) country_
                    + lenVersion
                    + lenMessageLen
                    + lenTypeHash
                    + lenHeaderLen
                    + 26 -- headerLength
        arr <- Serialize.new size
        let i0 = 0
        i1 <- recPrimSerializeAt i0 arr (0 :: Int16)
        i2 <- recPrimSerializeAt i1 arr (i_i32 size)
        let hash = typeHash (Proxy :: Proxy Address)
        i3 <- unsafePutCompleteSlice i2 arr hash
        i4 <- recPrimSerializeAt i3 arr (26 :: Int16)
        i5 <- unsafePutHeaderKey i4 arr "zipCode"
        let i6 = i5 + 4
        i7 <- unsafePutHeaderKey i6 arr "country"
        let i8 = i7 + 4
        void $ recPrimSerializeAt i5 arr (i_i32 i8 :: Int32)
        i9 <- recPrimSerializeAt i8 arr zipCode_

        -- Special handling of Nullable values
        i10 <- case country_ of
            Nothing -> do
                void $ recPrimSerializeAt i7 arr (0 :: Int32)
                pure i9
            Just val -> do
                void $ recPrimSerializeAt i7 arr (i_i32 i9 :: Int32)
                recPrimSerializeAt i9 arr val
        pure $ Record True $ Array arr 0 i10

instance HasField (Proxy "zipCode") (Record Address) Int where

    getField :: Proxy "zipCode" -> Record Address -> Int
    getField _ (Record True arr) = getFieldTrustedStatic (offsetMessageBody 26) arr
    getField _ (Record False arr) =
        fromJust $ getFieldUntrusted 26 (encodeSimpleString "zipCode") arr

instance HasField (Proxy "country") (Record Address) (Maybe (Maybe (Maybe String))) where

    getField :: Proxy "country" -> Record Address -> Maybe (Maybe (Maybe String))
    getField _ (Record True arr) =
        case getFieldTrustedNullable (offsetHeaderBody + 22) arr of
            Nothing -> Nothing
            Just res -> Just $ Just $ Just res
    getField _ (Record False arr) = do
        -- Special handling of Nullable values
        case getFieldUntrusted 26 (encodeSimpleString "country") arr of
            Nothing -> Nothing
            Just res -> Just $ Just $ Just res

--------------------------------------------------------------------------------
-- User
--------------------------------------------------------------------------------

data User = User
    { name :: String
    , age :: Int
    , height :: Double
    , isMarried :: Bool
    , address :: Record Address
    }

-- TODO: order is decided by the TH combinators
-- order: [age, height, isMarried, name, address]
instance IsRecordable User where
    --   2 + length "name"      + 4 -- 10
    -- + 2 + length "age"       + 4 -- 9
    -- + 2 + length "height"    + 4 -- 12
    -- + 2 + length "isMarried" + 4 -- 15
    -- + 2 + length "address"   + 4 -- 13
    -- headerLength _ = 59

    -- TODO: Use xxHash here and use a builder like combination
    typeHash _ = Array.getSliceUnsafe 0 32 $
           encodeSimpleString "name" <> recPrimHash (toValueProxy (Proxy :: Proxy String))
        <> encodeSimpleString "age" <> recPrimHash (toValueProxy (Proxy :: Proxy Int))
        <> encodeSimpleString "height" <> recPrimHash (toValueProxy (Proxy :: Proxy Double))
        <> encodeSimpleString "isMarried" <> recPrimHash (toValueProxy (Proxy :: Proxy Bool))
        <> encodeSimpleString "address" <> recPrimHash (toValueProxy (Proxy :: Proxy (Record Address)))
        <> Array.fromList (replicate 32 0)

    isRecordStatic _ = False

    createRecord user = unsafePerformIO $ do
        let name_ = toValue $ name user
            age_ = toValue $ age user
            height_ = toValue $ height user
            isMarried_ = toValue $ isMarried user
            address_ = toValue $ address user
            size =
                recPrimAddSizeTo
                    (recPrimAddSizeTo
                         (recPrimAddSizeTo
                              (recPrimAddSizeTo
                                   (recPrimAddSizeTo 0 address_)
                                   isMarried_)
                              height_)
                         age_)
                    name_
                    + lenVersion
                    + lenMessageLen
                    + lenTypeHash
                    + lenHeaderLen
                    + 59 -- headerLength
        arr <- Serialize.new size
        let i0 = 0
        i1 <- recPrimSerializeAt i0 arr (0 :: Int16)
        i2 <- recPrimSerializeAt i1 arr (i_i32 size)
        let hash = typeHash (Proxy :: Proxy User)
        i3 <- unsafePutCompleteSlice i2 arr hash
        i4 <- recPrimSerializeAt i3 arr (59 :: Int16)
        i5 <- unsafePutHeaderKey i4 arr "age"
        let i6 = i5 + 4
        i7 <- unsafePutHeaderKey i6 arr "height"
        let i8 = i7 + 4
        i9 <- unsafePutHeaderKey i8 arr "isMarried"
        let i10 = i9 + 4
        i11 <- unsafePutHeaderKey i10 arr "name"
        let i12 = i11 + 4
        i13 <- unsafePutHeaderKey i12 arr "address"
        let i14 = i13 + 4
        void $ recPrimSerializeAt i5 arr (i_i32 i14 :: Int32)
        i15 <- recPrimSerializeAt i14 arr age_
        void $ recPrimSerializeAt i7 arr (i_i32 i15 :: Int32)
        i16 <- recPrimSerializeAt i15 arr height_
        void $ recPrimSerializeAt i9 arr (i_i32 i16 :: Int32)
        i17 <- recPrimSerializeAt i16 arr isMarried_
        void $ recPrimSerializeAt i11 arr (i_i32 i17 :: Int32)
        i18 <- recPrimSerializeAt i17 arr name_
        void $ recPrimSerializeAt i13 arr (i_i32 i18 :: Int32)
        i19 <- recPrimSerializeAt i18 arr address_
        pure $ Record True $ Array arr 0 i19

instance HasField (Proxy "age") (Record User) Int where

    getField :: Proxy "age" -> Record User -> Int
    getField _ (Record True arr) = getFieldTrustedStatic (offsetMessageBody 59) arr
    getField _ (Record False arr) =
        fromJust $ getFieldUntrusted 59 (encodeSimpleString "age") arr

instance HasField (Proxy "height") (Record User) Double where

    getField :: Proxy "height" -> Record User -> Double
    getField _ (Record True arr) = getFieldTrustedStatic ((offsetMessageBody 59) + 8) arr
    getField _ (Record False arr) =
        fromJust $ getFieldUntrusted 59 (encodeSimpleString "height") arr

instance HasField (Proxy "isMarried") (Record User) Bool where

    getField :: Proxy "isMarried" -> Record User -> Bool
    getField _ (Record True arr) = getFieldTrustedStatic ((offsetMessageBody 59) + 16) arr
    getField _ (Record False arr) =
        fromJust $ getFieldUntrusted 59 (encodeSimpleString "isMarried") arr

instance HasField (Proxy "name") (Record User) String where

    getField :: Proxy "name" -> Record User -> String
    getField _ (Record True arr) = getFieldTrustedStatic ((offsetMessageBody 59) + 17) arr
    getField _ (Record False arr) =
        fromJust $ getFieldUntrusted 59 (encodeSimpleString "name") arr

instance HasField (Proxy "address") (Record User) (Record Address) where

    getField :: Proxy "address" -> Record User -> (Record Address)
    getField _ (Record True arr) = getFieldTrustedDynamic (offsetHeaderBody + 55) arr
    getField _ (Record False arr) =
        fromJust $ getFieldUntrusted 59 (encodeSimpleString "address") arr

--------------------------------------------------------------------------------
-- Example
--------------------------------------------------------------------------------

addressRecord :: Record Address
addressRecord =
    createRecord $ Address
        { zipCode = 123456
        , country = Just (Just Nothing)
        }

userRecord :: Record User
userRecord =
    createRecord $ User
        { name = "Adithya"
        , age = 26
        , height = 183.5
        , isMarried = True
        , address = addressRecord
        }

main :: IO ()
main = do
    print $ getField @(Proxy "zipCode") Proxy addressRecord
    print $ getField @(Proxy "country") Proxy addressRecord

    print $ getField @(Proxy "name") Proxy userRecord
    print $ getField @(Proxy "age") Proxy userRecord
    print $ getField @(Proxy "height") Proxy userRecord
    print $ getField @(Proxy "isMarried") Proxy userRecord

    print
        $ getField @(Proxy "zipCode") Proxy
        $ getField @(Proxy "address") Proxy userRecord

    print
        $ getField @(Proxy "country") Proxy
        $ getField @(Proxy "address") Proxy userRecord

    putStrLn ""

    print $ getField @(Proxy "zipCode") Proxy $ breakTrust addressRecord
    print $ getField @(Proxy "country") Proxy $ breakTrust addressRecord

    print $ getField @(Proxy "name") Proxy $ breakTrust userRecord
    print $ getField @(Proxy "age") Proxy $ breakTrust userRecord
    print $ getField @(Proxy "height") Proxy $ breakTrust userRecord
    print $ getField @(Proxy "isMarried") Proxy $ breakTrust userRecord

    print
        $ getField @(Proxy "zipCode") Proxy
        $ breakTrust $ getField @(Proxy "address") Proxy $ breakTrust userRecord

    print
        $ getField @(Proxy "country") Proxy
        $ breakTrust $ getField @(Proxy "address") Proxy $ breakTrust userRecord
