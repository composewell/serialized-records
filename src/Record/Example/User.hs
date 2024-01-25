{-# OPTIONS_GHC -Wno-orphans #-}

module Record.Example.User where

import Record.Example.Address
import Record.Types
import Record.TH

#define DERIVE_TH(x) $(deriveSerializableInstances $(expTypeHash $(expRecordMeta ''x) ''x) $(expRecordMeta ''x) ''x)

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

DERIVE_TH(User)

{-
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
        encodeSimpleString "age" <> recPrimHash (toValueProxy (Proxy :: Proxy Int))
        <> encodeSimpleString "height" <> recPrimHash (toValueProxy (Proxy :: Proxy Double))
        <> encodeSimpleString "isMarried" <> recPrimHash (toValueProxy (Proxy :: Proxy Bool))
        <> encodeSimpleString "address" <> recPrimHash (toValueProxy (Proxy :: Proxy (Record Address)))
        <> encodeSimpleString "name" <> recPrimHash (toValueProxy (Proxy :: Proxy String))
        <> Array.fromList (replicate 32 0)

    recStaticSize _ = Nothing

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
        (i5, i6) <- unsafePutHeaderKey i4 arr "age"
        (i7, i8) <- unsafePutHeaderKey i6 arr "height"
        (i9, i10) <- unsafePutHeaderKey i8 arr "isMarried"
        (i11, i12) <- unsafePutHeaderKey i10 arr "address"
        (i13, i14) <- unsafePutHeaderKey i12 arr "name"
        void $ recPrimSerializeAt i5 arr (i_i32 i14 :: Int32)
        i15 <- recPrimSerializeAt i14 arr age_
        void $ recPrimSerializeAt i7 arr (i_i32 i15 :: Int32)
        i16 <- recPrimSerializeAt i15 arr height_
        void $ recPrimSerializeAt i9 arr (i_i32 i16 :: Int32)
        i17 <- recPrimSerializeAt i16 arr isMarried_
        void $ recPrimSerializeAt i11 arr (i_i32 i17 :: Int32)
        i18 <- recPrimSerializeAt i17 arr address_
        void $ recPrimSerializeAt i13 arr (i_i32 i18 :: Int32)
        i19 <- recPrimSerializeAt i18 arr name_
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
    getField _ (Record True arr) = getFieldTrustedDynamic (offsetHeaderBody + 55) arr
    getField _ (Record False arr) =
        fromJust $ getFieldUntrusted 59 (encodeSimpleString "name") arr

instance HasField (Proxy "address") (Record User) (Record Address) where

    getField :: Proxy "address" -> Record User -> (Record Address)
    getField _ (Record True arr) = getFieldTrustedStatic ((offsetMessageBody 59) + 17) arr
    getField _ (Record False arr) =
        fromJust $ getFieldUntrusted 59 (encodeSimpleString "address") arr
-}
