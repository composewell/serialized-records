{-# OPTIONS_GHC -Wno-orphans #-}

module Record.Example.Address where

import Record.TH

#define DERIVE_TH(x) $(deriveSerializableInstances $(expTypeHash $(expRecordMeta ''x) ''x) $(expRecordMeta ''x) ''x)

--------------------------------------------------------------------------------
-- Address
--------------------------------------------------------------------------------

data Address = Address
    { zipCode :: Int
    , country :: Maybe (Maybe (Maybe String))
    }

DERIVE_TH(Address)

{-
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

    recStaticSize _ = Nothing

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
        (i5, i6) <- unsafePutHeaderKey i4 arr "zipCode"
        (i7, i8) <- unsafePutHeaderKey i6 arr "country"
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
-}
