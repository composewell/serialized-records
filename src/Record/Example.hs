{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE QuasiQuotes #-}

module Record.Example where

import Record.Types

import Data.Maybe (fromJust)
import Data.Proxy (Proxy(..))

import qualified Streamly.Internal.Data.Array as Array

--------------------------------------------------------------------------------
-- Address
--------------------------------------------------------------------------------

data Address = Address
    { zipCode :: Int
    , country :: String
    }

-- This should be derived via template-haskell helpers
instance RecordMeta (Record Address) where
    --   2
    -- + 2 + length "zipCode" + 4 -- 13
    -- + 2 + length "country" + 4 -- 13
    -- headerLength _ = 28

    -- TODO: Use xxHash here and use a builder like combination
    typeHash _ = Array.getSliceUnsafe 0 32 $
           encodeSimpleString "zipCode" <> recPrimHash (toValueProxy (Proxy :: Proxy Int))
        <> encodeSimpleString "country" <> recPrimHash (toValueProxy (Proxy :: Proxy String))
        <> Array.fromList (replicate 32 0)

instance RecordMeta (Record Address) => HasField (Proxy "zipCode") (Record Address) Int where

    getField :: Proxy "zipCode" -> Record Address -> Int
    getField _ (Record True arr) = getFieldTrustedStatic (offsetMessageBody 28) arr
    getField _ (Record False arr) =
        fromJust $ getFieldUntrusted 28 (encodeSimpleString "zipCode") arr

instance RecordMeta (Record Address) => HasField (Proxy "country") (Record Address) String where

    getField :: Proxy "country" -> Record Address -> String
    getField _ (Record True arr) = getFieldTrustedStatic ((offsetMessageBody 28) + 8) arr
    getField _ (Record False arr) =
        fromJust $ getFieldUntrusted 28 (encodeSimpleString "country") arr

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
instance RecordMeta (Record User) where
    --   2
    -- + 2 + length "name"      + 4 -- 10
    -- + 2 + length "age"       + 4 -- 9
    -- + 2 + length "height"    + 4 -- 12
    -- + 2 + length "isMarried" + 4 -- 15
    -- + 2 + length "address"   + 4 -- 13
    -- headerLength _ = 61

    -- TODO: Use xxHash here and use a builder like combination
    typeHash _ = Array.getSliceUnsafe 0 32 $
           encodeSimpleString "name" <> recPrimHash (toValueProxy (Proxy :: Proxy String))
        <> encodeSimpleString "age" <> recPrimHash (toValueProxy (Proxy :: Proxy Int))
        <> encodeSimpleString "height" <> recPrimHash (toValueProxy (Proxy :: Proxy Double))
        <> encodeSimpleString "isMarried" <> recPrimHash (toValueProxy (Proxy :: Proxy Bool))
        <> encodeSimpleString "address" <> recPrimHash (toValueProxy (Proxy :: Proxy (Record Address)))
        <> Array.fromList (replicate 32 0)

instance HasField (Proxy "age") (Record User) Int where

    getField :: Proxy "age" -> Record User -> Int
    getField _ (Record True arr) = getFieldTrustedStatic (offsetMessageBody 61) arr
    getField _ (Record False arr) =
        fromJust $ getFieldUntrusted 28 (encodeSimpleString "age") arr

instance HasField (Proxy "height") (Record User) Double where

    getField :: Proxy "height" -> Record User -> Double
    getField _ (Record True arr) = getFieldTrustedStatic ((offsetMessageBody 61) + 8) arr
    getField _ (Record False arr) =
        fromJust $ getFieldUntrusted 28 (encodeSimpleString "height") arr

instance HasField (Proxy "isMarried") (Record User) Bool where

    getField :: Proxy "isMarried" -> Record User -> Bool
    getField _ (Record True arr) = getFieldTrustedStatic ((offsetMessageBody 61) + 16) arr
    getField _ (Record False arr) =
        fromJust $ getFieldUntrusted 28 (encodeSimpleString "isMarried") arr

instance HasField (Proxy "name") (Record User) String where

    getField :: Proxy "name" -> Record User -> String
    getField _ (Record True arr) = getFieldTrustedStatic ((offsetMessageBody 61) + 17) arr
    getField _ (Record False arr) =
        fromJust $ getFieldUntrusted 28 (encodeSimpleString "name") arr

instance HasField (Proxy "address") (Record User) (Record Address) where

    getField :: Proxy "address" -> Record User -> (Record Address)
    getField _ (Record True arr) = getFieldTrustedDynamic (offsetHeaderBody + 55) arr
    getField _ (Record False arr) =
        fromJust $ getFieldUntrusted 28 (encodeSimpleString "address") arr

--------------------------------------------------------------------------------
-- Example
--------------------------------------------------------------------------------

userRecord :: Record User
userRecord = error "Unimplemented"

nameField :: String
nameField = getField @(Proxy "name") Proxy userRecord
