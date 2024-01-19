{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE QuasiQuotes #-}

module Record.Example where

import Record.Types
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
    -- + 2 + length "zipCode"
    -- + 2 + length "country"
    headerLength _ = 18
    -- TODO: Use xxHash here and use a builder like combination
    typeHash _ = Array.getSliceUnsafe 0 32 $
           encodeSimpleString "zipCode" <> recPrimHash (valueType (Proxy :: Proxy Int))
        <> encodeSimpleString "country" <> recPrimHash (valueType (Proxy :: Proxy String))
        <> Array.fromList (replicate 32 0)

instance RecordMeta (Record Address) => HasField (Proxy "zipCode") (Record Address) Int where

    fieldPosition :: Proxy "zipCode" -> Record Address -> FieldPos
    fieldPosition _ _ = ICValue (offsetMessageBody (headerLength (Proxy :: Proxy (Record Address))))

    getField :: Proxy "zipCode" -> Record Address -> Int
    getField _ _ = error "Unimplemented"

instance RecordMeta (Record Address) => HasField (Proxy "country") (Record Address) String where

    fieldPosition :: Proxy "country" -> Record Address -> FieldPos
    fieldPosition _ _ = ICValue (offsetMessageBody (headerLength (Proxy :: Proxy (Record Address)) + 8))

    getField :: Proxy "country" -> Record Address -> String
    getField _ _ = error "Unimplemented"

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
    -- + 2 + length "name"      -- 6
    -- + 2 + length "age"       -- 5
    -- + 2 + length "height"    -- 8
    -- + 2 + length "isMarried" -- 11
    -- + 2 + length "address"   -- 9
    headerLength _ = 41
    -- TODO: Use xxHash here and use a builder like combination
    typeHash _ = Array.getSliceUnsafe 0 32 $
           encodeSimpleString "name" <> recPrimHash (valueType (Proxy :: Proxy String))
        <> encodeSimpleString "age" <> recPrimHash (valueType (Proxy :: Proxy Int))
        <> encodeSimpleString "height" <> recPrimHash (valueType (Proxy :: Proxy Double))
        <> encodeSimpleString "isMarried" <> recPrimHash (valueType (Proxy :: Proxy Bool))
        <> encodeSimpleString "address" <> recPrimHash (valueType (Proxy :: Proxy (Record Address)))
        <> Array.fromList (replicate 32 0)

instance HasField (Proxy "age") (Record User) Int where

    fieldPosition :: Proxy "age" -> Record User -> FieldPos
    fieldPosition _ _ = ICValue (offsetMessageBody (headerLength (Proxy :: Proxy (Record User))))

    getField :: Proxy "age" -> Record User -> Int
    getField _ _ = error "Unimplemented"

instance HasField (Proxy "height") (Record User) Double where

    fieldPosition :: Proxy "height" -> Record User -> FieldPos
    fieldPosition _ _ = ICValue (offsetMessageBody (headerLength (Proxy :: Proxy (Record User)) + 8))

    getField :: Proxy "height" -> Record User -> Double
    getField _ _ = error "Unimplemented"

instance HasField (Proxy "isMarried") (Record User) Bool where

    fieldPosition :: Proxy "isMarried" -> Record User -> FieldPos
    fieldPosition _ _ = ICValue (offsetMessageBody (headerLength (Proxy :: Proxy (Record User)) + 12))

    getField :: Proxy "isMarried" -> Record User -> Bool
    getField _ _ = error "Unimplemented"

instance HasField (Proxy "name") (Record User) String where

    fieldPosition :: Proxy "name" -> Record User -> FieldPos
    fieldPosition _ _ = ICValue (offsetMessageBody (headerLength (Proxy :: Proxy (Record User)) + 13))

    getField :: Proxy "name" -> Record User -> String
    getField _ _ = error "Unimplemented"

instance HasField (Proxy "address") (Record User) (Record Address) where

    fieldPosition :: Proxy "address" -> Record User -> FieldPos
    fieldPosition _ _ = ICOffsetToValue (offsetHeaderBody + 30)

    getField :: Proxy "address" -> Record User -> (Record Address)
    getField _ _ = error "Unimplemented"

--------------------------------------------------------------------------------
-- Example
--------------------------------------------------------------------------------

userRecord :: Record User
userRecord = error "Unimplemented"

nameField :: String
nameField = getField @(Proxy "name") Proxy userRecord
