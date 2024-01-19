{-# OPTIONS_GHC -Wno-orphans #-}

module Record.Example where

import Record.Types
import Data.Proxy (Proxy(..))
import System.IO.Unsafe (unsafePerformIO)
import qualified Streamly.Data.Stream as Stream
import qualified Streamly.Unicode.Stream as Encoding
import qualified Streamly.Internal.Data.Array as Array
import qualified Streamly.Unicode.Stream as Unicode

instance IsValue String where
    toValue =
        unsafePerformIO
            . fmap Text
            . Array.fromStream
            . Unicode.encodeUtf8'
            . Stream.fromList
    fromValue (Text arr) =
        unsafePerformIO
            $ Stream.toList
            $ Unicode.decodeUtf8'
            $ Array.read arr
    fromValue _ = error "fromValue: String"

instance IsValue (Record Address) where
    toValue (Record arr) = Rec arr
    fromValue (Rec arr) = Record arr
    fromValue _ = error "fromValue: Record Address"

instance IsValue Int where
    toValue = R_Int
    fromValue (R_Int val) = val
    fromValue _ = error "fromValue: Int"

instance IsValue Double where
    toValue = R_Double
    fromValue (R_Double val) = val
    fromValue _ = error "fromValue: Double"

instance IsValue Bool where
    toValue = R_Bool
    fromValue (R_Bool val) = val
    fromValue _ = error "fromValue: Bool"

data Address = Address
    { line1 :: String
    , line2 :: String
    , zipCode :: Int
    , city :: String
    , country :: String
    }

data User = User
    { name :: String
    , age :: Int
    , height :: Double
    , isMarried :: Bool
    , address :: Record Address
    }

instance TypeHash (Record User) where
    typeHash _ =
        let val = "name:age:height:isMarried"
            n = length val
         in unsafePerformIO
                $ Array.fromStreamN n
                $ Encoding.encodeLatin1'
                $ Stream.fromList val

instance HasField (Proxy "name") (Record User) String where
    getField :: Proxy "name" -> Record User -> String
    getField _ _ = error "Unimplemented"

instance FieldPosition (Proxy "name") (Record User) where
    icFieldIndex :: Proxy "name" -> Record User -> Int
    icFieldIndex _ _ = 0

userRecord :: Record User
userRecord = error "Unimplemented"

nameField :: String
nameField = getField @(Proxy "name") Proxy userRecord
