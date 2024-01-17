module Record.Example where

import Record.Types
import Data.Proxy (Proxy(..))
import System.IO.Unsafe (unsafePerformIO)
import qualified Streamly.Data.Stream as Stream
import qualified Streamly.Unicode.Stream as Encoding
import qualified Streamly.Internal.Data.Array as Array

data User = User
    { name :: String
    , age :: Int
    , height :: Double
    , isMarried :: Bool
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
    fieldPosition :: Proxy "name" -> Record User -> Int
    fieldPosition _ _ = 0

userRecord :: Record User
userRecord = error "Unimplemented"

nameField :: String
nameField = getField @(Proxy "name") Proxy userRecord
