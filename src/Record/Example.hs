{-# OPTIONS_GHC -Wno-orphans #-}

module Record.Example where

import Record.Types

import Data.Proxy (Proxy(..))

import Record.Example.Address
import Record.Example.User

--------------------------------------------------------------------------------
-- Example
--------------------------------------------------------------------------------

addressRecord :: Record Address
addressRecord =
    createRecord $ Address
        { zipCode = 123456
        , country = Just (Just (Just "India"))
        }

hsUser :: User String Int Double Bool (Record Address)
hsUser =
    User
        { name = "Adithya"
        , age = 26
        , height = 183.5
        , isMarried = True
        , address = addressRecord
        }

userRecord :: Record (User String Int Double Bool (Record Address))
userRecord = createRecord hsUser

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
