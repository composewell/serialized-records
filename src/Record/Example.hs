{-# OPTIONS_GHC -Wno-orphans #-}

module Record.Example where

import Record.Types

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

    let parsedUser = parseRecord userRecord
        parsedAddress = parseRecord addressRecord
        parsedAddressU = parseRecord $ breakTrust $ addressRecord
        parsedUserU = parseRecord $ breakTrust $ userRecord
        parsedAddressI = parseRecord $ address parsedUser
        parsedAddressIU =
            parseRecord $ breakTrust $ address $ parsedUserU

    print $ zipCode parsedAddress
    print $ country parsedAddress

    print $ name parsedUser
    print $ age parsedUser
    print $ height parsedUser
    print $ isMarried parsedUser

    print $ zipCode parsedAddressU
    print $ country parsedAddressU

    print $ name parsedUserU
    print $ age parsedUserU
    print $ height parsedUserU
    print $ isMarried parsedUserU

    print $ zipCode parsedAddressI
    print $ country parsedAddressI

    print $ zipCode parsedAddressIU
    print $ country parsedAddressIU
