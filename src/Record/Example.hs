{-# OPTIONS_GHC -Wno-orphans #-}

module Record.Example where

import Record.Types

import Record.Example.Address
import Record.Example.User

--------------------------------------------------------------------------------
-- Example
--------------------------------------------------------------------------------

addressR :: Address
addressR =
    Address
        { zipCode = 123456
        , country = Just (Just (Just "India"))
        }

addressRecord :: Record Address
addressRecord = createRecord addressR

userR :: User String Int Double Bool (R Address)
userR =
    User
        { name = "Adithya"
        , age = 26
        , height = 183.5
        , isMarried = True
        , address = R addressR
        }

userRecord :: Record (User String Int Double Bool (R Address))
userRecord = createRecord userR

main :: IO ()
main = do

    let parsedUser = parseRecord userRecord
        parsedAddress = parseRecord addressRecord
        parsedAddressU = parseRecord $ breakTrust $ addressRecord
        parsedUserU = parseRecord $ breakTrust $ userRecord
        parsedAddressI = unR $ address parsedUser
        parsedAddressIU = unR $ address $ parsedUserU

    print $ zipCode parsedAddressI
    print $ country parsedAddressI

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

    print $ zipCode parsedAddressIU
    print $ country parsedAddressIU
