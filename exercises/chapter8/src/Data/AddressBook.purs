module Data.AddressBook where

import Prelude

type Address
  = { street :: String
    , city :: String
    , state :: String
    }

address :: String -> String -> String -> Address
address street city state = { street, city, state }

data PhoneType
  = HomePhone
  | WorkPhone
  | CellPhone
  | OtherPhone

instance showPhoneType :: Show PhoneType where
  show HomePhone = "HomePhone"
  show WorkPhone = "WorkPhone"
  show CellPhone = "CellPhone"
  show OtherPhone = "OtherPhone"

instance eqPhoneType :: Eq PhoneType where
  eq :: PhoneType -> PhoneType -> Boolean
  eq HomePhone HomePhone = true
  eq WorkPhone WorkPhone = true
  eq CellPhone CellPhone = true
  eq OtherPhone OtherPhone = true
  eq _ _ = false

type PhoneNumber
  = { "type" :: PhoneType
    , number :: String
    }

phoneNumber :: PhoneType -> String -> PhoneNumber
phoneNumber ty number =
  { "type": ty
  , number: number
  }

type Person
  = { firstName :: String
    , lastName :: String
    , homeAddress :: Address
    , phones :: Array PhoneNumber
    }

person :: String -> String -> Address -> Array PhoneNumber -> Person
person firstName lastName homeAddress phones = { firstName, lastName, homeAddress, phones }

examplePerson :: Person
examplePerson =
  person "John" "Smith"
    (address "123 Fake St." "FakeTown" "CA")
    [ phoneNumber HomePhone "555-555-5555"
    , phoneNumber CellPhone "555-555-0000"
    , phoneNumber WorkPhone "555-555-5511"  
    ]
