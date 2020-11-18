module Data.AddressBook.Validation where

import Prelude

import Data.AddressBook (Address, Person, PhoneNumber, PhoneType(..), address, person, phoneNumber)
import Data.Either (Either(..))
import Data.String (length)
import Data.String.Regex (Regex, test, regex)
import Data.String.Regex.Flags (noFlags)
import Data.Traversable (traverse)
import Data.Validation.Semigroup (V, unV, invalid)
import Partial.Unsafe (unsafePartial)

data Field = FirstNameField
           | LastNameField
           | StreetField
           | CityField
           | StateField
           | PhoneField PhoneType

instance showField :: Show Field where
  show :: Field -> String
  show FirstNameField = "First Name"
  show LastNameField = "Last Name"
  show StreetField = "Street"
  show CityField = "City"
  show StateField = "State"
  show (PhoneField NoPhone) = "Phone Numbers"
  show (PhoneField pt) = show pt

instance eqField :: Eq Field where
  eq :: Field -> Field -> Boolean
  eq FirstNameField FirstNameField = true
  eq LastNameField LastNameField = true
  eq StreetField StreetField = true
  eq CityField CityField = true
  eq StateField StateField = true
  eq (PhoneField pt1) (PhoneField pt2) = pt1 == pt2
  eq _ _ = false


data ValidationError = ValidationError String Field

type Errors
  = Array ValidationError

nonEmpty :: Field -> String -> V Errors Unit
nonEmpty field "" = invalid [ ValidationError ("Field '" <> show field <> "' cannot be empty") field ]
nonEmpty _ _ = pure unit

arrayNonEmpty :: forall a. Field -> Array a -> V Errors Unit
arrayNonEmpty field [] = invalid [ ValidationError ("Fields '" <> show field <> "' must contain at least one value") field ]
arrayNonEmpty _ _ = pure unit

lengthIs :: Field -> Int -> String -> V Errors Unit
lengthIs field len value
  | length value /= len = invalid [ ValidationError ("Field '" <> show field <> "' must have length " <> show len) field ]
lengthIs _ _ _ = pure unit

phoneNumberRegex :: Regex
phoneNumberRegex =
  unsafePartial case regex "^\\d{3}-\\d{3}-\\d{4}$" noFlags of
    Right r -> r

matches :: Field -> Regex -> String -> V Errors Unit
matches _ regex value
  | test regex value = pure unit
matches field _ _ = invalid [ ValidationError ("Field '" <> show field <> "' did not match the required format") field ]

validateAddress :: Address -> V Errors Address
validateAddress a =
  address <$> (nonEmpty StreetField a.street *> pure a.street)
    <*> (nonEmpty CityField a.city *> pure a.city)
    <*> (lengthIs StateField 2 a.state *> pure a.state)

validatePhoneNumber :: PhoneNumber -> V Errors PhoneNumber
validatePhoneNumber pn =
  phoneNumber <$> pure pn."type"
    <*> (matches (PhoneField pn."type") phoneNumberRegex pn.number *> pure pn.number)

validatePerson :: Person -> V Errors Person
validatePerson p =
  person <$> (nonEmpty FirstNameField p.firstName *> pure p.firstName)
    <*> (nonEmpty LastNameField p.lastName *> pure p.lastName)
    <*> validateAddress p.homeAddress
    <*> (arrayNonEmpty (PhoneField NoPhone) p.phones *> traverse validatePhoneNumber p.phones)

validatePerson' :: Person -> Either Errors Person
validatePerson' p = unV Left Right $ validatePerson p
