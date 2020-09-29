module Test.MySolutions where

import Prelude

import Data.AddressBook (Entry, AddressBook)
import Data.List (filter, head, nubBy, null)
import Data.Maybe (Maybe)

findEntryByStreet :: String -> AddressBook -> Maybe Entry
-- 1. function composition
-- 2. book has been eta reduced
findEntryByStreet street = head <<< filter hasStreetName
  where
  hasStreetName :: Entry -> Boolean
  hasStreetName entry = entry.address.street == street

isInBook :: String -> String -> AddressBook -> Boolean
-- 1. function composition
-- 2. book has been eta reduced
isInBook first last = not null <<< filter hasFirstAndLastName
  where
  hasFirstAndLastName :: Entry -> Boolean
  hasFirstAndLastName entry = entry.firstName == first && entry.lastName == last

removeDuplicates :: AddressBook -> AddressBook
-- book has been eta reduced
removeDuplicates = nubBy compareFirstAndLastNames
  where
  compareFirstAndLastNames :: Entry -> Entry -> Boolean
  compareFirstAndLastNames e1 e2 = e1.firstName == e2.firstName && e1.lastName == e2.lastName