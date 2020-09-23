module Test.MySolutions where

import Prelude

import Control.Extend (duplicate)
import Data.AddressBook (AddressBook, Entry, findEntry)
import Data.List (filter, head, null, nubBy)
import Data.Maybe (Maybe)

-- Note to reader: Add your solutions to this file

--findEntry :: String -> String -> AddressBook -> Maybe Entry

findEntryByStreet :: String -> AddressBook -> Maybe Entry
findEntryByStreet street = head <<< filter filterEntry
    where
      filterEntry :: Entry -> Boolean
      filterEntry entry = entry.address.street == street

-- not efficient
isInBook :: String -> String -> AddressBook -> Boolean
isInBook firstName lastName = not null <<< filter filterEntry
    where
        filterEntry :: Entry -> Boolean
        filterEntry entry = entry.firstName == firstName && entry.lastName == lastName


removeDuplicates :: AddressBook -> AddressBook
removeDuplicates book = nubBy duplicate book
    where
        duplicate :: Entry -> Entry -> Boolean
        duplicate e1 e2 = e1.firstName == e2.firstName && e1.lastName == e2.lastName
