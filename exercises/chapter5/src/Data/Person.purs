module Data.Person where

import Prelude

type Address = { street :: String, city :: String }

type Person = { name :: String, address :: Address }

sameCity :: Person -> Person -> Boolean
sameCity {address: addr1} {address: addr2} = addr1.city == addr2.city