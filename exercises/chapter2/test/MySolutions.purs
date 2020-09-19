module Test.MySolutions where

import Prelude
import Math (sqrt, pi, e)
import Global (readFloat)

diagonal anKathete gegenKathete = sqrt (anKathete * anKathete + gegenKathete * gegenKathete)

circleArea radius = radius * radius * pi

addE num = e + readFloat num