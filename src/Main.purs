module Main where
  
import Prelude
import Effect (Effect)
import Data.Maybe (Maybe(..))
 
import Effect.Exception (throw)

import Web.HTML (window)
import Web.HTML.Window (document)
import Web.HTML.HTMLDocument (toNonElementParentNode)
import Web.DOM.NonElementParentNode (getElementById)
import Web.DOM.Node(setTextContent)
import Web.DOM.Element(toNode)

import Web.DOM.Internal.Types (Element)
foreign import setInnerText :: Element -> String -> Effect Unit

main :: Effect Unit
main = do
    document  <- map toNonElementParentNode (window >>= document)
    container <- getElementById "container" document
    case container of
        Nothing      -> throw "no 'container' node!"
        Just element -> setTextContent  "new Text!!!" (toNode element) 
