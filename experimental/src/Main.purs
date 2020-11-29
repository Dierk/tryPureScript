module Main where
  
import Prelude

import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Console (logShow)
import Effect.Exception (throw)
import Web.DOM.Element (toNode)
import Web.DOM.Internal.Types (Element)
import Web.DOM.Node (Node, setTextContent)
import Web.DOM.NonElementParentNode (getElementById)
import Web.Event.Event (EventType(..), Event)
import Web.Event.EventTarget (addEventListener, eventListener)
import Web.HTML (window)
import Web.HTML.HTMLDocument (toNonElementParentNode)
import Web.HTML.HTMLInputElement (HTMLInputElement, fromElement, setValue, value, toEventTarget)
import Web.HTML.Window (document)

foreign import setInnerText :: Element -> String -> Effect Unit

findIdInDoc :: String -> Effect (Maybe Element) 
findIdInDoc id = do
    win          <- window
    doc          <- document win
    getElementById id (toNonElementParentNode doc)


getInputElement :: Effect HTMLInputElement
getInputElement = do
    foo <- findIdInDoc "inText"
    case foo of
        Nothing      -> throw "no 'inText' node!"
        Just element -> case fromElement element of 
            Nothing     -> throw "inText is not an input element"
            Just input  -> pure input       

getNode :: Effect Node
getNode = do 
    foo <- findIdInDoc "outText"
    case foo of
        Nothing      -> throw "no 'outText' node!"
        Just element -> pure (toNode element) 

onInput :: forall a. HTMLInputElement -> (Event -> Effect a) -> Effect Unit
onInput inputElement listener =  do
    foo <- eventListener listener     
    addEventListener (EventType "input") foo false (toEventTarget inputElement)


main :: Effect Unit
main = do
    inText  <- getInputElement
    outText <- getNode

    onInput inText \evt -> do
        val <- value inText 
        setTextContent val outText
