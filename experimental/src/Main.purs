module Main where
  
import Prelude

import Data.Maybe (Maybe(..))
import Effect (Effect)
-- import Effect.Console (logShow)
import Effect.Exception (throw)
import Observable (newObservable', onChange', setValue)
import Web.DOM.Element (toNode)
import Web.DOM.Internal.Types (Element)
import Web.DOM.Node (Node, setTextContent)
import Web.DOM.NonElementParentNode (getElementById)
import Web.Event.Event (EventType(..), Event)
import Web.Event.EventTarget (addEventListener, eventListener)
import Web.HTML (window)
import Web.HTML.HTMLDocument (toNonElementParentNode)
import Web.HTML.HTMLInputElement (HTMLInputElement, fromElement, value, toEventTarget)
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
    callback <- eventListener listener     
    addEventListener (EventType "input") callback false (toEventTarget inputElement)


main :: Effect Unit
main = do
    inText  <- getInputElement
    outText <- getNode

    obs <- newObservable' ""
        >>= onChange' \val -> setTextContent val outText

    onInput inText \_ ->
        value inText >>= flip setValue obs

    -- since Obs.setValue and onChange create new Observables, there are a number of
    -- issues to consider. One might need an Effect.Ref to keep track.

{-  old version, with view coupling: inText knows about outText
    onInput inText \evt -> do
        val <- value inText 
        setTextContent val outText
-}