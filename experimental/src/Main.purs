module Main where
  
import Prelude

import Data.Maybe (Maybe(..))
--import Data.String (toUpper)
import Data.String.CodeUnits (length)
import Effect (Effect)
--import Effect.Console (log)
import Effect.Exception (throw)
import Effect.Ref as Ref
import Observable (Observable, newObservable, onChange, setValue)
import Web.DOM.Element (toNode)
import Web.DOM.Internal.Types (Element)
import Web.DOM.Node (Node, setTextContent)
import Web.DOM.NonElementParentNode (getElementById)
import Web.DOM.DOMTokenList (add, remove)
import Web.Event.Event (EventType(..), Event)
import Web.Event.EventTarget (addEventListener, eventListener)
import Web.HTML (window)
import Web.HTML.HTMLDocument (toNonElementParentNode)
import Web.HTML.HTMLInputElement (HTMLInputElement, fromElement, value, toEventTarget, toHTMLElement)
import Web.HTML.HTMLInputElement (setValue) as Input
import Web.HTML.HTMLElement (classList)
import Web.HTML.Window (document)

foreign import setInnerText :: Element -> String -> Effect Unit

findIdInDoc :: String -> Effect (Maybe Element) 
findIdInDoc id = do
    win          <- window
    doc          <- document win
    getElementById id (toNonElementParentNode doc)


getInputElement :: String -> Effect HTMLInputElement
getInputElement id = do
    candidate <- findIdInDoc id
    case candidate of
        Nothing      -> throw $ "no element with id '" <> id <> "' in document!"
        Just element -> case fromElement element of 
            Nothing     -> throw $ "element with id '" <> id <> "' is not an input element!"
            Just input  -> pure input       

getNodeById :: String -> Effect Node
getNodeById id = do 
    candidate <- findIdInDoc id
    case candidate of
        Nothing      -> throw $ "no node with id ¨" <> id <> "' in document!"
        Just element -> pure (toNode element) 

onInput :: forall a. HTMLInputElement -> (Event -> Effect a) -> Effect Unit
onInput inputElement listener =  do
    callback <- eventListener listener     
    addEventListener (EventType "input") callback false (toEventTarget inputElement)

setRefValue :: forall a. Eq a => Ref.Ref (Observable a) -> a -> Effect Unit
setRefValue obsRef val = do
    obs    <- Ref.read obsRef
    newObs <- setValue val obs
    Ref.write newObs obsRef

withObservable :: forall a. Ref.Ref (Observable a) -> (Observable a -> Observable a) -> Effect Unit
withObservable obsRef doit = 
    Ref.modify_ doit obsRef

main :: Effect Unit
main = do
    obsRef   <- Ref.new $ newObservable ""    -- start of some rich attributes
    validRef <- Ref.new $ newObservable true

    -- section inText
    inputElement <- getInputElement "inText"
    -- view binding
    onInput inputElement \_ ->
        value inputElement >>= setRefValue obsRef
    -- data binding
    withObservable obsRef   $ onChange \value ->
        Input.setValue value inputElement
    withObservable validRef $ onChange \value -> do
        classNames <- classList $ toHTMLElement inputElement
        (if value then remove else add) classNames "invalid"
    -- some validation
    withObservable obsRef $ onChange \value ->
        setRefValue validRef (length value > 3)
    -- here, some conversion is missing

    -- section outText
    outTextElement <- getNodeById "outText"
    withObservable obsRef $ onChange \value -> 
        setTextContent value outTextElement

    -- section charCount
    charCountElement <- getNodeById "chars"
    withObservable obsRef $ onChange \value ->
        setTextContent (show $ length value) charCountElement



