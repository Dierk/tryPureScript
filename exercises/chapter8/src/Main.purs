module Main where

import Prelude
import Data.AddressBook (PhoneNumber, PhoneType(..), examplePerson)
import Data.AddressBook.Validation (Errors, validatePerson', ValidationError(..), Field(..))
import Data.Array (mapWithIndex, updateAt, filter, concat)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Console (log)
import Effect.Exception (throw)
import React.Basic.DOM as D
import React.Basic.DOM.Events (targetValue)
import React.Basic.Events (handler)
import React.Basic.Hooks (ReactComponent, element, reactComponent, useState)
import React.Basic.Hooks as R
import Web.DOM.NonElementParentNode (getElementById)
import Web.HTML (window)
import Web.HTML.HTMLDocument (toNonElementParentNode)
import Web.HTML.Window (document)

-- Note that there's a Purty formatting bug that
-- adds an unwanted blank line
-- https://gitlab.com/joneshf/purty/issues/77
renderValidationErrors :: Errors -> Array R.JSX
renderValidationErrors [] = []

renderValidationErrors xs =
  [ D.div
      { className: "col-sm-4"
      , children: [ D.div_ $ renderError <$> xs ]
      }
  ]

renderError :: ValidationError -> R.JSX
renderError (ValidationError e _) =
  D.div
    { className: "alert alert-danger"
    , children: [ D.text e ]
    }

extractErrors :: Field -> Errors -> Errors
extractErrors f = filter \(ValidationError _ f') -> f == f'

-- Helper function to render a single form field with an
-- event handler to update
formField :: Field -> String -> String -> Errors -> (String -> Effect Unit) -> Array R.JSX
formField field placeholder value errors setValue =
  [ D.div
      { className: "form-group row"
      , children:
          [ D.label
              { className: "col-sm-2 col-form-label"
              , children: [ D.text $ show field ]
              }
          , D.div
              { className: "col-sm-4"
              , children:
                  [ D.input
                      { className: "form-control"
                      , placeholder
                      , value
                      , onChange:
                          let
                            handleValue :: Maybe String -> Effect Unit
                            handleValue (Just v) = setValue v

                            handleValue Nothing = pure unit
                          in
                            handler targetValue handleValue
                      }
                  ]
              }
          ]
            <> renderValidationErrors (extractErrors field errors)
      }
  ]

mkAddressBookApp :: Effect (ReactComponent {})
mkAddressBookApp =
  -- incoming \props are unused
  reactComponent "AddressBookApp" \props -> R.do
    -- `useState` takes a default initial value and returns the
    -- current value and a way to update the value.
    -- Consult react-hooks docs for a more detailed explanation of `useState`.
    Tuple person setPerson <- useState examplePerson
    let
      errors = case validatePerson' person of
        Left e -> e
        Right _ -> []

      -- helper-function to return array unchanged instead of Nothing if index is out of bounds
      updateAt' :: forall a. Int -> a -> Array a -> Array a
      updateAt' i x xs = fromMaybe xs (updateAt i x xs)

      -- helper-function to render a single phone number at a given index
      renderPhoneNumber :: Int -> PhoneNumber -> Array R.JSX
      renderPhoneNumber index phone =
        formField
          (PhoneField phone."type")
          "XXX-XXX-XXXX"
          phone.number
          errors
          (\s -> setPerson _ { phones = updateAt' index phone { number = s } person.phones })

      -- helper-function to render all phone numbers
      renderPhoneNumbers :: Array R.JSX
      renderPhoneNumbers =
        let
          phoneErrors = renderValidationErrors $ extractErrors (PhoneField NoPhone) errors
        in
          phoneErrors <> (concat $ mapWithIndex renderPhoneNumber person.phones)
    pure
      $ D.div
          { className: "container"
          , children:
              [ D.div
                  { className: "row"
                  , children:
                      [ D.form
                          { className: "col-md-12"
                          , children:
                              [ D.h3_ [ D.text "Basic Information" ] ]
                                <> ( formField FirstNameField "First Name" person.firstName errors \s ->
                                      setPerson _ { firstName = s }
                                  )
                                <> ( formField LastNameField "Last Name" person.lastName errors \s ->
                                      setPerson _ { lastName = s }
                                  )
                                <> [ D.h3_ [ D.text "Address" ] ]
                                <> ( formField StreetField "Street" person.homeAddress.street errors \s ->
                                      setPerson _ { homeAddress { street = s } }
                                  )
                                <> ( formField CityField "City" person.homeAddress.city errors \s ->
                                      setPerson _ { homeAddress { city = s } }
                                  )
                                <> ( formField StateField "State" person.homeAddress.state errors \s ->
                                      setPerson _ { homeAddress { state = s } }
                                  )
                                <> [ D.h3_ [ D.text "Contact Information" ] ]
                                <> renderPhoneNumbers
                          }
                      ]
                  }
              ]
          }

main :: Effect Unit
main = do
  log "Rendering address book component"
  -- Get window object
  w <- window
  -- Get window's HTML document
  doc <- document w
  -- Get "container" element in HTML
  ctr <- getElementById "container" $ toNonElementParentNode doc
  case ctr of
    Nothing -> throw "Container element not found."
    Just c -> do
      -- Create AddressBook react component
      addressBookApp <- mkAddressBookApp
      let
        -- Create JSX node from react component. Pass-in empty props
        app = element addressBookApp {}
      -- Render AddressBook JSX node in DOM "container" element
      D.render app c
