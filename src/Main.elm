module Main exposing (main)

import Html exposing (Html, button, div, text, hr)
import Html.Events exposing (onClick)
import Browser


type alias Model = 
  { 
    counter: Int, 
    which: Int 
  }

main: Program () Model Msg
main =
  Browser.sandbox { init = init, update = update, view = view }

init: Model
init = { counter = 0, which = 0}

type Msg = 
  Increment 
  | Decrement
  | Reset

update: Msg -> Model -> Model
update msg model =
  case msg of
    Increment ->
      { model | counter = model.counter + 1 }
    Decrement ->
      { model | counter = model.counter - 1 }
    Reset ->
      { model | counter = 0 }

view: Model -> Html Msg
view model =
  div []
    [ button [ onClick Decrement ] [ text "-" ]
    , div [] [ text (String.fromInt model.counter) ]
    , button [ onClick Increment ] [ text "+" ]
    , hr [] [] 
    , button [ onClick Reset ] [ text " Reset" ]
    ]

{- 
module Main exposing (main)

import Browser
import Html exposing (Html, div, span, text)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)


type alias User = String

init: User
init = "Arnold"


main : Program () String Msg
main =
  Browser.sandbox { init = init, update = update, view = view }

greet : String -> String
greet name = "Hello " ++ name ++ "!"

type Msg = Highlight

update : Msg -> User -> String
update msg model = 
  case msg of 
    -- String -> greet model
    Highlight -> "*" ++ model ++ "*"

view : User -> Html Msg
view user = 
  div [] [span [
    onClick Highlight,
    style "color" "#222"
    ] [text (greet user)]]

-}