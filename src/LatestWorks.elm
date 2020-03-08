module LatestWorks exposing (Msg, update, view, init)

import Models exposing(Lang(..), Area(..), Work, Model, listOfWorksDecoder, initialWork)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Browser
import Http exposing (..)

main : Program () Model Msg
main =
    Browser.element
        { init = init
        , subscriptions = subscriptions
        , view = view
        , update = update
    }


subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none


init : () -> (Model, Cmd Msg)
init _ =
    (
      { currentWork = initialWork
      , latestWorks = []
      }
      , getLatestWorks
    )


getLatestWorks: Cmd Msg
getLatestWorks =
    Http.get
      { 
        url = "http://www.graniteng.com/admin/latestWorks/api.php?login=***&password=***",
        expect = Http.expectJson GotText listOfWorksDecoder
      }
    
processLoad : Model -> Int -> (Model, Cmd Msg)
processLoad model id =

    case finder model.latestWorks id of
      Nothing -> (model, Cmd.none)
      Just w -> 
        (
          { model | currentWork = w }
          , Cmd.none
        )


processJson : Model -> Result Error (List Work) -> (Model, Cmd Msg)
processJson model result = 
  case result of
    Ok value -> 
      (
        { model | latestWorks = value }
        , Cmd.none
      )
    Err err ->
      ( 
        {
          model 
          | latestWorks = [
            { id = 1
            , language = Italian
            , area = Technical
            , title = "Error"
            , description = 
              case err of 
                BadBody str -> str
                _ -> "Unknown error"
            , order = 1
        }]
      } , Cmd.none)




update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Load id -> processLoad model id
        Create -> (model, Cmd.none)
        Delete ->  (model, Cmd.none)
        GotText result -> processJson model result

view : Model -> Html Msg
view model =
      div [class "container"]
        [ 
          workForm model.currentWork,
          table [ class "table table-striped" ]
            (List.map 
            (\work -> tr []
              [
                td[][text (String.fromInt work.id)],
                td[][text (
                  case work.area of
                    Technical -> "area tecnica"
                    Training -> "formazione"
                    Management -> "management"
                )],
                td[][text (
                  case work.language of 
                    English -> "English"
                    Italian -> "Italiano"
                )],
                td[][text work.title],
                td[][text work.description],
                td[][
                  button[onClick (Load work.id)][ text "Edit"]
                ]
              ]
            )
            model.latestWorks
            )
        ]

workForm: Work -> Html Msg
workForm work = 
  Html.form[][
    span [] [text (String.fromInt work.id)],
    hr [] [],
    label[for "title"][text "Titolo"],
    input[value work.title, name "title", id "title"][],
    label[for "language"][text "Lingua"],
    select[name "language", id "language"] (List.map toLangOption [
      ((Italian, "Italiano"), work.language), 
      ((English, "Inglese"), work.language)
    ]),
    label[for "area"][text "Area"],
    select[name "area", id "area"] (List.map toAreaOption [
      ((Technical, "Area tecnica"), work.area), 
      ((Training, "Formazione"), work.area),
      ((Management, "Management"), work.area)
    ]),
    label[for "description"][text "Descrizione"],
    input[id "description", value work.description][],
    input[type_ "hidden", value (String.fromInt work.id)][]
  ]

toLangOption: ((Lang, String), Lang) -> Html msg
toLangOption ((value_, label), currentValue)  = 
  option [value 
  (
    case value_ of 
      Italian -> "it"
      English -> "en"
  ) , selected 
    (  value_ == currentValue
    )] [text label]

toAreaOption: ((Area, String), Area) -> Html msg
toAreaOption ((value_, label), currentValue)  = 
  option [value 
  (
    case value_ of 
      Training -> "training"
      Management -> "management"
      Technical -> "technical"
  ) , selected 
    (  value_ == currentValue
    )] [text label]


type Msg
    = Load Int
    | GotText (Result Http.Error (List Work))
    | Create
    | Delete

finder : List Work -> Int -> Maybe Work
finder lst id = 
  case lst of
    [] -> Nothing
    x :: xs ->
      if x.id == id 
      then 
        Just x
      else 
        finder xs id

