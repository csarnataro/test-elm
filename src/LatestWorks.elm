module LatestWorks exposing (Msg, update, view, init)

import Models exposing(..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Browser
import Http exposing (..)
import Json.Decode as D exposing (Decoder, field, string, int)


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , subscriptions = subscriptions
        , view = view
        , update = update
    }


initialWork: Work
initialWork =   {
    id = -1, 
    language = Italian, 
    area = Technical,
    title = "",
    description = "",
    order = 0
  }


latestWorks: List Work
latestWorks = [
  {
    id = 1, 
    language = Italian, 
    area = Technical,
    title = "Test title",
    description = "Long description",
    order = 1
  },
  {
    id = 2, 
    language = Italian, 
    area = Technical,
    title = "Test title 2",
    description = "Long description 2",
    order = 2
  }

  ]


subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none


init : () -> (Model, Cmd Msg)
init _ =
    ({ 
      currentWork = initialWork,
      latestWorks = []
    },
    getUsers
    )


getUsers: Cmd Msg
getUsers =
    Http.get
      { 
        url = "https://reqres.in/api/users",
        expect = Http.expectJson GotText listOfWorksDecoder
      }
    

-- userDecoder : Decoder (List Work)
-- userDecoder =
--   field "data" (list (field "email" string))


workDecoder : D.Decoder Work
workDecoder =
    D.map6
        Work
        (D.field "id" D.int)
        (D.succeed Italian)
        (D.succeed Technical)
        (D.field "id" D.int)
        (D.field "email" D.string)
        (D.field "first_name" D.string)

    -- id : Int,
    -- language: Lang,
    -- area: Area,
    -- order: Int,
    -- title: String,
    -- description: String


listOfWorksDecoder : D.Decoder (List Work)
listOfWorksDecoder =
    field "data" (D.list workDecoder)

type Msg
    = Load Int
    | GotText (Result Http.Error (List Work))
    | Create
    | Delete


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Load id ->
          case finder model.latestWorks id of
            Nothing -> (model, Cmd.none)
            Just w -> ({model | currentWork = w}, Cmd.none)

        Create ->
          (model, Cmd.none)

        Delete -> 
          (model, Cmd.none)
        GotText result -> 
          case result of
            Ok value -> (
              {
                model 
              | latestWorks = value
              }, Cmd.none)
            Err err -> ( {
                model 
              | latestWorks = [
                {
                  id = 1, 
                  language = Italian, 
                  area = Technical,
                  title = case err of 
                    BadBody str ->
                      str
                    _ -> "Error"
                  ,
                  description = "Long description",
                  order = 1
                }]
              } , Cmd.none)

view : Model -> Html Msg
view model =
      div []
        [ 
          workForm model.currentWork,
          table [ class "table table-striped" ]
            (List.map 
            (\work -> tr []
              [
                td[][text (String.fromInt work.id)],
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
    label[for "title"][text "Title"],
    input[value work.title, name "title", id "title"][],
    input[value work.description][],
    input[type_ "hidden", value (String.fromInt work.id)][]
  ]

-- toTableRow: Work -> Html Msg
-- toTableRow myListItem =
--   tr []
--      [
--      td[][text (String.fromInt myListItem.id)],
--      td[][text myListItem.title]
--      ]


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

