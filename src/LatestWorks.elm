module LatestWorks exposing (Model, Msg, update, view, init)


import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Browser


main : Program () Model Msg
main =
    Browser.sandbox
        { init = init
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

type Area 
  = Technical
  | Training
  | Management

type Lang 
  = Italian
  | English

type alias Work = 
  { 
    id : Int,
    language: Lang,
    area: Area,
    order: Int,
    title: String,
    description: String
  }


type alias Model = 
  {
    currentWork: Work,
    latestWorks: List Work
  }



init : Model
init =
    { 
      currentWork = initialWork,
      latestWorks = latestWorks
    }


type Msg
    = Load Int
    | Create
    | Delete


update : Msg -> Model -> Model
update msg model =
    case msg of
        Load id ->
          case finder model.latestWorks id of
             Nothing -> model
             Just w -> {model | currentWork = w}

        Create ->
            model

        Delete -> 
          model


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
                  button[onClick (Load work.id)][ text "Load"]
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
    []      -> Nothing
    x :: xs ->
      if x.id == id then Just x
      else finder xs id

