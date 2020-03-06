module Models exposing (Model, Work, Area(..), Lang(..), listOfWorksDecoder, initialWork)

import Json.Decode as D exposing (field, string, int)


type alias Model = 
  {
    currentWork: Work,
    latestWorks: List Work
  }
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

initialWork: Work
initialWork =   {
    id = -1, 
    language = Italian, 
    area = Technical,
    title = "",
    description = "",
    order = 0
  }



workDecoder : D.Decoder Work
workDecoder =
    D.map6
        Work
        (D.field "id" D.int)
        (D.field "language" languageDecoder)
        (D.field "area" areaDecoder)
        (D.field "row_order" D.int)
        (D.field "title" D.string)
        (D.field "description" D.string)

languageDecoder: D.Decoder Lang
languageDecoder = 
    D.string
        |> D.andThen (\str ->
           case str of
                "en" ->
                    D.succeed English
                _ ->
                    D.succeed Italian
        )

areaDecoder: D.Decoder Area
areaDecoder = 
    D.string
        |> D.andThen (\str ->
           case str of
                "technical" ->
                    D.succeed Technical
                "training" ->
                    D.succeed Training
                "management" ->
                    D.succeed Management
                other -> D.fail <| "Unknown area: " ++ other
        )



listOfWorksDecoder : D.Decoder (List Work)
listOfWorksDecoder =
    D.list workDecoder
