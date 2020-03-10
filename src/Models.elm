module Models exposing
    ( Area(..)
    , Lang(..)
    , Model
    , Msg(..)
    , Work
    , areaFromString
    , langFromString
    , formHeaders
    , initialWork
    , listOfWorksDecoder
    , latestWorksDecoder
    )

import Http exposing (..)
import Json.Decode as D exposing (field, int, string)


type Msg
    = Load Int
    | GotLatestWorks (Result Http.Error (List Work))
    | GotCreatedWork (Result Http.Error Work)
    | Save Work
    | Reset
    | Delete
    | SetTitle String
    | SetDescription String
    | SetArea String
    | SetLanguage String


type alias Model =
    { currentWork : Work
    , formHeader : String
    , latestWorks : List Work
    , message : String
    }


type Area
    = Technical
    | Training
    | Management

type Lang
    = Italian
    | English


formHeaders :
    { createWork : String
    , editWork : String
    }
formHeaders =
    { createWork = "Nuovo lavoro"
    , editWork = "Modifica lavoro"
    }


type alias Work =
    { id : Int
    , language : Lang
    , area : Area
    , order : Int
    , title : String
    , description : String
    }


initialWork : Work
initialWork =
    { id = -1
    , language = Italian
    , area = Technical
    , title = ""
    , description = ""
    , order = 0
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


languageDecoder : D.Decoder Lang
languageDecoder =
    D.string
        |> D.andThen
            (\str ->
                case str of
                    "en" ->
                        D.succeed English

                    "it" ->
                        D.succeed Italian

                    other ->
                        D.fail <| "Unknown area: " ++ other
            )

areaFromString : String -> Maybe Area
areaFromString str =
    case str of
        "technical" ->
            Just Technical

        "training" ->
            Just Training

        "management" ->
            Just Management

        _ ->
            Nothing

langFromString : String -> Maybe Lang
langFromString str =
    case str of
        "it" ->
            Just Italian

        "en" ->
            Just English

        _ ->
            Nothing


areaDecoder : D.Decoder Area
areaDecoder =
    D.string
        |> D.andThen
            (\str ->
                case str of
                    "technical" ->
                        D.succeed Technical

                    "training" ->
                        D.succeed Training

                    "management" ->
                        D.succeed Management

                    other ->
                        D.fail <| "Unknown area: " ++ other
            )


listOfWorksDecoder : D.Decoder (List Work)
listOfWorksDecoder =
    D.list workDecoder


latestWorksDecoder : D.Decoder Work
latestWorksDecoder =
    workDecoder
