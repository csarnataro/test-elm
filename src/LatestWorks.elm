module LatestWorks exposing (init, update, view)

import Browser
import Debug
import Forms.Forms
    exposing
        ( encodeForm
        , latestWorksTable
        )
import Html exposing (..)
import Html.Attributes exposing (..)
import Http exposing (..)
import Models
    exposing
        ( Area(..)
        , Lang(..)
        , Model
        , Msg(..)
        , Work
        , areaFromString
        , formHeaders
        , initialWork
        , langFromString
        , latestWorksDecoder
        , listOfWorksDecoder
        )


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


init : () -> ( Model, Cmd Msg )
init _ =
    ( { currentWork = initialWork
      , formHeader = formHeaders.createWork
      , latestWorks = []
      , message = ""
      }
    , getLatestWorks
    )


getLatestWorks : Cmd Msg
getLatestWorks =
    Http.get
        { url = "http://localhost:1337/latest-works"
        , expect = Http.expectJson GotLatestWorks listOfWorksDecoder
        }


saveWork : Model -> ( Model, Cmd Msg )
saveWork model =
    let
        work =
            model.currentWork
    in
    ( model
    , Http.post
        { body =
            encodeForm work
                |> Http.jsonBody
        , url = "http://localhost:1337/latest-works"
        , expect = Http.expectJson GotCreatedWork latestWorksDecoder
        }
    )


processLoad : Model -> Int -> ( Model, Cmd Msg )
processLoad model id =
    case finder model.latestWorks id of
        Nothing ->
            ( model, Cmd.none )

        Just w ->
            ( { model
                | currentWork = w
                , formHeader = formHeaders.editWork
              }
            , Cmd.none
            )


processCreatedWorkJson : Model -> Result Error Work -> ( Model, Cmd Msg )
processCreatedWorkJson model result =
    case result of
        Ok value ->
            ( { model | message = "Lavoro " ++ value.title ++ " salvato" }
            , Cmd.none
            )

        Err err ->
            ( { model
                | message =
                    case err of
                        BadBody str ->
                            str

                        _ ->
                            "Unknown error"
              }
            , Cmd.none
            )


processLatestWorksJson : Model -> Result Error (List Work) -> ( Model, Cmd Msg )
processLatestWorksJson model result =
    case result of
        Ok value ->
            ( { model | latestWorks = value }
            , Cmd.none
            )

        Err err ->
            ( { model
                | latestWorks =
                    [ { id = 1
                      , language = Italian
                      , area = Technical
                      , title = "Error"
                      , description =
                            case err of
                                BadBody str ->
                                    str

                                _ ->
                                    "Unknown error"
                      , order = 1
                      }
                    ]
              }
            , Cmd.none
            )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Load id ->
            processLoad model id

        Save m ->
            saveWork model

        Delete ->
            ( model, Cmd.none )

        GotLatestWorks result ->
            processLatestWorksJson model result

        GotCreatedWork result ->
            processCreatedWorkJson model result

        SetTitle title ->
            ( updateTitle model title, Cmd.none )

        SetDescription description ->
            ( updateDescription model description, Cmd.none )

        SetLanguage language ->
            ( updateLanguage model language, Cmd.none )

        SetArea area ->
            ( updateArea model area, Cmd.none )

        Reset ->
            ( { model
                | currentWork = initialWork
                , formHeader = formHeaders.createWork
              }
            , Cmd.none
            )


updateTitle : Model -> String -> Model
updateTitle ({ currentWork } as model) title =
    { model | currentWork = { currentWork | title = title } }


updateDescription : Model -> String -> Model
updateDescription ({ currentWork } as model) description =
    { model | currentWork = { currentWork | description = description } }


updateArea : Model -> String -> Model
updateArea ({ currentWork } as model) area =
    case areaFromString area of
        Just a ->
            { model | currentWork = { currentWork | area = a } }

        Nothing ->
            model


updateLanguage : Model -> String -> Model
updateLanguage ({ currentWork } as model) language =
    case langFromString language of
        Just a ->
            { model | currentWork = { currentWork | language = a } }

        Nothing ->
            model


view : Model -> Html Msg
view model =
    let
        _ =
            Debug.log "model" model.currentWork
    in
    latestWorksTable model


finder : List Work -> Int -> Maybe Work
finder lst id =
    case lst of
        [] ->
            Nothing

        x :: xs ->
            if x.id == id then
                Just x

            else
                finder xs id
