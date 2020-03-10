module Forms.Forms exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (custom, onClick, onInput)
import Json.Decode as Decode
import Json.Encode as Encode
import Models exposing (..)


workForm : Work -> Html Msg
workForm work =
    Html.form []
        [ div
            [ class "row" ]
            [ div [ class "column" ]
                [ label [ for "language" ] [ text "Lingua" ]
                , select
                    [ name "language"
                    , id "language"
                    , onInput SetLanguage
                    ]
                    (List.map toLangOption
                        [ ( ( Italian, "Italiano" ), work.language )
                        , ( ( English, "Inglese" ), work.language )
                        ]
                    )
                ]
            , div
                [ class "column" ]
                [ label [ for "area" ] [ text "Area" ]
                , select
                    [ name "area"
                    , id "area"
                    , onInput SetArea
                    ]
                    (List.map toAreaOption
                        [ ( ( Technical, "Area tecnica" ), work.area )
                        , ( ( Training, "Formazione" ), work.area )
                        , ( ( Management, "Management" ), work.area )
                        ]
                    )
                ]
            ]
        , div [ class "row" ]
            [ div [ class "column" ]
                [ label [ for "title" ]
                    [ text "Titolo"
                    , input
                        [ value work.title
                        , name "title"
                        , id "title"
                        , type_ "text"
                        , onInput SetTitle
                        ]
                        []
                    ]
                , label [ for "description" ]
                    [ text "Descrizione"
                    , textarea
                        [ id "description"
                        , value work.description
                        , onInput SetDescription
                        ]
                        []
                    ]
                , input
                    [ type_ "hidden"
                    , name "id"
                    , value (String.fromInt work.id)
                    ]
                    []
                ]
            ]
        , submitButton work
        ]


toLangOption : ( ( Lang, String ), Lang ) -> Html msg
toLangOption ( ( value_, label ), currentValue ) =
    option
        [ value
            (case value_ of
                Italian ->
                    "it"

                English ->
                    "en"
            )
        , selected
            (value_ == currentValue)
        ]
        [ text label ]


toAreaOption : ( ( Area, String ), Area ) -> Html msg
toAreaOption ( ( value_, label ), currentValue ) =
    option
        [ value
            (case value_ of
                Training ->
                    "training"

                Management ->
                    "management"

                Technical ->
                    "technical"
            )
        , selected
            (value_ == currentValue)
        ]
        [ text label ]


latestWorksTable : Model -> Html Msg
latestWorksTable model =
    let
        displayMessage = case model.message of 
          "" -> "none" 
          _ -> "block"
    in
    
    div [ class "container" ]
        [ pageHeader
        , formHeader model
        , workForm model.currentWork
        , blockquote
            [ style "border-left" "0.3rem solid #c51443" 
            , style "display" displayMessage
            ]
            [ text model.message ]
        , table [ class "table table-striped" ]
            (List.map
                (\work ->
                    tr []
                        [ td [] [ text (String.fromInt work.id) ]
                        , td []
                            [ text
                                (case work.area of
                                    Technical ->
                                        "area tecnica"

                                    Training ->
                                        "formazione"

                                    Management ->
                                        "management"
                                )
                            ]
                        , td []
                            [ text
                                (case work.language of
                                    English ->
                                        "English"

                                    Italian ->
                                        "Italiano"
                                )
                            ]
                        , td [] [ text work.title ]
                        , td [] [ text work.description ]
                        , td []
                            [ button [ onClick (Load work.id) ] [ text "Edit" ]
                            ]
                        ]
                )
                model.latestWorks
            )
        ]


submitButton : Work -> Html Msg
submitButton work =
    div [ class "row" ]
        [ div [ class "column" ]
            [ div [ class "clearfix" ]
                [ div [ class "float-right" ]
                    [ text (work.id |> String.fromInt)
                    , if work.id /= -1 then
                        cancelButton

                      else
                        text ""
                    , text " "
                    , button [ onCustomClick (Save work) ] [ text "Salva" ]
                    ]
                ]
            ]
        ]


cancelButton : Html Msg
cancelButton =
    input
        [ type_ "reset"
        , onClick Reset
        , class "button button-outline"
        , value "Annulla"
        ]
        []


pageHeader : Html Msg
pageHeader =
    h1 [] [ text "Amministrazione Ultimi Lavori" ]


formHeader : Model -> Html Msg
formHeader model =
    h3 [] [ text model.formHeader ]


onCustomClick : msg -> Html.Attribute msg
onCustomClick msg =
    custom "click"
        (Decode.succeed
            { message = msg
            , stopPropagation = True
            , preventDefault = True
            }
        )


encodeForm : Work -> Encode.Value
encodeForm work =
    Encode.object
        [ ( "id", Encode.int work.id )
        , ( "title", Encode.string work.title )
        , ( "description", Encode.string work.description )
        , ( "language"
          , Encode.string
                (case work.language of
                    Italian ->
                        "it"

                    English ->
                        "en"
                )
          )
        , ( "area"
          , Encode.string
                (case work.area of
                    Technical ->
                        "technical"

                    Training ->
                        "training"

                    Management ->
                        "management"
                )
          )
        , ( "row_order", Encode.int work.order )
        ]
