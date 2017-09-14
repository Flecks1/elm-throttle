module Main exposing (main)

import Html exposing (Html, br, div, input, label, text)
import Html.Attributes exposing (value)
import Html.Events exposing (onInput)
import Platform exposing (Program)
import Throttle
import Time


type Msg
    = SetTextAndLogWithLeading String
    | SetTextAndLogWithTrailing String
    | SetTextAndLogWithBoth String
    | LogText String


update : Msg -> String -> ( String, Cmd Msg )
update msg model =
    case msg of
        SetTextAndLogWithLeading txt ->
            txt ! [ Throttle.leading Time.second "logTextLeading" (LogText txt) ]

        SetTextAndLogWithTrailing txt ->
            txt ! [ Throttle.trailing Time.second "logTextTrailing" (LogText txt) ]

        SetTextAndLogWithBoth txt ->
            txt ! [ Throttle.both Time.second "logTextBoth" (LogText txt) ]

        LogText txt ->
            let
                _ =
                    Debug.log "Logged text " txt
            in
            model ! []


view : String -> Html Msg
view txt =
    div []
        [ label []
            [ text "With leading:"
            , br [] []
            , input [ value txt, onInput SetTextAndLogWithLeading ] []
            ]
        , label []
            [ text "With trailing:"
            , br [] []
            , input [ value txt, onInput SetTextAndLogWithTrailing ] []
            ]
        , label []
            [ text "With both:"
            , br [] []
            , input [ value txt, onInput SetTextAndLogWithBoth ] []
            ]
        ]


main : Program Never String Msg
main =
    Html.program
        { init = ( "", Cmd.none )
        , update = update
        , subscriptions = always Sub.none
        , view = view
        }
