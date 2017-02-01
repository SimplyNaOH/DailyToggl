module Main exposing (..)

import Html exposing (Html, button, input, div, body, text, img, a, ul, li, p, span, select, option, h1, i, h2)
import Html.Attributes exposing (style, class, classList, src, height, width, href, target, attribute, id, placeholder, value)
import Html.Events exposing (onClick, onInput, onMouseEnter, onMouseLeave)
import Json.Decode as Json
import Date exposing (Date)
import Task
import Time exposing (Time)
import Http
import BasicAuth

import Model exposing (..)
import Update exposing (..)
import TimelineView exposing (..)


-- View

view : Model -> Html.Html Msg
view model =
    div []
        [ h1 [] [text "Beware"]
        , h2 [] [text "This is a proof of concept. If you don't throttle the network some requests will fail."]
        , p [] [ text <| "Today is " ++ Maybe.withDefault "Loading Date" (Maybe.map toString model.date) ]
        , input [placeholder "API Token", value model.token, onInput SetToken] []
        , input [placeholder "Workspace ID", value model.workspace, onInput SetWorkspace] []
        , a [ href "#0", onClick LoadData ] [ text "load data" ]
--        , ul [] <|
--            List.map (\data -> li [] [ text data.project ]) model.data
        , timelineView model
        , p [ style [ ( "text-align", "center" ) ] ] [ text model.tooltip ]
        , ul [] <|
            li [] [ text "Errors:" ]
                :: List.map (\err -> li [] [ text err ]) model.errors
        ]

main =
    Html.program
        { init = ( initialModel, Task.perform SetDate Date.now )
        , subscriptions = always Sub.none
        , view = view
        , update = update
        }
