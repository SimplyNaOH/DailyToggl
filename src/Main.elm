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

import Time

import Model exposing (..)
import Update exposing (..)
import TimelineView exposing (..)
import CmdDispatcher


-- View

view : Model Msg -> Html.Html Msg
view model =
    div []
        [ p [] [ text <| "Today is " ++ Maybe.withDefault "Loading Date" (Maybe.map toString model.date) ]
        , input [placeholder "API Token", value model.token, onInput SetToken] []
        , input [placeholder "Workspace ID", value model.workspace, onInput SetWorkspace] []
        , p [] [a [ href "#0", onClick LoadData ] [ text "load data" ]]
        , p [] [a [ href "#0", onClick ExampleData ] [ text "example data" ]]
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
        , subscriptions = \model -> Sub.map CmdDispatcherMsg (CmdDispatcher.subscriptions Time.second model.cmdDispatcher)
        , view = view
        , update = update
        }
