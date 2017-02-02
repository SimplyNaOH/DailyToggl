module TimelineView exposing (..)

import Model exposing (Entry)
import Update exposing (Msg (..))

import Html exposing (div, text)
import Html.Attributes exposing (style)
import Html.Events exposing (onMouseEnter, onMouseLeave)

import Dict

import Date
import Time

entryView model entry =
  let
        percentage =
            (Time.inHours entry.dur) / 24 * 100
        left = ((\x -> toFloat (Date.hour x) + toFloat (Date.minute x) / 60) <| Date.fromTime entry.start) / 24 * 100

        divStyle =
            style
                [ ( "position", "absolute" )
                , ( "height", "90%" )
                , ( "width", toString percentage ++ "%" )
                , ("top", "10%")
                , ( "left", toString left ++ "%")
                , ( "background-color", Maybe.withDefault "#aaa" (Dict.get entry.project model.colors) )
                ]
  in
        div [ divStyle, onMouseEnter <| SetTooltip <| entry.project ++ " " ++ (String.left 5 <| toString percentage) ++ "%", onMouseLeave <| SetTooltip "" ] []--[ text <| (String.left 5 <| toString percentage) ++ "%" ]

hourMarks =
  let
        percentage = 1 / 24 * 100

        divStyle i =
            style
                [ ( "float", "left" )
                , ( "height", "9%" )
                , ( "width", toString percentage ++ "%" )
                , ( "background-color", if i % 2 == 0 then "#aaa" else "#888" )
                , ( "border", "solid #fff" )
                , ( "border-width", "0 1px")
                , ( "box-sizing", "border-box")
                ]
  in
    List.map
      (\i -> div [ divStyle i, onMouseEnter <| SetTooltip <| toString i ++ "hs", onMouseLeave <| SetTooltip ""] [])
      (List.range 0 23)

timelineView model =
    let
        divStyle =
            style
                [ ("position", "relative")
                , ( "width", "90%" )
                , ( "height", "70px" )
                , ( "margin", "8px auto" )
                , ( "border", "1px solid #bbb" )
                , ("overflow", "hidden")
                ]
        dateText date = div [ style [
            ("position", "absolute")
          , ("width", "100%")
          , ("height", "100%")
          , ("z-index", "3")
          , ("text-align", "center")
          , ("padding-top", "0.6%")
          , ("font-size", "3em")
          , ("color", "white")
          , ("text-shadow", "black 0 0 17px, black 0 0 7px")
          , ("pointer-events", "none")
          ] ] [text <| toString <| Date.day date]
    in
      div [] <|
        List.map (\(date, entries) -> div [ divStyle ] <| dateText date :: hourMarks ++ List.map (entryView model) entries) model.days



agreggatedView entry =
    let
        percentage =
            (Time.inHours entry.time) / 24 * 100

        divStyle =
            style
                [ ( "float", "left" )
                , ( "height", "100%" )
                , ( "width", toString percentage ++ "%" )
                , ( "background-color", entry.hexColor )
                ]
    in
        div [ divStyle, onMouseEnter <| SetTooltip entry.project, onMouseLeave <| SetTooltip "" ] [ text <| (String.left 5 <| toString percentage) ++ "%" ]
