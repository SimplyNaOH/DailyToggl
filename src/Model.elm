module Model exposing (..)

import Time exposing (Time)
import Date exposing (Date)

import Dict exposing (Dict)

-- Model

type alias Entry =
    { project : String
    --, hexColor : String
    , start : Time
    , end : Time
    , dur : Time
    }


type alias Model =
    { days : List (Date, List Entry)
    , colors : Dict String String
    , tooltip : String
    , date : Maybe Date
    , token : String
    , workspace : String
    , errors : List String
    }


initialModel =
    Model [] Dict.empty "" Nothing "" "" []
