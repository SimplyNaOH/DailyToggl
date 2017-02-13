module Update exposing (..)

import Model exposing (..)
import CmdDispatcher

import Http
import BasicAuth

import Json.Decode as Json

import Dict

import Date exposing (Date)
import Time
import Date.Extra.Period as Period

-- Update


type Msg
    = RequestResponse Date (Result Http.Error (List Entry))
    | ColorsResponse (Result Http.Error (List (String, String)))
    | LoadData
    | ExampleData
    | SetTooltip String
    | SetDate Date
    | SetToken String
    | SetWorkspace String
    | CmdDispatcherMsg CmdDispatcher.Msg
    | NoOp


update : Msg -> Model Msg -> ( Model Msg, Cmd Msg )
update msg model =
    case msg of
        RequestResponse date (Ok entries) ->
--            ( { model | days = List.sortBy (Maybe.withDefault 0 << Maybe.map Date.day << Maybe.map Date.fromTime << Maybe.map .start << List.head)
--                <| model.days ++ [entries] }, Cmd.none )
            ( { model | days = List.sortBy (\(date,_) -> Date.toTime date) <| model.days ++ [(date, entries)] }, Cmd.none)

        RequestResponse _ (Err htmlError) ->
            ( { model | errors = model.errors ++ [ toString htmlError ] }, Cmd.none )

        ColorsResponse (Ok colorList) ->
            ( { model | colors = Dict.union model.colors (Dict.fromList colorList) }, Cmd.none )

        ColorsResponse (Err htmlError) ->
            ( { model | errors = model.errors ++ [ toString htmlError ] }, Cmd.none )

        LoadData ->
            case model.date of
                Nothing ->
                    ( { model | errors = model.errors ++ [ "Tried to load data without the current date." ] }, Cmd.none )

                Just date ->
                  let
                    entriesCmds = (List.map (\date -> Http.send (RequestResponse date) <| getDetailedReport model.token model.workspace date) <|
                      List.scanl (\x acc -> prevDay acc) date (List.range 1 7))
                    colorsCmds = (List.map (\date -> Http.send ColorsResponse <| getSummary model.token model.workspace date) <|
                      List.scanl (\x acc -> prevDay acc) date (List.range 1 7))
                  in
                    ( {model | cmdDispatcher = model.cmdDispatcher ++ entriesCmds ++ colorsCmds }, Cmd.none)

        ExampleData ->
          let
            tupleDecoder = Json.map2 (,) (Json.field "fst" Json.string) (Json.field "snd" Json.string)
            colorsDecoder = Json.map (Dict.fromList) (Json.list tupleDecoder)
            entryDecoder = Json.map4 Entry
              (Json.field "project" Json.string)
              (Json.field "start" Json.float)
              (Json.field "end" Json.float)
              (Json.field "dur" Json.float)
            daysDecoder = Json.list <| Json.map2 (,)
              (Json.field "date" (Json.map Date.fromTime Json.float))
              (Json.field "entries" (Json.list entryDecoder))
            colorsResult = Json.decodeString (Json.field "colors" colorsDecoder) exampleData
            daysResult = Json.decodeString (Json.field "days" daysDecoder) exampleData
          in
            ( { model | days = Result.withDefault [] daysResult, colors = Result.withDefault Dict.empty colorsResult }, Cmd.none )

        SetTooltip str ->
            ( { model | tooltip = str }, Cmd.none )

        SetDate date ->
            ( { model | date = Just date }, Cmd.none )

        SetToken token ->
            ( { model | token = token }, Cmd.none )

        SetWorkspace workspace ->
            ( { model | workspace = workspace }, Cmd.none )

        CmdDispatcherMsg subMsg ->
          let
            (updatedDispatcher, cmds) = CmdDispatcher.update 4 subMsg model.cmdDispatcher
          in
            ( { model | cmdDispatcher = updatedDispatcher }
            , cmds)

        NoOp ->
            ( model, Cmd.none )

-- Commands


decodeEntry =
    Json.map4 Entry
        (Json.map (Maybe.withDefault "Other") <|
            Json.field "project" (Json.nullable Json.string)
        )
        (Json.map (Result.withDefault 0 << Result.map Date.toTime << Date.fromString) <| Json.field "start" Json.string)
        (Json.map (Result.withDefault 0 << Result.map Date.toTime << Date.fromString) <|Json.field "end" Json.string)
        (Json.field "dur" Json.float)


decodeResponse =
    Json.field "data" (Json.list decodeEntry)

decodeColors =
  Json.field "data" <| Json.list <|
    Json.map2 (\a b -> (a,b))
      (Json.map (Maybe.withDefault "Other") <|
          Json.at ["title", "project"] (Json.nullable Json.string)
      )
      (Json.map (Maybe.withDefault "#aaa") <|
          Json.at ["title", "hex_color"] (Json.nullable Json.string)
      )

header token =
    BasicAuth.buildAuthorizationHeader token "api_token"


getDetailedReport token workspace date =
    let
        dateString =
            dateToYYYYMMDD date
    in
        Http.request
            { method = "GET"
            , headers = [ header token ]
            , url = "https://toggl.com/reports/api/v2/details?user_agent=test&page=1&workspace_id=" ++ workspace ++ "&since=" ++ dateString ++ "&until=" ++ dateString
            , body = Http.emptyBody
            , expect = Http.expectJson decodeResponse
            , timeout = Nothing
            , withCredentials = False
            }

getSummary token workspace date =
    let
        dateString =
            dateToYYYYMMDD date
    in
        Http.request
            { method = "GET"
            , headers = [ header token ]
            , url = "https://toggl.com/reports/api/v2/summary?user_agent=test&workspace_id=" ++ workspace ++ "&since=" ++ dateString ++ "&until=" ++ dateString
            , body = Http.emptyBody
            , expect = Http.expectJson decodeColors
            , timeout = Nothing
            , withCredentials = False
            }


-- Helper
prevDay : Date -> Date
prevDay date =
  Period.add Period.Day (-1) date
  {-
  Result.withDefault date <| Date.fromString <|
        (toString (Date.year date))
            ++ "-"
            ++ monthToMM (Date.month date)
            ++ "-"
            ++ (toString (Date.day date - 1))
            -}
  --(Result.withDefault date << Result.andThen Date.fromString << Result.map (IsoDate.toString) << Result.map (IsoDate.fromTime << round) << Result.map ((-) (24 * Time.hour)) << Result.map (toFloat << IsoDate.toTime) << IsoDate.fromString << dateToYYYYMMDD) date

monthToMM month =
    case month of
        Date.Jan ->
            "01"

        Date.Feb ->
            "02"

        Date.Mar ->
            "03"

        Date.Apr ->
            "04"

        Date.May ->
            "05"

        Date.Jun ->
            "06"

        Date.Jul ->
            "07"

        Date.Aug ->
            "08"

        Date.Sep ->
            "09"

        Date.Oct ->
            "10"

        Date.Nov ->
            "11"

        Date.Dec ->
            "12"


dateToYYYYMMDD date =
    (toString (Date.year date))
        ++ "-"
        ++ monthToMM (Date.month date)
        ++ "-"
        ++ (toString (Date.day date))
