module CmdDispatcher exposing (..)

import Time exposing (Time)

type alias State msg = List (Cmd msg)

empty : State msg
empty = []

type Msg = Dispatch

update : Int -> Msg -> State msg -> (State msg, Cmd msg)
update batchSize msg state = case msg of
  Dispatch ->
    ( List.drop batchSize state
    , Cmd.batch <| List.take batchSize state )

subscriptions : Time -> State msg -> Sub Msg
subscriptions delay state =
    if List.isEmpty state
      then
        Sub.none
      else
        Time.every delay (always Dispatch)
