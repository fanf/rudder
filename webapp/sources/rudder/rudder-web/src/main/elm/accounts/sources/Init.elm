port module Init exposing (..)

import ApiCalls exposing (..)
import DataTypes exposing (..)

import Http exposing (Error)
import SingleDatePicker exposing (Settings, TimePickerVisibility(..), defaultSettings)
import Task
import Time exposing (Month(..), Posix, Zone)
import Time.Extra as Time exposing (Interval(..))
import DatePickerUtils exposing (..)
import Json.Decode exposing (..)
import Json.Decode.Pipeline as D exposing (..)
import JsonDecoder exposing (decodeAcl)


-- PORTS / SUBSCRIPTIONS

port successNotification : String -> Cmd msg
port errorNotification   : String -> Cmd msg
port initTooltips        : String -> Cmd msg
port copy                : String -> Cmd msg
port initAcl             : String -> Cmd msg
port shareAcl            : Value  -> Cmd msg
port getCheckedAcl       : (Json.Decode.Value -> msg) -> Sub msg

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.batch
  [ SingleDatePicker.subscriptions (userDefinedDatePickerSettings model.ui.datePickerInfo.zone model.ui.datePickerInfo.currentTime model.ui.datePickerInfo.currentTime) UpdatePicker model.ui.datePickerInfo.picker
  , Time.every 1000 Tick -- Update of the current time every second
  , getCheckedAcl (GetCheckedAcl << decodeValue (Json.Decode.list decodeAcl))
  ]

init : { contextPath : String, hasWriteRights : Bool } -> ( Model, Cmd Msg )
init flags =
  let
    initDatePicker = DatePickerInfo (Time.millisToPosix 0) Time.utc Nothing SingleDatePicker.init
    initFilters    = TableFilters Name Asc "" ""
    initUi         = UI initFilters NoModal False True initDatePicker False
    initModel      = Model flags.contextPath initUi [] False Nothing
    initActions    =
      [ ZIO.perform Tick Time.now
      , ZIO.perform AdjustTimeZone Time.here
      ]
  in
    ( initModel , Cmd.batch initActions )
