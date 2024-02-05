port module Accounts.Init exposing (..)

import Accounts.ApiCalls exposing (..)
import Accounts.DataTypes exposing (..)
import Accounts.DatePickerUtils exposing (..)
import Accounts.JsonDecoder exposing (decodeAcl)
import Http exposing (Error)
import Json.Decode exposing (..)
import Json.Decode.Pipeline as D exposing (..)
import SingleDatePicker exposing (Settings, TimePickerVisibility(..), defaultSettings)
import Task
import Time exposing (Month(..), Posix, Zone)
import Time.Extra as Time exposing (Interval(..))



-- PORTS / SUBSCRIPTIONS


port successNotification : String -> Cmd msg


port errorNotification : String -> Cmd msg


port initTooltips : String -> Cmd msg



-- clipboard copy


port copy : String -> Cmd msg



--for the Api Authorization plugin


port initAcl :
    -- init the elm app
    String -> Cmd msg


port shareAcl :
    -- when we change account, give plugin id / acl
    Value -> Cmd msg


port getCheckedAcl :
    -- get the list of ACL selected in the plugin extension
    (Json.Decode.Value -> msg) -> Sub msg



{--for the multi tenants plugin.
    The basic behavior is:
    - when the plugin is disabled, only show a read-only value, either  "all" or "none"
    - when the plugin is enabled, display the select menu in edit, and the three cases
--}


port initTenants :
    -- init the elm app
    String -> Cmd msg


port focusAccountTenants :
    -- when we change account, give plugin id / access type
    Value -> Cmd msg


port getAccessModeTenants :
    -- get the access type for tenants set in the plugin
    (Json.Decode.Value -> msg) -> Sub msg


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ SingleDatePicker.subscriptions (userDefinedDatePickerSettings model.ui.datePickerInfo.zone model.ui.datePickerInfo.currentTime model.ui.datePickerInfo.currentTime) model.ui.datePickerInfo.picker
        , Time.every 1000 Tick -- Update of the current time every second
        , getCheckedAcl (GetCheckedAcl << decodeValue (Json.Decode.list decodeAcl))
        ]


init : { contextPath : String, hasWriteRights : Bool } -> ( Model, Cmd Msg )
init flags =
    let
        initDatePicker =
            DatePickerInfo (Time.millisToPosix 0) Time.utc Nothing (SingleDatePicker.init UpdatePicker)

        initFilters =
            TableFilters Name Asc "" ""

        initUi =
            UI initFilters NoModal NoCopy False True initDatePicker False

        initModel =
            Model flags.contextPath initUi [] False Nothing

        initActions =
            [ Task.perform Tick Time.now
            , Task.perform AdjustTimeZone Time.here
            ]
    in
    ( initModel, Cmd.batch initActions )
