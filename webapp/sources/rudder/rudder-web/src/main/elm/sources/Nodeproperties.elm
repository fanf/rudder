port module Nodeproperties exposing (..)

import Browser
import Http exposing (..)
import Result
import Json.Encode exposing (encode, string, object)
import Json.Decode exposing (decodeValue, value)

import NodeProperties.ApiCalls exposing (..)
import NodeProperties.DataTypes exposing (..)
import NodeProperties.Init exposing (init)
import NodeProperties.JsonDecoder exposing (..)
import NodeProperties.View exposing (view)

import Debug

-- PORTS / SUBSCRIPTIONS
port errorNotification   : String -> Cmd msg
port successNotification : String -> Cmd msg
port initTooltips        : String -> Cmd msg

subscriptions : Model -> Sub Msg
subscriptions _ =
  Sub.none

main =
  Browser.element
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }

--
-- update loop --
--
update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    Ignore ->
      ( model , Cmd.none )

    CallApi apiCall ->
      ( model , apiCall model)

    SaveChanges ->
      ( model , successNotification "" )

    UpdateNewProperty newProperty ->
      ({model | newProperty = newProperty}, Cmd.none)

    SaveProperty res ->
      case  res of
        Ok p ->
          let
            ui  = model.ui
            newModel = { model
              | newProperty = (EditProperty "" "" model.newProperty.format True True)
              , ui = { ui | loading = False }
              }
          in
            ( newModel
            , Cmd.batch [ initTooltips "" , successNotification "", getNodeProperties newModel]
            )
        Err err ->
          processApiError "saving node properties" err model

    GetNodeProperties res ->
      case  res of
        Ok properties ->
          let
            modelUi  = model.ui
          in
            ( { model | properties = properties, ui = { modelUi | loading = False } }
              , initTooltips ""
            )
        Err err ->
          processApiError "Getting node properties" err model


    AddProperty ->
      let
        cmd = case model.newProperty.format of
          StringFormat -> saveProperty model.newProperty model
          JsonFormat   ->
            let
              --checkJsonFormat = (Json.Decode.dict (Json.Decode.lazy (\_ -> decodePropertyValue)) |> Json.Decode.map JsonObject) encodedValue
              checkJsonFormat = decodeValue decodePropertyValueTest (string model.newProperty.value) -- NOT WORKING
            in
              case checkJsonFormat of
                Ok s  -> saveProperty model.newProperty model
                Err _ -> errorNotification "JSON check is enabled, but the value format is invalid."
      in
        (model, cmd)

    DeleteProperty key ->
          (model, Cmd.none)

    ToggleEditPopup modalState ->
      let
        ui = model.ui
      in
        ( { model | ui = {ui | modalState = modalState} }, Cmd.none )


processApiError : String -> Error -> Model -> ( Model, Cmd Msg )
processApiError apiName err model =
  let
    message =
      case err of
        Http.BadUrl url ->
            "The URL " ++ url ++ " was invalid"
        Http.Timeout ->
            "Unable to reach the server, try again"
        Http.NetworkError ->
            "Unable to reach the server, check your network connection"
        Http.BadStatus 500 ->
            "The server had a problem, try again later"
        Http.BadStatus 400 ->
            "Verify your information and try again"
        Http.BadStatus _ ->
            "Unknown error"
        Http.BadBody errorMessage ->
            errorMessage
  in
    (model, errorNotification ("Error when " ++ apiName ++ ", details: \n" ++ message ) )