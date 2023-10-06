module DirectiveCompliance.Init exposing (..)

import Dict exposing (Dict)

import DirectiveCompliance.ApiCalls exposing (..)
import DirectiveCompliance.DataTypes exposing (..)
import Compliance.DataTypes exposing (..)


init : { directiveId : String, contextPath : String } -> ( Model, Cmd Msg )
init flags =
  let
    initFilters  = (TableFilters Asc "" Dict.empty (ComplianceFilters False False []))
    initUI       = UI initFilters initFilters RulesView True False
    initModel    = Model (DirectiveId flags.directiveId) flags.contextPath "" initUI Nothing
    listInitActions =
      [ getPolicyMode initModel
      ]
  in
    ( initModel
    , Cmd.batch listInitActions
    )