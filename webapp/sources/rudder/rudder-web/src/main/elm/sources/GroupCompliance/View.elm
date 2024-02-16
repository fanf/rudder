module GroupCompliance.View exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import List
import Html.Lazy

import GroupCompliance.DataTypes exposing (..)
import GroupCompliance.ViewUtils exposing (..)
import GroupCompliance.ViewRulesCompliance exposing (..)
import GroupCompliance.ViewNodesCompliance exposing (..)


view : Model -> Html Msg
view model =
  div [class "tab-table-content"]
  ( List.append
    [ ul [class "nav nav-underline"]
      [ li [class "nav-item"]
        [ button
          [ attribute "role" "tab", type_ "button", class ("nav-link " ++ (if model.ui.viewMode == RulesView then " active" else "")), onClick (ChangeViewMode RulesView)]
          [ text "By Rules" ]
        ]
      , li [class "nav-item"]
        [ button
          [ attribute "role" "tab", type_ "button", class ("nav-link " ++ (if model.ui.viewMode == NodesView then " active" else "")), onClick (ChangeViewMode NodesView)]
          [ text "By Rules" ]
        ]
      ]
    ]
    [( case model.ui.viewMode of
      RulesView -> Html.Lazy.lazy displayRulesComplianceTable model
      NodesView -> Html.Lazy.lazy displayNodesComplianceTable model
    )]
  )