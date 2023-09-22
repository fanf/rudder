module Rules.ComplianceUtils exposing (..)

import Dict exposing (Dict)
import Dict.Extra
import Maybe.Extra exposing (isJust)
import Html exposing (Html, button, div, i, span, text, h1, h4, ul, li, input, a, p, form, label, textarea, select, option, table, thead, tbody, tr, th, td, small)
import Html.Attributes exposing (id, class, type_, placeholder, value, for, href, colspan, rowspan, style, selected, disabled, attribute)
import Html.Events exposing (onClick, onInput)
import List.Extra
import List
import String exposing (fromFloat)
import Tuple exposing (first, second)

import Rules.ApiCalls exposing (..)
import Rules.DataTypes exposing (..)


getCompliance : Float -> String -> Html msg
getCompliance val t =
  if val > 0 then
    div[class ("progress-bar progress-bar-" ++ t), style "flex" (fromFloat val)][text ((fromFloat val) ++ "%")]
  else
    text ""

getValueCompliance : Maybe Float -> Float
getValueCompliance f =
  case f of
    Just v  -> v
    Nothing -> 0

getRuleCompliance : Model -> RuleId -> Maybe RuleComplianceGlobal
getRuleCompliance model rId =
  Dict.get rId.value model.rulesCompliance

getAllComplianceValues : ComplianceDetails ->
  { okStatus        : {value : Float, rounded : Int, details : String}
  , nonCompliant    : {value : Float, rounded : Int, details : String}
  , error           : {value : Float, rounded : Int, details : String}
  , unexpected      : {value : Float, rounded : Int, details : String}
  , pending         : {value : Float, rounded : Int, details : String}
  , reportsDisabled : {value : Float, rounded : Int, details : String}
  , noReport        : {value : Float, rounded : Int, details : String}
  }
getAllComplianceValues complianceDetails =
  let
    barContent : List (Float, String) -> String
    barContent lst =
      let
        content = lst
          |> List.map (\x ->
            if first x > 0 then
              "<li>" ++ second x ++ ": " ++ String.fromFloat (first x) ++ "%</li>"
            else
              ""
          )
      in
        String.join "" (List.append ["</ul>"] ("<ul>" :: content))

    valSuccessNotApplicable       = getValueCompliance complianceDetails.successNotApplicable       -- 0
    valSuccessAlreadyOK           = getValueCompliance complianceDetails.successAlreadyOK           -- 0
    valSuccessRepaired            = getValueCompliance complianceDetails.successRepaired            -- 0
    valAuditCompliant             = getValueCompliance complianceDetails.auditCompliant             -- 0
    valAuditNotApplicable         = getValueCompliance complianceDetails.auditNotApplicable         -- 0

    valAuditNonCompliant          = getValueCompliance complianceDetails.auditNonCompliant          -- 1

    valError                      = getValueCompliance complianceDetails.error                      -- 2
    valAuditError                 = getValueCompliance complianceDetails.auditError                 -- 2

    valUnexpectedUnknownComponent = getValueCompliance complianceDetails.unexpectedUnknownComponent -- 3
    valUnexpectedMissingComponent = getValueCompliance complianceDetails.unexpectedMissingComponent -- 3
    valBadPolicyMode              = getValueCompliance complianceDetails.badPolicyMode              -- 3

    valApplying                   = getValueCompliance complianceDetails.applying                   -- 4

    valReportsDisabled            = getValueCompliance complianceDetails.reportsDisabled            -- 5

    valNoReport                   = getValueCompliance complianceDetails.noReport                   -- 6

    okStatus        = valSuccessNotApplicable + valSuccessAlreadyOK + valSuccessRepaired + valAuditCompliant + valAuditNotApplicable
    nonCompliant    = valAuditNonCompliant
    error           = valError + valAuditError
    unexpected      = valUnexpectedUnknownComponent + valUnexpectedMissingComponent + valBadPolicyMode
    pending         = valApplying
    reportsDisabled = valReportsDisabled
    noReport        = valNoReport

    okStatusText =
      if okStatus > 0 then
        let
          checks  =
            [ ( valSuccessAlreadyOK     , "Success (enforce)"        )
            , ( valAuditCompliant       , "Compliant"                )
            , ( valSuccessRepaired      , "Repaired"                 )
            , ( valSuccessNotApplicable , "Not applicable (enforce)" )
            , ( valAuditNotApplicable   , "Not applicable (audit)"   )
            ]
          content = barContent checks
        in
          content
      else
        ""

    nonCompliantText =
      if nonCompliant > 0 then
        barContent [( nonCompliant , "Non compliant" )]
      else
        ""

    errorText =
      if error > 0 then
        let
          checks  =
            [ ( valError      , "Errors (enforce)" )
            , ( valAuditError , "Errors (audit)"   )
            ]
          content = barContent checks
        in
            content
      else
        ""

    unexpectedText =
      if unexpected > 0 then
        let
          checks  =
            [ ( valUnexpectedUnknownComponent , "Unknown reports" )
            , ( valUnexpectedMissingComponent , "Missing reports" )
            , ( valBadPolicyMode              , "Not supported mixed mode on directive from same Technique" )
            ]
          content = barContent checks
        in
            content
      else
        ""

    pendingText =
      if pending > 0 then
        barContent [( valApplying , "Applying" )]
      else
        ""

    reportsDisabledText =
      if reportsDisabled > 0 then
        barContent [( valReportsDisabled , "Reports Disabled" )]
      else
        ""

    noReportText =
      if noReport > 0 then
        barContent [( valNoReport , "No report" )]
      else
        ""

    -- This prevents the sum of the rounded compliances from being greater or less than 100%, using the largest remainder method.
    -- 1. Rounding everything down
    compliances = Dict.fromList
      [ ("okStatus"        , { floor = Basics.floor okStatus        , decimal = okStatus        - toFloat (Basics.floor okStatus        )})
      , ("nonCompliant"    , { floor = Basics.floor nonCompliant    , decimal = nonCompliant    - toFloat (Basics.floor nonCompliant    )})
      , ("error"           , { floor = Basics.floor error           , decimal = error           - toFloat (Basics.floor error           )})
      , ("unexpected"      , { floor = Basics.floor unexpected      , decimal = unexpected      - toFloat (Basics.floor unexpected      )})
      , ("pending"         , { floor = Basics.floor pending         , decimal = pending         - toFloat (Basics.floor pending         )})
      , ("reportsDisabled" , { floor = Basics.floor reportsDisabled , decimal = reportsDisabled - toFloat (Basics.floor reportsDisabled )})
      , ("noReport"        , { floor = Basics.floor noReport        , decimal = noReport        - toFloat (Basics.floor noReport        )})
      ]
    -- 2. Getting the difference in sum and 100
    getDiff = 100 - ( compliances
      |> Dict.map (\k a -> a.floor)
      |> Dict.values
      |> List.sum
      )
    -- 3. Distributing the difference by adding 1 to items in decreasing order of their decimal parts
    flippedComparison a b =
        case compare (Tuple.second a).decimal (Tuple.second b).decimal of
          LT -> GT
          EQ -> EQ
          GT -> LT
    getNumbersToUpdate = compliances
      |> Dict.toList
      |> List.sortWith flippedComparison
      |> List.take getDiff
      |> List.map (\x -> (Tuple.first x))
    roundedCompliance = compliances
      |> Dict.map (\k a -> if (List.member k getNumbersToUpdate) then {a | floor = a.floor+1} else a )
    getRoundedValue : String -> Float -> Int
    getRoundedValue key fallback =
      let
        roundedValue =
          case ( roundedCompliance
            |> Dict.get key ) of
              Just v  -> v.floor
              Nothing -> Basics.round fallback
      in
        roundedValue

    allComplianceValues =
      { okStatus        = { value = okStatus        , rounded = (getRoundedValue "okStatus"        okStatus        ), details = okStatusText        }
      , nonCompliant    = { value = nonCompliant    , rounded = (getRoundedValue "nonCompliant"    nonCompliant    ), details = nonCompliantText    }
      , error           = { value = error           , rounded = (getRoundedValue "error"           error           ), details = errorText           }
      , unexpected      = { value = unexpected      , rounded = (getRoundedValue "unexpected"      unexpected      ), details = unexpectedText      }
      , pending         = { value = pending         , rounded = (getRoundedValue "pending"         pending         ), details = pendingText         }
      , reportsDisabled = { value = reportsDisabled , rounded = (getRoundedValue "reportsDisabled" reportsDisabled ), details = reportsDisabledText }
      , noReport        = { value = noReport        , rounded = (getRoundedValue "noReport"        noReport        ), details = noReportText        }
      }
  in
    allComplianceValues


sumPercent : ComplianceDetails -> Float
sumPercent compliance =
  let
    getValue v = Maybe.withDefault 0 v
  in
   [ compliance.successAlreadyOK, compliance.applying, compliance.auditCompliant, compliance.auditError,
     compliance.auditNonCompliant, compliance.auditNotApplicable, compliance.badPolicyMode, compliance.error,
     compliance.noReport, compliance.reportsDisabled, compliance.successNotApplicable, compliance.successRepaired,
     compliance.unexpectedMissingComponent, compliance.unexpectedUnknownComponent
   ] |> List.map getValue |> List.sum

getDirectiveComputedCompliance : (DirectiveCompliance value) -> Float
getDirectiveComputedCompliance dc =
  let
    allComplianceValues = getAllComplianceValues dc.complianceDetails
  in
    if ( allComplianceValues.okStatus.value + allComplianceValues.nonCompliant.value + allComplianceValues.error.value + allComplianceValues.unexpected.value + allComplianceValues.pending.value + allComplianceValues.reportsDisabled.value + allComplianceValues.noReport.value == 0 ) then
      -1.0
    else
      dc.compliance

getNodeComputedCompliance : NodeCompliance -> Float
getNodeComputedCompliance nc =
  let
    allComplianceValues = getAllComplianceValues nc.complianceDetails
  in
    if ( allComplianceValues.okStatus.value + allComplianceValues.nonCompliant.value + allComplianceValues.error.value + allComplianceValues.unexpected.value + allComplianceValues.pending.value + allComplianceValues.reportsDisabled.value + allComplianceValues.noReport.value == 0 ) then
      -1.0
    else
      nc.compliance

mergeCompliance : ComplianceDetails -> ComplianceDetails -> ComplianceDetails
mergeCompliance c1 c2 =
  let
    sumMaybeFloat : Maybe Float -> Maybe Float -> Maybe Float
    sumMaybeFloat  mf1 mf2 =
      case (mf1,mf2) of
        ( Nothing , Nothing ) -> Nothing
        ( Just f1 , Just f2 ) -> Just (f1+f2)
        ( a       , Nothing ) -> a
        ( Nothing , b       ) -> b
    toMaybeFloat = \ fun -> sumMaybeFloat (fun c1) (fun c2)
  in
    ComplianceDetails
      (toMaybeFloat .successNotApplicable)
      (toMaybeFloat .successAlreadyOK)
      (toMaybeFloat .successRepaired)
      (toMaybeFloat .error)
      (toMaybeFloat .auditCompliant)
      (toMaybeFloat .auditNonCompliant)
      (toMaybeFloat .auditError)
      (toMaybeFloat .auditNotApplicable)
      (toMaybeFloat .unexpectedUnknownComponent)
      (toMaybeFloat .unexpectedMissingComponent)
      (toMaybeFloat .noReport)
      (toMaybeFloat .reportsDisabled)
      (toMaybeFloat .applying)
      (toMaybeFloat .badPolicyMode)

complianceStatusGroups : Dict String (List String)
complianceStatusGroups = Dict.fromList
  [ ("Success"        , [ "successAlreadyOK", "auditCompliant", "successRepaired", "successNotApplicable", "auditNotApplicable" ] )
  , ("Non compliant"  , [ "auditNonCompliant" ] )
  , ("Error"          , [ "error", "auditError" ] )
  , ("Unexpected"     , [ "unexpectedMissingComponent", "unexpectedUnknownComponent", "badPolicyMode" ] )
  , ("Pending"        , [ "applying" ] )
  , ("Disabled"       , [ "reportsDisabled" ] )
  , ("No report"      , [ "noReport" ] )
  ]

getComplianceStatusTitle : String -> String
getComplianceStatusTitle id =
 case id of
   "successAlreadyOK"           -> "Success (enforce)"
   "auditCompliant"             -> "Compliant"
   "successRepaired"            -> "Repaired"
   "successNotApplicable"       -> "Not applicable (enforce)"
   "auditNotApplicable"         -> "Not applicable (audit)"
   "auditNonCompliant"          -> "Non compliant"
   "error"                      -> "Error (enforce)"
   "auditError"                 -> "Error (audit)"
   "unexpectedMissingComponent" -> "Missing reports"
   "unexpectedUnknownComponent" -> "Unknown reports"
   "badPolicyMode"              -> "Not supported mixed mode"
   "applying"                   -> "Applying"
   "reportsDisabled"            -> "Reports disabled"
   "noReport"                   -> "No report"
   _ -> ""


filterCompliance : ComplianceDetails -> ComplianceFilters -> ComplianceDetails
filterCompliance complianceDetails complianceFilters =
  let
    dict s = case s of
               "successAlreadyOK"  ->  \v -> { v | successAlreadyOK = Nothing }
               "auditCompliant"  ->  \v -> { v | auditCompliant = Nothing }
               "successRepaired"  ->  \v -> { v | successRepaired = Nothing }
               "successNotApplicable"  ->  \v -> { v | successNotApplicable = Nothing }
               "auditNotApplicable"  ->  \v -> { v | auditNotApplicable = Nothing }
               "auditNonCompliant"  ->  \v -> { v | auditNonCompliant = Nothing }
               "error"  ->  \v -> { v | error = Nothing }
               "auditError"  ->  \v -> { v | auditError = Nothing }
               "unexpectedMissingComponent"  ->  \v -> { v | unexpectedMissingComponent = Nothing }
               "unexpectedUnknownComponent"  ->  \v -> { v | unexpectedUnknownComponent = Nothing }
               "badPolicyMode"  ->  \v -> { v | badPolicyMode = Nothing }
               "applying"  ->  \v -> { v | applying = Nothing }
               "reportsDisabled"  ->  \v -> { v | reportsDisabled = Nothing }
               "noReport"  ->  \v -> { v | noReport = Nothing }
               _ -> \v -> v

    fun key current = (dict key) current

    allStatuses =
                 [ "successAlreadyOK", "auditCompliant", "successRepaired", "successNotApplicable", "auditNotApplicable"
                 , "auditNonCompliant"
                   , "error", "auditError", "unexpectedMissingComponent", "unexpectedUnknownComponent", "badPolicyMode"
                 , "applying" , "reportsDisabled" , "noReport"
                   ]
    statuses = if complianceFilters.showOnlyStatus then
                 List.filter (\s -> not (List.member s complianceFilters.selectedStatus )) allStatuses
               else
                 complianceFilters.selectedStatus
  in
    List.foldl fun complianceDetails statuses

checkFilterCompliance : ComplianceDetails -> ComplianceFilters -> Bool
checkFilterCompliance complianceDetails complianceFilters =
  let
    dict = Dict.fromList
      [ ("successAlreadyOK"           , complianceDetails.successAlreadyOK           )
      , ("auditCompliant"             , complianceDetails.auditCompliant             )
      , ("successRepaired"            , complianceDetails.successRepaired            )
      , ("successNotApplicable"       , complianceDetails.successNotApplicable       )
      , ("auditNotApplicable"         , complianceDetails.auditNotApplicable         )
      , ("auditNonCompliant"          , complianceDetails.auditNonCompliant          )
      , ("error"                      , complianceDetails.error                      )
      , ("auditError"                 , complianceDetails.auditError                 )
      , ("unexpectedMissingComponent" , complianceDetails.unexpectedMissingComponent )
      , ("unexpectedUnknownComponent" , complianceDetails.unexpectedUnknownComponent )
      , ("badPolicyMode"              , complianceDetails.badPolicyMode              )
      , ("applying"                   , complianceDetails.applying                   )
      , ("reportsDisabled"            , complianceDetails.reportsDisabled            )
      , ( "noReport"                  , complianceDetails.noReport                   )
      ]
    filteredDict = dict
      |> Dict.filter (\k v -> isJust v)
  in
    filteredDict
    |> Dict.Extra.any (\k val ->
      let
        isSelected = List.member k complianceFilters.selectedStatus
      in
        if complianceFilters.showOnlyStatus then
          isSelected
        else
          not isSelected
    )