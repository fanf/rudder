module Accounts.ViewModals exposing (..)

import Accounts.ApiCalls exposing (..)
import Accounts.DataTypes exposing (..)
import Accounts.DatePickerUtils exposing (..)
import Accounts.JsonDecoder exposing (parseTenants)
import Html exposing (..)
import Html.Attributes exposing (attribute, checked, class, disabled, for, id, name, placeholder, readonly, selected, size, style, title, type_, value)
import Html.Events exposing (custom, onCheck, onClick, onInput)
import SingleDatePicker exposing (Settings, TimePickerVisibility(..), defaultSettings, defaultTimePickerSettings)
import Task
import Time exposing (Month(..), Posix, Zone)
import Time.Extra as Time exposing (Interval(..), add)


displayModals : Model -> Html Msg
displayModals model =
    let
        ( checkEmptyBtn, checkEmptyWarning, checkAlreadyUsedName ) =
            case model.editAccount of
                Nothing ->
                    ( False, False, False )

                Just account ->
                    case model.ui.modalState of
                        NewAccount ->
                            ( String.isEmpty account.name, False, List.member account.name (List.map .name model.accounts) )

                        EditAccount a ->
                            ( String.isEmpty account.name, String.isEmpty account.name, a.name /= account.name && List.member account.name (List.map .name model.accounts) )

                        _ ->
                            ( False, False, False )

        modalClass =
            if model.ui.modalState == NoModal then
                ""

            else
                " show"

        ( modalTitle, btnTxt, btnClass ) =
            case model.ui.modalState of
                NoModal ->
                    ( "", "Save", "default" )

                NewAccount ->
                    ( "Create a new API account", "Create", "success" )

                EditAccount a ->
                    ( "Update account '" ++ a.name ++ "'", "Update", "success" )

                Confirm Delete a call ->
                    ( "Delete API account '" ++ a ++ "'", "Confirm", "danger" )

                Confirm Regenerate a call ->
                    ( "Regenerate token of API account '" ++ a ++ "'", "Confirm", "primary" )

        modalUI =
            case model.ui.modalState of
                NoModal ->
                    ModalUI False False Ignore (text "")

                Confirm modalType a call ->
                    let
                        subTitle =
                            case modalType of
                                Delete ->
                                    "delete"

                                Regenerate ->
                                    "regenerate token of"
                    in
                    ModalUI False
                        False
                        call
                        (div []
                            [ h4 [ class "text-center" ] [ text ("You are about to " ++ subTitle ++ " an API account.") ]
                            , div [ class "alert alert-warning" ]
                                [ i [ class "fa fa-exclamation-triangle" ] []
                                , text "If you continue, any scripts using this will no longer be able to connect to Rudder's API."
                                ]
                            ]
                        )

                _ ->
                    case model.editAccount of
                        Nothing ->
                            ModalUI False False Ignore (text "")

                        Just account ->
                            let
                                datePickerValue =
                                    getDateString model.ui.datePickerInfo model.ui.datePickerInfo.pickedTime

                                ( expirationDate, selectedDate ) =
                                    case account.expirationDate of
                                        Just d ->
                                            ( if account.expirationDateDefined then
                                                posixToString model.ui.datePickerInfo d

                                              else
                                                "Never"
                                            , d
                                            )

                                        Nothing ->
                                            ( "Never", add Month 1 model.ui.datePickerInfo.zone model.ui.datePickerInfo.currentTime )

                                aclList =
                                    case account.acl of
                                        Just l ->
                                            l

                                        Nothing ->
                                            []

                                displayWarningName =
                                    if checkEmptyWarning then
                                        span [ class "warning-info" ] [ i [ class "fa fa-warning" ] [], text " This field is required" ]

                                    else if checkAlreadyUsedName then
                                        span [ class "warning-info" ] [ i [ class "fa fa-warning" ] [], text " This name is already used" ]

                                    else
                                        text ""

                                -- if the plugin is disable, only show a read-only view of tenants. Else, it's an option among all, none, a list
                                displayTenantAccess =
                                    select [ id "newAccount-tenants", class "form-select", onInput (\s -> UpdateAccountForm { account | tenantMode = Tuple.first (parseTenants s) }) ]
                                        [ option [ value "*", selected (account.tenantMode == AllAccess), disabled (not model.tenantsPluginEnabled) ] [ text "Access to all tenants" ]
                                        , option [ value "-", selected (account.tenantMode == NoAccess), disabled (not model.tenantsPluginEnabled) ] [ text "Access to no tenant" ]
                                        , option [ value "list", selected (account.tenantMode == ByTenants), disabled (not model.tenantsPluginEnabled) ]
                                            [ text
                                                ("Access to restricted list of tenants: "
                                                    ++ (case account.selectedTenants of
                                                            Just tenants ->
                                                                String.join ", " tenants

                                                            Nothing ->
                                                                "-"
                                                       )
                                                )
                                            ]
                                        ]
                            in
                            ModalUI (account.authorisationType == "acl")
                                (account.tenantMode == ByTenants)
                                (CallApi (saveAccount account))
                                (form
                                    [ name "newAccount"
                                    , class
                                        ("newAccount"
                                            ++ (if SingleDatePicker.isOpen model.ui.datePickerInfo.picker then
                                                    " datepicker-open"

                                                else
                                                    ""
                                               )
                                        )
                                    ]
                                    [ div
                                        [ class
                                            ("form-group"
                                                ++ (if checkEmptyWarning || checkAlreadyUsedName then
                                                        " has-warning"

                                                    else
                                                        ""
                                                   )
                                            )
                                        ]
                                        [ label [ for "newAccount-name" ] [ text "Name", displayWarningName ]
                                        , input [ id "newAccount-name", type_ "text", class "form-control", value account.name, onInput (\s -> UpdateAccountForm { account | name = s }) ] []
                                        ]
                                    , div [ class "form-group" ]
                                        [ label [ for "newAccount-description" ] [ text "Description" ]
                                        , textarea [ id "newAccount-description", class "form-control vresize float-inherit", value account.description, onInput (\s -> UpdateAccountForm { account | description = s }) ] []
                                        ]
                                    , div [ class "form-group" ]
                                        [ label [ for "newAccount-expiration", class "mb-1" ]
                                            [ text "Expiration date"
                                            , label [ for "selectDate", class "custom-toggle toggle-secondary" ]
                                                [ input [ type_ "checkbox", id "selectDate", checked account.expirationDateDefined, onCheck (\c -> UpdateAccountForm { account | expirationDateDefined = c }) ] []
                                                , label [ for "selectDate", class "custom-toggle-group" ]
                                                    [ label [ for "selectDate", class "toggle-enabled" ] [ text "Defined" ]
                                                    , span [ class "cursor" ] []
                                                    , label [ for "selectDate", class "toggle-disabled" ] [ text "Undefined" ]
                                                    ]
                                                ]
                                            , if checkIfExpired model.ui.datePickerInfo account then
                                                span [ class "warning-info" ] [ i [ class "fa fa-warning" ] [], text " Expiration date has passed" ]

                                              else
                                                text ""
                                            ]
                                        , div [ class "elm-datepicker-container" ]
                                            [ button [ type_ "button", class "form-control btn-datepicker", disabled (not account.expirationDateDefined), onClick (OpenPicker selectedDate), placeholder "Select an expiration date" ]
                                                [ text expirationDate
                                                ]
                                            , SingleDatePicker.view (userDefinedDatePickerSettings model.ui.datePickerInfo.zone model.ui.datePickerInfo.currentTime selectedDate) model.ui.datePickerInfo.picker
                                            ]
                                        ]
                                    , div [ class "form-group" ]
                                        [ label [ for "newAccount-tenants" ] [ text "Access to tenants" ]
                                        , displayTenantAccess
                                        ]
                                    , div [ class "form-group" ]
                                        [ label [ for "newAccount-access" ] [ text "Access level" ]
                                        , select [ id "newAccount-access", class "form-select", onInput (\s -> UpdateAccountForm { account | authorisationType = s }) ]
                                            [ option [ value "ro", selected (account.authorisationType == "ro") ] [ text "Read only" ]
                                            , option [ value "rw", selected (account.authorisationType == "rw") ] [ text "Full access" ]
                                            , option [ value "acl", selected (account.authorisationType == "acl"), disabled (not model.aclPluginEnabled) ]
                                                [ text
                                                    ("Custom ACL"
                                                        ++ (if model.aclPluginEnabled then
                                                                ""

                                                            else
                                                                " (Plugin needed)"
                                                           )
                                                    )
                                                ]
                                            ]
                                        ]
                                    ]
                                )
    in
    case model.ui.copyState of
        NoCopy ->
            div [ class ("modal modal-account fade " ++ modalClass) ]
                [ div [ class "modal-backdrop fade show", onClick (ToggleEditPopup NoModal) ] []
                , div [ class "modal-dialog" ]
                    [ div [ class "modal-content" ]
                        [ div [ class "modal-header" ]
                            [ h5 [ class "modal-title" ] [ text modalTitle ]
                            , button [ type_ "button", class "btn-close", attribute "data-bs-dismiss" "modal", attribute "aria-label" "Close" ] []
                            ]
                        , div [ class "modal-body" ]
                            [ modalUI.body

                            -- Tenants plugin container
                            , div
                                [ id "tenantapiaccounts-app"
                                , style "display"
                                    (if modalUI.displayTenants then
                                        "block"

                                     else
                                        "none"
                                    )
                                ]
                                [ h4 [] [ text "Select tenants for account:" ]
                                , div [ id "tenantapiaccounts-content" ] []
                                ]

                            -- ACL plugin container
                            , div
                                [ id "apiauthorization-app"
                                , style "display"
                                    (if modalUI.displayAcl then
                                        "block"

                                     else
                                        "none"
                                    )
                                ]
                                [ h4 [] [ text "Select ACL for account:" ]
                                , div [ id "apiauthorization-content" ] []
                                ]
                            ]
                        , div [ class "modal-footer" ]
                            [ button [ type_ "button", class "btn btn-default", onClick (ToggleEditPopup NoModal) ] [ text "Close" ]
                            , button [ type_ "button", class ("btn btn-" ++ btnClass), onClick modalUI.saveAction, disabled (checkEmptyBtn || checkAlreadyUsedName) ] [ text btnTxt ]
                            ]
                        ]
                    ]
                ]

        Token txt ->
            -- Almost a modal as it is a notification that requires user interaction (copy)
            div [ class "modal fade show", style "display" "block" ]
                [ div [ class "modal-backdrop fade show", onClick CloseCopyPopup ] []
                , div [ class "modal-dialog" ]
                    [ div [ class "modal-content" ]
                        [ div [ class "modal-header" ]
                            [ h5 [ class "modal-title" ] [ text "Copy the token" ]
                            , button [ type_ "button", class "btn-close", attribute "data-bs-dismiss" "modal", attribute "aria-label" "Close", onClick CloseCopyPopup ] []
                            ]
                        , div [ class "modal-body" ]
                            [ div []
                                [ div [ class "alert alert-info" ]
                                    [ i [ class "fa fa-exclamation-triangle" ] []
                                    , text "This is the only time the token value will be available."
                                    ]
                                , div []
                                    [ span [ class "token-txt" ]
                                        [ text txt ]
                                    , a [ class "btn-goto always clipboard", title "Copy to clipboard", onClick (Copy txt) ]
                                        [ i [ class "ion ion-clipboard" ] [] ]
                                    ]
                                ]
                            ]
                        , div [ class "modal-footer" ]
                            [ button [ type_ "button", class "btn btn-success", onClick CloseCopyPopup ] [ text "Close" ]
                            ]
                        ]
                    ]
                ]
