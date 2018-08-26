module Workflows.SimpleTodos exposing (Model, Msg, initialModel, update, view)

import Browser.Dom as Dom
import Html exposing (Html, button, div, form, hr, i, input, text)
import Html.Attributes exposing (action, class, classList, disabled, id, placeholder, value)
import Html.Events exposing (custom, onClick, onInput, onSubmit)
import Json.Decode
import ProgrissStore as Store exposing (Action, ActionId, ActionState(..), ProgrissStore, decoder)
import Task


type Msg
    = UpdateNewActionDescription String
    | UpdateActionDescription Action String
    | CreateNewAction
    | ToggleActionDone ActionId
    | ToggleEditAction (Maybe ActionId)
    | SetFocusTo String
    | FocusResult (Result Dom.Error ())


type alias Model =
    { newActionDescription : String, editing : Maybe ActionId }


initialModel : Model
initialModel =
    { newActionDescription = "", editing = Nothing }


update : Msg -> ProgrissStore -> Model -> ( Model, ProgrissStore, Cmd Msg )
update msg store model =
    case msg of
        UpdateNewActionDescription newActionDescription ->
            ( { model | newActionDescription = newActionDescription }, store, Cmd.none )

        UpdateActionDescription action description ->
            let
                updatedStore =
                    Store.updateAction { action | description = description } store
            in
            ( model, updatedStore, Cmd.none )

        CreateNewAction ->
            let
                ( actionId, updatedStore ) =
                    Store.createAction model.newActionDescription store
            in
            ( { model | newActionDescription = "" }, updatedStore, Cmd.none )

        ToggleActionDone actionId ->
            ( { model | editing = Nothing }, Store.toggleActionDone actionId store, Cmd.none )

        ToggleEditAction maybeActionId ->
            case maybeActionId of
                Nothing ->
                    ( { model | editing = maybeActionId }, store, Cmd.none )

                Just actionId ->
                    let
                        domId =
                            "edit-" ++ Store.actionIdToString actionId
                    in
                    ( { model | editing = maybeActionId }, store, Task.attempt FocusResult (Dom.focus domId) )

        SetFocusTo domId ->
            ( model, store, Task.attempt FocusResult (Dom.focus domId) )

        FocusResult result ->
            case result of
                Err (Dom.NotFound id) ->
                    ( model, store, Cmd.none )

                Ok () ->
                    ( model, store, Cmd.none )


view : ProgrissStore -> Model -> Html Msg
view store model =
    div [ class "container" ]
        [ div [ class "row" ]
            [ div [ class "col" ]
                [ renderActions model.editing (Store.getAllActions store)
                , renderNewActionFormCard model
                ]
            ]
        ]


renderNewActionFormCard : Model -> Html Msg
renderNewActionFormCard model =
    div [ class "card bg-light" ]
        [ div [ class "card-body" ]
            [ div [ class "row align-middle" ]
                [ div [ class "col-2 col-md-1" ]
                    [ button
                        [ class "btn btn-success bmd-btn-fab bmd-btn-fab-sm"
                        , onClick (SetFocusTo "new-action-description")
                        ]
                        [ i [ class "material-icons" ] [ text "add" ] ]
                    ]
                , div [ class "col" ] [ newActionForm model ]
                ]
            ]
        ]


newActionForm : Model -> Html Msg
newActionForm model =
    form
        [ onSubmit CreateNewAction ]
        [ div [ class "input-group" ]
            [ input
                [ placeholder "Add an Action"
                , class "form-control"
                , value model.newActionDescription
                , onInput UpdateNewActionDescription
                , id "new-action-description"
                ]
                []
            , div [ class "input-group-append", disabled (String.isEmpty model.newActionDescription) ]
                [ button [ class "btn btn-success" ] [ text "Create Action" ] ]
            ]
        ]


renderActions : Maybe ActionId -> List Action -> Html Msg
renderActions editing actions =
    div [] (List.map (\action -> actionCard editing action) actions)


actionCard : Maybe ActionId -> Action -> Html Msg
actionCard editing action =
    div [ class (cardClassForAction action) ]
        [ div [ class "card-body", onClick (ToggleEditAction (Just action.id)) ]
            [ div [ class "row align-items-center" ]
                [ div [ class "col-2 col-md-1" ] [ toggleDoneButton action ]
                , if editing == Just action.id then
                    div [ class "col" ] [ editActionForm action ]

                  else
                    div [ class "col" ] [ text action.description ]
                ]
            ]
        ]


buttonColorForActionState : ActionState -> String
buttonColorForActionState state =
    case state of
        Done time ->
            "btn btn-success"

        _ ->
            "btn btn-primary"


iconForActionState : ActionState -> String
iconForActionState state =
    case state of
        Done time ->
            "done"

        _ ->
            "check_box_outline_blank"


cardClassForAction : Action -> String
cardClassForAction action =
    case action.state of
        Done time ->
            "card bg-light"

        _ ->
            "card"


toggleDoneButton action =
    button
        [ classList
            [ ( buttonColorForActionState action.state, True )
            , ( "bmd-btn-fab bmd-btn-fab-sm", True )
            ]
        , custom "click"
            (Json.Decode.succeed
                { message = ToggleActionDone action.id
                , preventDefault = True
                , stopPropagation = True
                }
            )
        ]
        [ i [ class "material-icons" ] [ text (iconForActionState action.state) ] ]


editActionForm action =
    form
        [ class "edit-action-form"
        , onSubmit (ToggleEditAction Nothing)
        ]
        [ div [ class "input-group" ]
            [ input
                [ class "form-control form-control-sm"
                , onInput (UpdateActionDescription action)
                , value action.description
                , id ("edit-" ++ Store.actionIdToString action.id)
                ]
                []
            , div [ class "input-group-append" ]
                [ button [ class "btn btn-success" ] [ text "Save" ] ]
            ]
        ]
