module Workflows.SimpleTodos exposing (Model, Msg, initialModel, update, view)

import Bootstrap.Button as Button
import Bootstrap.Card as Card
import Bootstrap.Card.Block as Block
import Bootstrap.Form as Form
import Bootstrap.Form.Input as Input
import Bootstrap.Form.InputGroup as InputGroup
import Bootstrap.Grid as Grid
import Bootstrap.Grid.Col as Col
import Bootstrap.Grid.Row as Row
import Dom
import Html exposing (Html, div, hr, i, text)
import Html.Attributes exposing (action, class, defaultValue, id, value)
import Html.Events exposing (onClick, onInput, onSubmit, onWithOptions)
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
                            "edit-" ++ toString actionId
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
    Grid.container []
        [ Grid.row []
            [ Grid.col []
                [ renderActions model.editing (Store.getAllActions store)
                , renderNewActionFormCard model
                ]
            ]
        ]


renderNewActionFormCard : Model -> Html Msg
renderNewActionFormCard model =
    Card.config [ Card.light ]
        |> Card.block []
            [ Block.custom
                (Grid.row [ Row.middleXs ]
                    [ Grid.col [ Col.xs2, Col.md1 ]
                        [ Button.button
                            [ Button.success
                            , Button.attrs
                                [ class "bmd-btn-fab bmd-btn-fab-sm"
                                , onClick (SetFocusTo "new-action-description")
                                ]
                            ]
                            [ i [ class "material-icons" ] [ text "add" ] ]
                        ]
                    , Grid.col [] [ newActionForm model ]
                    ]
                )
            ]
        |> Card.view


newActionForm : Model -> Html Msg
newActionForm model =
    Form.form
        [ onSubmit CreateNewAction
        , Html.Attributes.action "javascript:void(0);"
        ]
        [ InputGroup.config
            (InputGroup.text
                [ Input.placeholder "Add an Action"
                , Input.attrs
                    [ value model.newActionDescription
                    , onInput UpdateNewActionDescription
                    , id "new-action-description"
                    ]
                ]
            )
            |> InputGroup.successors
                [ InputGroup.button
                    [ Button.success
                    , Button.disabled (String.isEmpty model.newActionDescription)
                    , Button.attrs []
                    ]
                    [ text "Create Action" ]
                ]
            |> InputGroup.view
        ]


renderActions : Maybe ActionId -> List Action -> Html Msg
renderActions editing actions =
    div [] (List.map (\action -> actionCard editing action) actions)


actionCard : Maybe ActionId -> Action -> Html Msg
actionCard editing action =
    Card.config (cardConfigForAction action)
        |> Card.block [ Block.attrs [ onClick (ToggleEditAction (Just action.id)) ] ]
            [ Block.custom
                (Grid.row [ Row.middleXs ]
                    [ Grid.col [ Col.xs2, Col.md1 ]
                        [ Button.button
                            [ buttonColorForActionState action.state
                            , Button.attrs
                                [ onWithOptions "click"
                                    { preventDefault = True, stopPropagation = True }
                                    (Json.Decode.succeed (ToggleActionDone action.id))
                                , class "bmd-btn-fab bmd-btn-fab-sm"
                                ]
                            ]
                            [ i [ class "material-icons" ]
                                [ text (iconForActionState action.state) ]
                            ]
                        ]
                    , if editing == Just action.id then
                        Grid.col
                            []
                            [ Form.form
                                [ class "edit-action-form"
                                , Html.Attributes.action "javascript:void(0);"
                                , onSubmit (ToggleEditAction Nothing)
                                ]
                                [ InputGroup.config
                                    (InputGroup.text
                                        [ Input.small
                                        , Input.attrs
                                            [ onInput (UpdateActionDescription action)
                                            , defaultValue action.description
                                            , id ("edit-" ++ toString action.id)
                                            ]
                                        ]
                                    )
                                    |> InputGroup.successors
                                        [ InputGroup.button [ Button.success ] [ text "Save" ] ]
                                    |> InputGroup.view
                                ]
                            ]

                      else
                        Grid.col
                            [ Col.attrs [] ]
                            [ text action.description ]
                    ]
                )
            ]
        |> Card.view


buttonColorForActionState : ActionState -> Button.Option Msg
buttonColorForActionState state =
    case state of
        Done time ->
            Button.success

        _ ->
            Button.primary


iconForActionState : ActionState -> String
iconForActionState state =
    case state of
        Done time ->
            "done"

        _ ->
            "check_box_outline_blank"


cardConfigForAction : Action -> List (Card.Option Msg)
cardConfigForAction action =
    case action.state of
        Done time ->
            [ Card.light ]

        _ ->
            []
