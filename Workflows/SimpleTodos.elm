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
import Html exposing (Html, div, hr, i, text)
import Html.Attributes exposing (action, href)
import Html.Events exposing (onSubmit)
import ProgrissStore as Store exposing (Action, ActionId, ActionState(Done), ProgrissStore, decoder)


type Msg
    = UpdateNewActionDescription String
    | CreateNewAction
    | ToggleActionDone ActionId


type alias Model =
    { newActionDescription : String
    }


initialModel : Model
initialModel =
    { newActionDescription = "" }


update : Msg -> ProgrissStore -> Model -> ( Model, ProgrissStore, Cmd Msg )
update msg store model =
    case msg of
        UpdateNewActionDescription newActionDescription ->
            ( { model | newActionDescription = newActionDescription }, store, Cmd.none )

        CreateNewAction ->
            let
                ( actionId, updatedStore ) =
                    Store.createAction model.newActionDescription store
            in
            ( { model | newActionDescription = "" }, updatedStore, Cmd.none )

        ToggleActionDone actionId ->
            ( model, Store.toggleActionDone actionId store, Cmd.none )


view : ProgrissStore -> Model -> Html Msg
view store model =
    Grid.container []
        [ Grid.row []
            [ Grid.col []
                [ renderActions (Store.getAllActions store)
                , hr [] []
                , Form.form [ onSubmit CreateNewAction, action "javascript:void(0);" ]
                    [ InputGroup.config
                        (InputGroup.text
                            [ Input.placeholder "Type action description here"
                            , Input.attrs
                                [ Html.Attributes.value model.newActionDescription
                                , Html.Events.onInput UpdateNewActionDescription
                                ]
                            ]
                        )
                        |> InputGroup.successors
                            [ InputGroup.button
                                [ Button.secondary, Button.attrs [] ]
                                [ text "Create Action" ]
                            ]
                        |> InputGroup.view
                    ]
                ]
            ]
        ]


renderActions : List Action -> Html Msg
renderActions actions =
    div [] (List.map (\action -> actionCard action) actions)


actionCard : Action -> Html Msg
actionCard action =
    Card.config (cardConfigForAction action)
        |> Card.block []
            [ Block.custom
                (Grid.row [ Row.middleXs ]
                    [ Grid.col [ Col.xs2, Col.md1 ]
                        [ Button.button
                            [ buttonColorForActionState action.state
                            , Button.attrs
                                [ Html.Events.onClick (ToggleActionDone action.id)
                                , Html.Attributes.class "bmd-btn-fab bmd-btn-fab-sm"
                                ]
                            ]
                            [ i [ Html.Attributes.class "material-icons" ]
                                [ text (iconForActionState action.state) ]
                            ]
                        ]
                    , Grid.col [] [ text action.description ]
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
