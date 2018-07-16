module Workflows.GtdActionLists exposing (Model, Msg, initialModel, navbarContent, update, view)

import Bootstrap.Button as Button
import Bootstrap.Card as Card
import Bootstrap.Card.Block as Block
import Bootstrap.Form as Form
import Bootstrap.Form.Input as Input
import Bootstrap.Form.InputGroup as InputGroup
import Bootstrap.Grid as Grid
import Bootstrap.Grid.Col as Col
import Bootstrap.Grid.Row as Row
import Bootstrap.ListGroup as ListGroup
import Html exposing (Html, div, hr, i, text)
import Html.Attributes exposing (action, class, href)
import Html.Events exposing (onClick)
import ProgrissStore as Store exposing (Action, ActionId, ActionState(..), Context, ContextId, ProgrissStore)


type SelectedContext
    = AnywhereContext
    | SpecificContext ContextId
    | AllContexts


type Msg
    = ChangeContext SelectedContext
    | UpdateNewActionDescription String
    | CreateNewAction
    | ToggleActionDone ActionId
    | ToggleContextMenu Bool


type alias Model =
    { selectedContext : SelectedContext
    , newActionDescription : String
    , contextMenuVisible : Bool
    }


initialModel : Model
initialModel =
    { selectedContext = AnywhereContext
    , newActionDescription = ""
    , contextMenuVisible = False
    }


update : Msg -> ProgrissStore -> Model -> ( Model, ProgrissStore, Cmd Msg )
update msg store model =
    case msg of
        UpdateNewActionDescription newActionDescription ->
            ( { model | newActionDescription = newActionDescription }, store, Cmd.none )

        ChangeContext selectedContext ->
            ( { model | selectedContext = selectedContext, contextMenuVisible = False }, store, Cmd.none )

        ToggleActionDone actionId ->
            ( model, Store.toggleActionDone actionId store, Cmd.none )

        ToggleContextMenu state ->
            ( { model | contextMenuVisible = state }, store, Cmd.none )

        CreateNewAction ->
            let
                ( actionId, storeWithNewAction ) =
                    Store.createAction model.newActionDescription store

                updatedStore =
                    case model.selectedContext of
                        SpecificContext contextId ->
                            storeWithNewAction
                                |> Store.associateActionToContext actionId contextId

                        AllContexts ->
                            storeWithNewAction

                        AnywhereContext ->
                            storeWithNewAction
            in
            ( { model | newActionDescription = "" }, updatedStore, Cmd.none )


view : ProgrissStore -> Model -> Html Msg
view store model =
    if model.contextMenuVisible then
        Grid.container []
            [ renderContextMenu store model.selectedContext ]
    else
        Grid.container []
            [ renderActions (actionsToRender store model.selectedContext)
            , hr [] []
            , Form.form [ Html.Events.onSubmit CreateNewAction, action "javascript:void(0);" ]
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
                            [ Button.success
                            , Button.disabled (String.isEmpty model.newActionDescription)
                            , Button.attrs []
                            ]
                            [ text "Create Action" ]
                        ]
                    |> InputGroup.view
                ]
            ]


renderContextMenu : ProgrissStore -> SelectedContext -> Html Msg
renderContextMenu store selectedContext =
    ListGroup.custom
        (store
            |> Store.getAllContexts
            |> List.map (clickableContext selectedContext)
            |> (::) (clickableAnywhereContext selectedContext)
            |> (::) (clickableAllContexts selectedContext)
        )


clickableAllContexts : SelectedContext -> ListGroup.CustomItem Msg
clickableAllContexts selectedContext =
    ListGroup.anchor (clickableContextAttributes selectedContext AllContexts) [ text "All" ]


clickableAnywhereContext : SelectedContext -> ListGroup.CustomItem Msg
clickableAnywhereContext selectedContext =
    ListGroup.anchor (clickableContextAttributes selectedContext AnywhereContext) [ text "Anywhere" ]


clickableContext : SelectedContext -> Context -> ListGroup.CustomItem Msg
clickableContext selectedContext context =
    ListGroup.anchor
        (clickableContextAttributes selectedContext (SpecificContext context.id))
        [ text context.name ]


clickableContextAttributes : SelectedContext -> SelectedContext -> List (ListGroup.ItemOption Msg)
clickableContextAttributes selectedContext context =
    if selectedContext == context then
        [ ListGroup.active, ListGroup.attrs [ href "#", onClick (ChangeContext context) ] ]
    else
        [ ListGroup.attrs [ href "#", onClick (ChangeContext context) ] ]


actionsToRender : ProgrissStore -> SelectedContext -> List Action
actionsToRender store selectedContext =
    case selectedContext of
        AllContexts ->
            Store.getAllActions store

        AnywhereContext ->
            Store.getActionsWithoutContext store

        SpecificContext contextId ->
            Store.getActionsForContext contextId store


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


cardConfigForAction : Action -> List (Card.Option msg)
cardConfigForAction action =
    case action.state of
        Done time ->
            [ Card.light ]

        _ ->
            []


navbarContent : ProgrissStore -> Model -> Html Msg
navbarContent store model =
    Html.button
        [ class "btn btn-outline-primary"
        , onClick (ToggleContextMenu (not model.contextMenuVisible))
        ]
        [ text (contextName store model.selectedContext) ]


contextName : ProgrissStore -> SelectedContext -> String
contextName store selectedContext =
    case selectedContext of
        AllContexts ->
            "All"

        AnywhereContext ->
            "Anywhere"

        SpecificContext contextId ->
            Store.getContext contextId store
                |> Maybe.map .name
                |> Maybe.withDefault ""
