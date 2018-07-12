module Workflows.GtdActionLists exposing (Model, Msg, initialModel, update, view)

import Bootstrap.Button as Button
import Bootstrap.Card as Card
import Bootstrap.Card.Block as Block
import Bootstrap.Form as Form
import Bootstrap.Form.Input as Input
import Bootstrap.Form.InputGroup as InputGroup
import Bootstrap.Grid as Grid
import Bootstrap.ListGroup as ListGroup
import Html exposing (..)
import Html.Attributes exposing (href)
import Html.Events exposing (..)
import ProgrissStore as Store exposing (Action, Context, ContextId, ProgrissStore, Project, ProjectId)


type SelectedContext
    = AnywhereContext
    | SpecificContext ContextId
    | AllContexts


type Msg
    = ChangeContext SelectedContext
    | UpdateNewActionDescription String
    | CreateNewAction


type alias Model =
    { selectedContext : SelectedContext
    , newActionDescription : String
    }


initialModel : Model
initialModel =
    { selectedContext = AnywhereContext
    , newActionDescription = ""
    }


update : Msg -> ProgrissStore -> Model -> ( Model, ProgrissStore, Cmd Msg )
update msg store model =
    case msg of
        UpdateNewActionDescription newActionDescription ->
            ( { model | newActionDescription = newActionDescription }, store, Cmd.none )

        ChangeContext selectedContext ->
            ( { model | selectedContext = selectedContext }, store, Cmd.none )

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
    Grid.container []
        [ Grid.row []
            [ Grid.col [] [ renderContextMenu store model.selectedContext ]
            , Grid.col []
                [ renderActions (actionsToRender store model.selectedContext)
                , hr [] []
                , Form.form []
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
                                [ Button.secondary, Button.attrs [ onClick CreateNewAction ] ]
                                [ text "Create Action" ]
                            ]
                        |> InputGroup.view
                    ]
                ]
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
    Card.config []
        |> Card.block [] [ Block.text [] [ text action.description ] ]
        |> Card.view
