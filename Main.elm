port module Main exposing (..)

import Bootstrap.Button as Button
import Bootstrap.CDN as CDN
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
import Json.Decode
import Json.Encode
import ListHelper
import ProgrissStore as Store
    exposing
        ( Action
        , Context
        , ContextId
        , ProgrissStore
        , Project
        , ProjectId
        , decoder
        , empty
        )


type alias Model =
    { store : ProgrissStore
    , selectedContext : SelectedContext
    , newActionDescription : String
    }


type SelectedContext
    = AnywhereContext
    | SpecificContext ContextId
    | AllContexts


type Msg
    = ChangeContext SelectedContext
    | Save
    | ReceiveStore String
    | TriggerLoad
    | UpdateNewActionDescription String
    | CreateNewAction


port persistStore : String -> Cmd msg


port loadStore : (String -> msg) -> Sub msg


port triggerStoreLoad : () -> Cmd msg


initialStore : ProgrissStore
initialStore =
    let
        jsonData =
            """
                {
                    "actions": [
                        {"id": 1, "description": "Call architect about garden", "project_id": 1},
                        {"id": 2, "description": "Buy cat food", "context_id": 1},
                        {"id": 3, "description": "Call Florist about Mom's favourite flowers", "project_id": 2, "context_id": 2}
                    ],
                    "contexts": [
                        {"id": 1, "name": "Errands"},
                        {"id": 2, "name": "Calls"}
                    ],
                    "projects": [
                        {"id": 1, "title": "Build our familiy house"},
                        {"id": 2, "title": "Mom's Birthday"}
                    ],
                    "notes": [
                        {"id": 1, "body": "Don't forget she likes tulips the best! So if you can get those please do. Whatever you do though, don't get roses. She hates them!", "project_id": 2}
                    ]
                }
            """
    in
    case Json.Decode.decodeString Store.decoder jsonData of
        Ok store ->
            store

        Err message ->
            Store.empty


initialModel : ( Model, Cmd Msg )
initialModel =
    ( { store = initialStore
      , selectedContext = AnywhereContext
      , newActionDescription = ""
      }
    , Cmd.none
    )


main : Program Never Model Msg
main =
    Html.program { init = initialModel, view = view, update = update, subscriptions = subscriptions }


subscriptions : Model -> Sub Msg
subscriptions model =
    loadStore ReceiveStore


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UpdateNewActionDescription newActionDescription ->
            ( { model | newActionDescription = newActionDescription }, Cmd.none )

        ChangeContext selectedContext ->
            ( { model | selectedContext = selectedContext }, Cmd.none )

        Save ->
            ( model, persistStore (Json.Encode.encode 2 (Store.encoder model.store)) )

        TriggerLoad ->
            ( model, triggerStoreLoad () )

        ReceiveStore value ->
            case Json.Decode.decodeString Store.decoder value of
                Ok store ->
                    ( { model | store = store }, Cmd.none )

                Err message ->
                    ( { model | store = Store.empty }, Cmd.none )

        CreateNewAction ->
            let
                ( actionId, storeWithNewAction ) =
                    model.store
                        |> Store.createAction model.newActionDescription

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
            ( { model
                | store = updatedStore
                , newActionDescription = ""
              }
            , Cmd.none
            )


view : Model -> Html Msg
view model =
    Grid.container []
        [ Grid.row []
            [ Grid.col [] [ renderContextMenu model.store model.selectedContext ]
            , Grid.col []
                [ renderActions (actionsToRender model.store model.selectedContext)
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
        , hr [] []
        , projectsCardOverview model.store
        , hr [] []
        , Button.button
            [ Button.primary, Button.attrs [ onClick Save, Html.Attributes.class "bmd-btn-fab" ] ]
            [ i [ Html.Attributes.class "material-icons" ] [ text "save" ] ]
        , Button.button
            [ Button.primary, Button.attrs [ onClick TriggerLoad, Html.Attributes.class "bmd-btn-fab" ] ]
            [ i [ Html.Attributes.class "material-icons" ] [ text "restore" ] ]
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


getActions : Maybe ContextId -> ProgrissStore -> List Action
getActions selectedContext store =
    case selectedContext of
        Nothing ->
            Store.getAllActions store

        Just contextId ->
            Store.getActionsForContext contextId store


projectsCardOverview : ProgrissStore -> Html Msg
projectsCardOverview store =
    let
        allProjects =
            Store.getAllProjects store

        groupedProjects =
            ListHelper.groupsOf projectsPerRow allProjects []
    in
    div [] (List.map (\projectGroup -> Card.deck (List.map (projectCard store) projectGroup)) groupedProjects)


actionCard : Action -> Html Msg
actionCard action =
    Card.config []
        |> Card.block [] [ Block.text [] [ text action.description ] ]
        |> Card.view


projectCard : ProgrissStore -> Project -> Card.Config Msg
projectCard store project =
    Card.config []
        |> Card.block [] [ Block.titleH3 [] [ text project.title ] ]
        |> Card.listGroup (projectCardActionList project.id store)
        |> Card.block []
            [ Block.text []
                [ projectCardNoteList project.id store
                , Button.button
                    [ Button.primary, Button.attrs [ Html.Attributes.class "bmd-btn-fab" ] ]
                    [ i [ Html.Attributes.class "material-icons" ] [ text "grade" ] ]
                ]
            ]


projectCardActionList : ProjectId -> ProgrissStore -> List (ListGroup.Item Msg)
projectCardActionList projectId store =
    List.map
        (\action -> ListGroup.li [] [ text action.description ])
        (Store.getActionsForProject projectId store)


projectCardNoteList : ProjectId -> ProgrissStore -> Html Msg
projectCardNoteList projectId store =
    ul []
        (List.map
            (\note -> li [] [ text note.body ])
            (Store.getNotesForProject projectId store)
        )


projectsPerRow : Int
projectsPerRow =
    3



-- Click on project card to see project in full width
-- Tests for interactive elements
-- Creating contexts in the interface
-- Creating projects in the interface
-- Proper interface that shows everything in the graph
-- DoneState = Done Int (Completed At)| Active | SomedayMaybe Int (Resubmit At) | Deleted Int (Deleted At) for Projects
-- DoneState = Done Int (Completed At)| Active | Deleted Int (Deleted At) for Actions
-- Store the DoneState in the record. I thought about calculating the state from stored Events int the Graph, but that doesn't seem a good idea, because the data structure allows me to store two DoneAt events for a single Action. What would this mean?
-- Benchmark to see if the store is fast
