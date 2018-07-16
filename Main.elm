port module Main exposing (..)

import Bootstrap.Button as Button
import Bootstrap.Navbar as Navbar
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Json.Decode
import Json.Encode
import ProgrissStore as Store exposing (ProgrissStore)
import Workflows.GtdActionLists
import Workflows.ProjectOverview
import Workflows.SimpleTodos


type alias Model =
    { store : ProgrissStore
    , gtdActionListsModel : Workflows.GtdActionLists.Model
    , projectCardOverviewModel : Workflows.ProjectOverview.Model
    , simpleTodosModel : Workflows.SimpleTodos.Model
    , navbarState : Navbar.State
    , selectedWorkflow : SelectableWorkflow
    , drawerVisible : Bool
    }


type SelectableWorkflow
    = SimpleTodosWorkflow
    | GtdActionListsWorkflow
    | ProjectOverviewWorkflow


type Msg
    = GtdActionListsMsg Workflows.GtdActionLists.Msg
    | ProjectOverviewMsg Workflows.ProjectOverview.Msg
    | SimpleTodosMsg Workflows.SimpleTodos.Msg
    | NavbarMsg Navbar.State
    | SelectWorkflow SelectableWorkflow
    | Save
    | ReceiveStore String
    | TriggerLoad
    | ToggleDrawer Bool


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
      , gtdActionListsModel = Workflows.GtdActionLists.initialModel
      , projectCardOverviewModel = Workflows.ProjectOverview.initialModel
      , simpleTodosModel = Workflows.SimpleTodos.initialModel
      , selectedWorkflow = SimpleTodosWorkflow
      , drawerVisible = False
      }
    , Cmd.none
    )


main : Program Never Model Msg
main =
    Html.program
        { init = initialModel
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ loadStore ReceiveStore
        , Navbar.subscriptions model.navbarState NavbarMsg
        ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Save ->
            ( { model | drawerVisible = False }
            , persistStore (Json.Encode.encode 2 (Store.encoder model.store))
            )

        TriggerLoad ->
            ( { model | drawerVisible = False }, triggerStoreLoad () )

        ReceiveStore value ->
            case Json.Decode.decodeString Store.decoder value of
                Ok store ->
                    ( { model | store = store }, Cmd.none )

                Err message ->
                    ( { model | store = Store.empty }, Cmd.none )

        GtdActionListsMsg msg ->
            let
                ( gtdActionListsModel, store, cmd ) =
                    Workflows.GtdActionLists.update msg model.store model.gtdActionListsModel
            in
            ( { model | store = store, gtdActionListsModel = gtdActionListsModel }
            , Cmd.map GtdActionListsMsg cmd
            )

        ProjectOverviewMsg msg ->
            let
                ( projectCardOverviewModel, store, cmd ) =
                    Workflows.ProjectOverview.update msg model.store model.projectCardOverviewModel
            in
            ( { model | store = store, projectCardOverviewModel = projectCardOverviewModel }
            , Cmd.map ProjectOverviewMsg cmd
            )

        SimpleTodosMsg msg ->
            let
                ( simpleTodosModel, store, cmd ) =
                    Workflows.SimpleTodos.update msg model.store model.simpleTodosModel
            in
            ( { model | store = store, simpleTodosModel = simpleTodosModel }
            , Cmd.map SimpleTodosMsg cmd
            )

        NavbarMsg state ->
            ( { model | navbarState = state }, Cmd.none )

        SelectWorkflow selectedWorkflow ->
            ( { model | selectedWorkflow = selectedWorkflow, drawerVisible = False }
            , Cmd.none
            )

        ToggleDrawer newState ->
            ( { model | drawerVisible = newState }, Cmd.none )


view : Model -> Html Msg
view model =
    div [] [ workflowMenu model, renderWorkflow model ]


workflowMenu : Model -> Html Msg
workflowMenu model =
    div
        [ classList
            [ ( "bmd-layout-container", True )
            , ( "bmd-drawer-f-l", True )
            , ( "bmd-drawer-overlay", True )
            , ( "bmd-drawer-in", model.drawerVisible )
            ]
        , style [ ( "position", "static" ) ]
        ]
        [ header [ class "bmd-layout-header" ]
            [ div [ class "navbar navbar-light bg-faded" ]
                [ a [ href "#", class "btn", onClick (ToggleDrawer (not model.drawerVisible)) ]
                    [ i [ class "material-icons" ] [ text "menu" ] ]
                , case model.selectedWorkflow of
                    GtdActionListsWorkflow ->
                        Workflows.GtdActionLists.navbarContent model.store model.gtdActionListsModel
                            |> Html.map GtdActionListsMsg

                    _ ->
                        text ""
                ]
            ]
        , div
            [ classList [ ( "bmd-layout-drawer", True ), ( "bg-faded", True ) ] ]
            [ header [] [ a [ class "navbar-brand" ] [ text "Title" ] ]
            , ul [ class "list-group" ]
                [ a [ href "#", class "list-group-item", onClick (SelectWorkflow SimpleTodosWorkflow) ]
                    [ text "Simple" ]
                , a [ href "#", class "list-group-item", onClick (SelectWorkflow GtdActionListsWorkflow) ]
                    [ text "GTD" ]
                , a [ href "#", class "list-group-item", onClick (SelectWorkflow ProjectOverviewWorkflow) ]
                    [ text "Projects" ]
                , a [ href "#", class "list-group-item", onClick Save ]
                    [ text "Save" ]
                , a [ href "#", class "list-group-item", onClick TriggerLoad ]
                    [ text "Load" ]
                ]
            ]
        ]


renderWorkflow : Model -> Html Msg
renderWorkflow model =
    case model.selectedWorkflow of
        SimpleTodosWorkflow ->
            Html.map SimpleTodosMsg (Workflows.SimpleTodos.view model.store model.simpleTodosModel)

        GtdActionListsWorkflow ->
            Html.map
                GtdActionListsMsg
                (Workflows.GtdActionLists.view model.store model.gtdActionListsModel)

        ProjectOverviewWorkflow ->
            Html.map
                ProjectOverviewMsg
                (Workflows.ProjectOverview.view model.store model.projectCardOverviewModel)
