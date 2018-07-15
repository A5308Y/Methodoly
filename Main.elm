port module Main exposing (..)

import Bootstrap.Button as Button
import Bootstrap.Navbar as Navbar
import Html exposing (Html, div, hr, i, text)
import Html.Attributes exposing (href)
import Html.Events exposing (onClick)
import Json.Decode
import Json.Encode
import ProgrissStore as Store exposing (ProgrissStore)
import Workflows.GtdActionLists
import Workflows.ProjectOverview
import Workflows.SimpleTodos


navbar : Model -> Html Msg
navbar model =
    Navbar.config NavbarMsg
        |> Navbar.withAnimation
        |> Navbar.collapseExtraLarge
        |> Navbar.items
            [ Navbar.itemLink [ href "#", onClick (SelectWorkflow SimpleTodosWorkflow) ] [ text "Simple" ]
            , Navbar.itemLink [ href "#", onClick (SelectWorkflow GtdActionListsWorkflow) ] [ text "GTD" ]
            , Navbar.itemLink [ href "#", onClick (SelectWorkflow ProjectOverviewWorkflow) ] [ text "Projects" ]
            ]
        |> Navbar.view model.navbarState


type alias Model =
    { store : ProgrissStore
    , gtdActionListsModel : Workflows.GtdActionLists.Model
    , projectCardOverviewModel : Workflows.ProjectOverview.Model
    , simpleTodosModel : Workflows.SimpleTodos.Model
    , navbarState : Navbar.State
    , selectedWorkflow : SelectableWorkflow
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
    let
        ( navbarState, navbarCmd ) =
            Navbar.initialState NavbarMsg
    in
    ( { store = initialStore
      , gtdActionListsModel = Workflows.GtdActionLists.initialModel
      , projectCardOverviewModel = Workflows.ProjectOverview.initialModel
      , simpleTodosModel = Workflows.SimpleTodos.initialModel
      , navbarState = navbarState
      , selectedWorkflow = SimpleTodosWorkflow
      }
    , navbarCmd
    )


main : Program Never Model Msg
main =
    Html.program { init = initialModel, view = view, update = update, subscriptions = subscriptions }


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
            ( model, persistStore (Json.Encode.encode 2 (Store.encoder model.store)) )

        TriggerLoad ->
            ( model, triggerStoreLoad () )

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
            ( { model | selectedWorkflow = selectedWorkflow }, Cmd.none )


view : Model -> Html Msg
view model =
    div []
        [ if List.length (Store.getAllActions model.store) >= 10 then
            navbar model
          else
            text ""
        , renderWorkflow model
        , hr [] []
        , div []
            [ Button.button
                [ Button.primary, Button.attrs [ onClick Save, Html.Attributes.class "bmd-btn-fab" ] ]
                [ i [ Html.Attributes.class "material-icons" ] [ text "save" ] ]
            , Button.button
                [ Button.primary, Button.attrs [ onClick TriggerLoad, Html.Attributes.class "bmd-btn-fab" ] ]
                [ i [ Html.Attributes.class "material-icons" ] [ text "restore" ] ]
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
