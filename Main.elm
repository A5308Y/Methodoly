port module Main exposing (..)

import Bootstrap.Button as Button
import Html exposing (Html, div, hr, i, text)
import Html.Attributes exposing (href)
import Html.Events exposing (onClick)
import Json.Decode
import Json.Encode
import ProgrissStore as Store exposing (ProgrissStore)
import Workflows.GtdActionLists
import Workflows.ProjectCardOverview
import Workflows.SimpleTodos


type alias Model =
    { store : ProgrissStore
    , gtdActionListsModel : Workflows.GtdActionLists.Model
    , projectCardOverviewModel : Workflows.ProjectCardOverview.Model
    , simpleTodosModel : Workflows.SimpleTodos.Model
    }


type Msg
    = GtdActionListsMsg Workflows.GtdActionLists.Msg
    | ProjectCardOverviewMsg Workflows.ProjectCardOverview.Msg
    | SimpleTodosMsg Workflows.SimpleTodos.Msg
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
    ( { store = initialStore
      , gtdActionListsModel = Workflows.GtdActionLists.initialModel
      , projectCardOverviewModel = Workflows.ProjectCardOverview.initialModel
      , simpleTodosModel = Workflows.SimpleTodos.initialModel
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

        ProjectCardOverviewMsg msg ->
            let
                ( projectCardOverviewModel, store, cmd ) =
                    Workflows.ProjectCardOverview.update msg model.store model.projectCardOverviewModel
            in
            ( { model | store = store, projectCardOverviewModel = projectCardOverviewModel }
            , Cmd.map ProjectCardOverviewMsg cmd
            )

        SimpleTodosMsg msg ->
            let
                ( simpleTodosModel, store, cmd ) =
                    Workflows.SimpleTodos.update msg model.store model.simpleTodosModel
            in
            ( { model | store = store, simpleTodosModel = simpleTodosModel }
            , Cmd.map SimpleTodosMsg cmd
            )


view : Model -> Html Msg
view model =
    div []
        [ renderWorkflow model
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
    if List.length (Store.getAllActions model.store) < 10 then
        Html.map SimpleTodosMsg (Workflows.SimpleTodos.view model.store model.simpleTodosModel)
    else
        div []
            [ Html.map
                GtdActionListsMsg
                (Workflows.GtdActionLists.view model.store model.gtdActionListsModel)
            , hr [] []
            , Html.map
                ProjectCardOverviewMsg
                (Workflows.ProjectCardOverview.view model.store model.projectCardOverviewModel)
            ]
