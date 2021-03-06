port module Main exposing (main)

import Html exposing (Html, a, div, header, i, text, ul)
import Html.Attributes exposing (class, classList, href)
import Html.Events exposing (onClick, onWithOptions)
import Http
import Json.Decode exposing (int, string)
import Json.Decode.Pipeline exposing (decode, required)
import Json.Encode
import Navigation
import ProgrissStore as Store exposing (ProgrissStore)
import Workflows.GtdActionLists
import Workflows.ProjectOverview
import Workflows.Settings
import Workflows.SimpleTodos


type alias Model =
    { store : ProgrissStore
    , gtdActionListsModel : Workflows.GtdActionLists.Model
    , projectCardOverviewModel : Workflows.ProjectOverview.Model
    , simpleTodosModel : Workflows.SimpleTodos.Model
    , settingsModel : Workflows.Settings.Model
    , selectedWorkflow : SelectableWorkflow
    , workflowMenuVisible : Bool
    , pasteBinId : Maybe String
    }


type alias PasteBinResult =
    { uri : String }


type SelectableWorkflow
    = SimpleTodosWorkflow
    | GtdActionListsWorkflow
    | ProjectOverviewWorkflow
    | SettingsWorkflow


type Msg
    = GtdActionListsMsg Workflows.GtdActionLists.Msg
    | ProjectOverviewMsg Workflows.ProjectOverview.Msg
    | SimpleTodosMsg Workflows.SimpleTodos.Msg
    | SettingsMsg Workflows.Settings.Msg
    | SelectWorkflow SelectableWorkflow
    | Save
    | ReceiveStore String
    | ReceiveStoreFromBin (Result Http.Error ProgrissStore)
    | TriggerStoreLoad
    | TriggerBinLoad String
    | ToggleDrawer Bool
    | RecieveBinConfirmation (Result Http.Error PasteBinResult)
    | Noop


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
                        {"id": 2, "title": "Mom's Birthday"},
                        {"id": 3, "title": "Write Essay about my favourite fish"},
                        {"id": 4, "title": "Clean Garage"}
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


pasteBinResultDecoder : Json.Decode.Decoder PasteBinResult
pasteBinResultDecoder =
    decode PasteBinResult
        |> required "uri" string


initialModel : Navigation.Location -> ( Model, Cmd Msg )
initialModel location =
    let
        search =
            String.split "#" location.hash |> List.reverse |> List.head
    in
    ( { store = initialStore
      , gtdActionListsModel = Workflows.GtdActionLists.initialModel
      , projectCardOverviewModel = Workflows.ProjectOverview.initialModel
      , simpleTodosModel = Workflows.SimpleTodos.initialModel
      , settingsModel = Workflows.Settings.initialModel
      , selectedWorkflow = GtdActionListsWorkflow
      , workflowMenuVisible = False
      , pasteBinId =
            if String.isEmpty (Maybe.withDefault "" search) then
                Nothing
            else
                search
      }
    , case search of
        Nothing ->
            Cmd.none

        Just possibleBinId ->
            case possibleBinId of
                "" ->
                    Cmd.none

                _ ->
                    Http.send ReceiveStoreFromBin (getFromBin (binUri possibleBinId))
    )


main : Program Never Model Msg
main =
    Navigation.program
        (\location -> Noop)
        { init = initialModel
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


subscriptions : Model -> Sub Msg
subscriptions model =
    loadStore ReceiveStore


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Noop ->
            ( model, Cmd.none )

        Save ->
            ( { model | workflowMenuVisible = False }
            , Cmd.batch
                [ persistStore (Json.Encode.encode 2 (Store.encoder model.store))
                , case model.pasteBinId of
                    Nothing ->
                        Http.send RecieveBinConfirmation (postToBin (Store.encoder model.store))

                    Just id ->
                        Http.send ReceiveStoreFromBin (putToBin (binUri id) (Store.encoder model.store))
                ]
            )

        TriggerStoreLoad ->
            ( { model | workflowMenuVisible = False }, triggerStoreLoad () )

        TriggerBinLoad id ->
            ( { model | workflowMenuVisible = False }
            , Http.send ReceiveStoreFromBin (getFromBin (binUri id))
            )

        ReceiveStore value ->
            case Json.Decode.decodeString Store.decoder value of
                Ok store ->
                    ( { model | store = store }, Cmd.none )

                Err message ->
                    ( { model | store = Store.empty }, Cmd.none )

        ReceiveStoreFromBin result ->
            case result of
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

        SettingsMsg msg ->
            let
                ( settingsModel, store, cmd ) =
                    Workflows.Settings.update msg model.store model.settingsModel
            in
            ( { model | store = store, settingsModel = settingsModel }
            , Cmd.map SettingsMsg cmd
            )

        SelectWorkflow selectedWorkflow ->
            ( { model
                | selectedWorkflow = selectedWorkflow
                , workflowMenuVisible = False
              }
            , Cmd.none
            )

        ToggleDrawer newState ->
            ( { model | workflowMenuVisible = newState }, Cmd.none )

        RecieveBinConfirmation result ->
            case result of
                Ok pasteBin ->
                    let
                        pasteBinId =
                            String.split "/" pasteBin.uri |> List.reverse |> List.head
                    in
                    ( { model | pasteBinId = pasteBinId }
                    , case pasteBinId of
                        Nothing ->
                            Cmd.none

                        Just id ->
                            Navigation.modifyUrl ("https://a5308y.github.io/Methodoly#" ++ id)
                      --Navigation.modifyUrl ("file:///Users/ahe/code/repositories/Active/ProgrissStore/index.html#" ++ id)
                    )

                Err _ ->
                    ( model, Cmd.none )


binUri : String -> String
binUri id =
    baseBinUri ++ "/" ++ id


baseBinUri : String
baseBinUri =
    "https://api.myjson.com/bins"


postToBin : Json.Encode.Value -> Http.Request PasteBinResult
postToBin jsonValue =
    Http.post baseBinUri (Http.jsonBody jsonValue) pasteBinResultDecoder


putToBin : String -> Json.Encode.Value -> Http.Request ProgrissStore
putToBin uri jsonValue =
    Http.request
        { method = "PUT"
        , headers = []
        , url = uri
        , body = Http.jsonBody jsonValue
        , expect = Http.expectJson Store.decoder
        , timeout = Nothing
        , withCredentials = False
        }


getFromBin : String -> Http.Request ProgrissStore
getFromBin uri =
    Http.get uri Store.decoder


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
            , ( "bmd-drawer-in", model.workflowMenuVisible )
            , ( "side-menu", True )
            ]
        ]
        [ header [ class "bmd-layout-header" ]
            [ div [ class "navbar navbar-light bg-faded" ]
                [ a [ class "btn", onClick (ToggleDrawer (not model.workflowMenuVisible)) ]
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
            [ header [] [ a [ class "navbar-brand" ] [ text "Methodoly" ] ]
            , ul [ class "list-group" ]
                [ a
                    [ href "#"
                    , class "list-group-item"
                    , onWithOptions "click"
                        { stopPropagation = True, preventDefault = True }
                        (Json.Decode.succeed (SelectWorkflow SimpleTodosWorkflow))
                    ]
                    [ text "Simple" ]
                , a
                    [ href "#"
                    , class "list-group-item"
                    , onWithOptions "click"
                        { stopPropagation = True, preventDefault = True }
                        (Json.Decode.succeed (SelectWorkflow GtdActionListsWorkflow))
                    ]
                    [ text "GTD" ]
                , a
                    [ href "#"
                    , class "list-group-item"
                    , onWithOptions "click"
                        { stopPropagation = True, preventDefault = True }
                        (Json.Decode.succeed (SelectWorkflow ProjectOverviewWorkflow))
                    ]
                    [ text "Projects" ]
                , div [ class "dropdown-divider" ] []
                , a
                    [ href "#"
                    , class "list-group-item"
                    , onWithOptions "click"
                        { stopPropagation = True, preventDefault = True }
                        (Json.Decode.succeed (SelectWorkflow SettingsWorkflow))
                    ]
                    [ text "Settings" ]
                , a
                    [ href "#"
                    , class "list-group-item"
                    , onWithOptions "click"
                        { stopPropagation = True, preventDefault = True }
                        (Json.Decode.succeed Save)
                    ]
                    [ text "Save" ]
                , a
                    [ href "#"
                    , class "list-group-item"
                    , onWithOptions "click"
                        { stopPropagation = True, preventDefault = True }
                        (Json.Decode.succeed TriggerStoreLoad)
                    ]
                    [ text "Load" ]
                , case model.pasteBinId of
                    Nothing ->
                        text ""

                    Just id ->
                        a
                            [ href "#"
                            , class "list-group-item"
                            , onWithOptions "click"
                                { stopPropagation = True, preventDefault = True }
                                (Json.Decode.succeed (TriggerBinLoad (binUri id)))
                            ]
                            [ text "Load from server" ]
                ]
            ]
        ]


renderWorkflow : Model -> Html Msg
renderWorkflow model =
    case model.selectedWorkflow of
        SimpleTodosWorkflow ->
            Html.map SimpleTodosMsg (Workflows.SimpleTodos.view model.store model.simpleTodosModel)

        SettingsWorkflow ->
            Html.map SettingsMsg (Workflows.Settings.view model.store model.settingsModel)

        GtdActionListsWorkflow ->
            Html.map
                GtdActionListsMsg
                (Workflows.GtdActionLists.view model.store model.gtdActionListsModel)

        ProjectOverviewWorkflow ->
            Html.map
                ProjectOverviewMsg
                (Workflows.ProjectOverview.view model.store model.projectCardOverviewModel)
