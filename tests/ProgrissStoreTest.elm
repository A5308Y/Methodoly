module ProgrissStoreTest exposing (..)

import Expect exposing (Expectation)
import Json.Decode
import ProgrissStore
    exposing
        ( Action
        , ActionId(ActionId)
        , Context
        , ContextId(ContextId)
        , ProgrissStore
        , Project
        , ProjectId(ProjectId)
        )
import Test exposing (..)


suite : Test
suite =
    describe "ProgrissStore"
        [ describe "ProgrissStore.getAllActions"
            [ test "returns all actions in the store" <|
                \_ ->
                    Expect.equal
                        (ProgrissStore.getAllActions fixtureStore)
                        [ Action (ActionId 1) "Call architect about garden"
                        , Action (ActionId 2) "Buy cat food"
                        , Action (ActionId 3) "Call Florist about Mom's favourite flowers"
                        ]
            ]
        , describe "ProgrissStore.getAllContexts"
            [ test "returns all contexts in the store" <|
                \_ ->
                    Expect.equal
                        (ProgrissStore.getAllContexts fixtureStore)
                        [ Context (ContextId 1) "Errands"
                        , Context (ContextId 2) "Calls"
                        ]
            ]
        , describe "ProgrissStore.getAllProjects"
            [ test "returns all projects in the store" <|
                \_ ->
                    Expect.equal
                        (ProgrissStore.getAllProjects fixtureStore)
                        [ Project (ProjectId 1) "Build our familiy house"
                        , Project (ProjectId 2) "Mom's Birthday"
                        ]
            ]
        , describe "ProgrissStore.getActionsForContext"
            [ test "returns all actions that are associated to the context" <|
                \_ ->
                    Expect.equal
                        (ProgrissStore.getActionsForContext (ContextId 1) fixtureStore)
                        [ Action (ActionId 2) "Buy cat food" ]
            ]
        , describe "ProgrissStore.getContextForAction"
            [ test "returns Nothing if the Action is not associated to a Context" <|
                \_ ->
                    Expect.equal
                        (ProgrissStore.getContextForAction (ActionId 1) fixtureStore)
                        Nothing
            , test "returns the context the actions is associated to" <|
                \_ ->
                    Expect.equal
                        (ProgrissStore.getContextForAction (ActionId 2) fixtureStore)
                        (Just (Context (ContextId 1) "Errands"))
            ]
        , describe "ProgrissStore.getProjectForAction"
            [ test "returns Nothing if the Action is not associated to a project" <|
                \_ ->
                    Expect.equal
                        (ProgrissStore.getProjectForAction (ActionId 1) fixtureStore)
                        (Just (Project (ProjectId 1) "Build our familiy house"))
            , test "returns the project the actions is associated to" <|
                \_ ->
                    Expect.equal
                        (ProgrissStore.getProjectForAction (ActionId 2) fixtureStore)
                        Nothing
            ]
        , describe "ProgrissStore.createAction"
            [ test "returns a new store with the newly created action" <|
                \_ ->
                    Expect.equal
                        (ProgrissStore.empty
                            |> ProgrissStore.createAction "Call Electrician"
                            |> ProgrissStore.createAction "Call Sam"
                            |> ProgrissStore.getAllActions
                        )
                        [ Action (ActionId 1) "Call Electrician"
                        , Action (ActionId 2) "Call Sam"
                        ]
            ]
        , describe "ProgrissStore.associateActionToContext"
            [ test "returns a new store with the newly associated action" <|
                \_ ->
                    Expect.equal
                        (ProgrissStore.empty
                            |> ProgrissStore.createAction "Call Electrician"
                            |> ProgrissStore.createContext "Calls"
                            |> ProgrissStore.associateActionToContext (ActionId 1) (ContextId 1)
                            |> ProgrissStore.getActionsForContext (ContextId 1)
                        )
                        [ Action (ActionId 1) "Call Electrician"
                        ]
            ]
        , describe "ProgrissStore.updateAction"
            [ test "returns a new store with the updated action" <|
                \_ ->
                    Expect.equal
                        (fixtureStore
                            |> ProgrissStore.updateAction (Action (ActionId 1) "Call architect about possible garden path layouts")
                            |> ProgrissStore.getAllActions
                        )
                        [ Action (ActionId 1) "Call architect about possible garden path layouts"
                        , Action (ActionId 2) "Buy cat food"
                        , Action (ActionId 3) "Call Florist about Mom's favourite flowers"
                        ]
            ]
        ]


fixtureStore : ProgrissStore
fixtureStore =
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
                ]
            }
            """
    in
    case Json.Decode.decodeString ProgrissStore.decoder jsonData of
        Err message ->
            Debug.crash message

        Ok store ->
            store
