module ProgrissStoreTest exposing (..)

import Expect exposing (Expectation)
import Json.Decode
import ProgrissStore exposing (Action, Context, ProgrissStore, Project)
import Test exposing (..)


suite : Test
suite =
    describe "ProgrissStore"
        [ describe "ProgrissStore.getAllActions"
            [ test "returns all actions in the store" <|
                \_ ->
                    Expect.equal
                        (ProgrissStore.getAllActions fixtureStore)
                        [ Action 1 "Call architect about garden"
                        , Action 2 "Buy cat food"
                        , Action 3 "Call Florist about Mom's favourite flowers"
                        ]
            ]
        , describe "ProgrissStore.getAllProjects"
            [ test "returns all projects in the store" <|
                \_ ->
                    Expect.equal
                        (ProgrissStore.getAllProjects fixtureStore)
                        [ Project 1 "Build our familiy house"
                        , Project 2 "Mom's Birthday"
                        ]
            ]
        , describe "ProgrissStore.getAllContexts"
            [ test "returns all contexts in the store" <|
                \_ ->
                    Expect.equal
                        (ProgrissStore.getAllContexts fixtureStore)
                        [ Context 1 "Errands"
                        , Context 2 "Calls"
                        ]
            ]
        , describe "ProgrissStore.getActionsForContext"
            [ test "returns all actions that are associated to the context" <|
                \_ ->
                    Expect.equal
                        (ProgrissStore.getActionsForContext 1 fixtureStore)
                        [ Action 2 "Buy cat food" ]
            ]
        , describe "ProgrissStore.getContextForAction"
            [ test "returns Nothing if the Action is not associated to a Context" <|
                \_ ->
                    Expect.equal
                        (ProgrissStore.getContextForAction 1 fixtureStore)
                        Nothing
            , test "returns the context the actions is associated to" <|
                \_ ->
                    Expect.equal
                        (ProgrissStore.getContextForAction 2 fixtureStore)
                        (Just (Context 1 "Errands"))
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
