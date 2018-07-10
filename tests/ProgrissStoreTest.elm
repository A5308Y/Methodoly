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
                        [ ( 1, Action "Call architect about garden" Nothing (Just 1) )
                        , ( 2, Action "Buy cat food" (Just 1) Nothing )
                        , ( 3, Action "Call Florist about Mom's favourite flowers" (Just 2) (Just 2) )
                        ]
            ]
        , describe "ProgrissStore.getAllProjects"
            [ test "returns all projects in the store" <|
                \_ ->
                    Expect.equal
                        (ProgrissStore.getAllProjects fixtureStore)
                        [ ( 1, Project "Build our familiy house" )
                        , ( 2, Project "Mom's Birthday" )
                        ]
            ]
        , describe "ProgrissStore.getAllContexts"
            [ test "returns all contexts in the store" <|
                \_ ->
                    Expect.equal
                        (ProgrissStore.getAllContexts fixtureStore)
                        [ ( 1, Context "Errands" )
                        , ( 2, Context "Calls" )
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
