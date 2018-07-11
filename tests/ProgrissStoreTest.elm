module ProgrissStoreTest exposing (..)

import Expect exposing (Expectation)
import Json.Decode
import ProgrissStore exposing (Action, ProgrissStore)
import Test exposing (..)


suite : Test
suite =
    describe "ProgrissStore"
        [ describe "ProgrissStore.getAllActions"
            [ test "returns all actions in the store" <|
                \_ ->
                    Expect.equal
                        (ProgrissStore.getAllActions fixtureStore |> List.map .description)
                        [ "Call architect about garden"
                        , "Buy cat food"
                        , "Call Florist about Mom's favourite flowers"
                        ]
            ]
        , describe "ProgrissStore.getAllContexts"
            [ test "returns all contexts in the store" <|
                \_ ->
                    Expect.equal
                        (ProgrissStore.getAllContexts fixtureStore |> List.map .name)
                        [ "Errands", "Calls" ]
            ]
        , describe "ProgrissStore.getAllProjects"
            [ test "returns all projects in the store" <|
                \_ ->
                    Expect.equal
                        (ProgrissStore.getAllProjects fixtureStore |> List.map .title)
                        [ "Build our familiy house", "Mom's Birthday" ]
            ]
        , describe "ProgrissStore.getActionsForContext"
            [ test "returns all actions that are associated to the context" <|
                \_ ->
                    Expect.equal
                        (fixtureStore
                            |> ProgrissStore.getAllContexts
                            |> List.map
                                (\context ->
                                    ( context.name
                                    , fixtureStore
                                        |> ProgrissStore.getActionsForContext context.id
                                        |> List.map .description
                                    )
                                )
                        )
                        [ ( "Errands", [ "Buy cat food" ] )
                        , ( "Calls", [ "Call Florist about Mom's favourite flowers" ] )
                        ]
            ]
        , describe "ProgrissStore.getContextForAction"
            [ test "returns Nothing if the Action is not associated to a Context" <|
                \_ ->
                    let
                        populatedStore =
                            ProgrissStore.empty
                                |> ProgrissStore.createAction "Call Electrician"
                    in
                    Expect.equal
                        (populatedStore
                            |> ProgrissStore.getAllActions
                            |> List.map (\action -> ProgrissStore.getContextForAction action.id populatedStore)
                        )
                        [ Nothing ]
            , test "returns the context the actions is associated to" <|
                \_ ->
                    let
                        populatedStore =
                            decodeStore """
                                {
                                    "actions": [{"id": 1, "description": "Buy cat food", "context_id": 1}],
                                    "contexts": [{"id": 1, "name": "Errands"}]
                                }
                            """
                    in
                    Expect.equal
                        (populatedStore
                            |> ProgrissStore.getAllActions
                            |> List.map
                                (\action ->
                                    ProgrissStore.getContextForAction action.id populatedStore
                                        |> Maybe.map .name
                                )
                        )
                        [ Just "Errands" ]
            ]
        , describe "ProgrissStore.getProjectForAction"
            [ test "returns Nothing if the Action is not associated to a project" <|
                \_ ->
                    let
                        populatedStore =
                            ProgrissStore.empty
                                |> ProgrissStore.createAction "Call Electrician"
                    in
                    Expect.equal
                        (populatedStore
                            |> ProgrissStore.getAllActions
                            |> List.map
                                (\action ->
                                    ProgrissStore.getProjectForAction action.id populatedStore
                                        |> Maybe.map .title
                                )
                        )
                        [ Nothing ]
            , test "returns the project the action is associated to" <|
                \_ ->
                    let
                        populatedStore =
                            decodeStore """
                                {
                                    "actions": [{"id": 1, "description": "Buy cat food", "project_id": 1}],
                                    "projects": [{"id": 1, "title": "Get a cat"}]
                                }
                            """
                    in
                    Expect.equal
                        (populatedStore
                            |> ProgrissStore.getAllActions
                            |> List.map
                                (\action ->
                                    ProgrissStore.getProjectForAction action.id populatedStore
                                        |> Maybe.map .title
                                )
                        )
                        [ Just "Get a cat" ]
            ]
        , describe "ProgrissStore.createAction"
            [ test "returns a new store with the newly created action" <|
                \_ ->
                    Expect.equal
                        (ProgrissStore.empty
                            |> ProgrissStore.createAction "Call Electrician"
                            |> ProgrissStore.createAction "Call Sam"
                            |> ProgrissStore.getAllActions
                            |> List.map .description
                        )
                        [ "Call Electrician", "Call Sam" ]
            ]
        , describe "ProgrissStore.associateActionToContext"
            [ test "returns a new store with the newly associated action" <|
                \_ ->
                    let
                        populatedStore =
                            ProgrissStore.empty
                                |> ProgrissStore.createAction "Call Electrician"
                                |> ProgrissStore.createContext "Calls"

                        firstAction =
                            List.head (ProgrissStore.getAllActions populatedStore)

                        firstContext =
                            List.head (ProgrissStore.getAllContexts populatedStore)

                        updatedStore =
                            Maybe.map2
                                (\firstAction firstContext ->
                                    populatedStore
                                        |> ProgrissStore.associateActionToContext firstAction.id firstContext.id
                                        |> ProgrissStore.getActionsForContext firstContext.id
                                )
                                firstAction
                                firstContext
                    in
                    Expect.equal
                        (Maybe.map (List.map .description) updatedStore)
                        (Just [ "Call Electrician" ])
            ]
        , describe "ProgrissStore.associateActionToProject"
            [ test "returns a new store with the newly associated action" <|
                \_ ->
                    let
                        populatedStore =
                            ProgrissStore.empty
                                |> ProgrissStore.createAction "Call Electrician"
                                |> ProgrissStore.createProject "Renovate House"

                        firstAction =
                            List.head (ProgrissStore.getAllActions populatedStore)

                        firstProject =
                            List.head (ProgrissStore.getAllProjects populatedStore)

                        updatedStore =
                            Maybe.map2
                                (\firstAction firstProject ->
                                    populatedStore
                                        |> ProgrissStore.associateActionToProject firstAction.id firstProject.id
                                        |> ProgrissStore.getActionsForProject firstProject.id
                                )
                                firstAction
                                firstProject
                    in
                    Expect.equal
                        (Maybe.map (List.map .description) updatedStore)
                        (Just [ "Call Electrician" ])
            ]
        , describe "ProgrissStore.updateAction"
            [ test "returns a new store with the updated action" <|
                \_ ->
                    let
                        firstAction =
                            List.head (ProgrissStore.getAllActions fixtureStore)
                    in
                    Expect.equal
                        (Maybe.map
                            (\action ->
                                fixtureStore
                                    |> ProgrissStore.updateAction (Action action.id "Call architect about possible garden path layouts")
                                    |> ProgrissStore.getAllActions
                                    |> List.map .description
                            )
                            firstAction
                        )
                        (Just
                            [ "Call architect about possible garden path layouts"
                            , "Buy cat food"
                            , "Call Florist about Mom's favourite flowers"
                            ]
                        )
            ]
        ]


fixtureStore : ProgrissStore
fixtureStore =
    decodeStore """
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


decodeStore : String -> ProgrissStore
decodeStore jsonData =
    case Json.Decode.decodeString ProgrissStore.decoder jsonData of
        Err message ->
            Debug.crash message

        Ok store ->
            store
