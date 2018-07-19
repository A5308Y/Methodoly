module ProgrissStore
    exposing
        ( Action
        , ActionId
        , ActionState(..)
        , Context
        , ContextId
        , ProgrissStore
        , Project
        , ProjectId
        , associateActionToContext
        , associateActionToProject
        , createAction
        , createContext
        , createProject
        , decoder
        , empty
        , encoder
        , getActionsForContext
        , getActionsForProject
        , getActionsWithoutContext
        , getActionsWithoutProject
        , getAllActions
        , getAllContexts
        , getAllProjects
        , getAllSettings
        , getContext
        , getContextForAction
        , getNotesForProject
        , getProjectForAction
        , toggleActionDone
        , updateAction
        , updateSettings
        )

import Dict exposing (Dict)
import Json.Decode exposing (Decoder, float, int, nullable, string)
import Json.Decode.Pipeline exposing (decode, hardcoded, optional, required)
import Json.Encode
import SettingsStore exposing (SettingsStore)
import Time exposing (Time)


type ProgrissStore
    = ProgrissStore Store


type alias Store =
    { projects : Dict Int ProjectData
    , actions : Dict Int ActionData
    , contexts : Dict Int ContextData
    , notes : Dict Int NoteData
    , settings : SettingsStore
    }


type alias ActionData =
    { description : String
    , contextId : Maybe Int
    , projectId : Maybe Int
    , state : ActionState
    }


type alias ProjectData =
    { title : String }


type alias ContextData =
    { name : String }


type alias NoteData =
    { body : String, projectId : Int }



{--Don't add the relationships to these records, because we might switch a graph implementation. Use
 the store to find a project or context for an action. --}


type alias Action =
    { id : ActionId, description : String, state : ActionState }


type alias Project =
    { id : ProjectId, title : String }


type alias Context =
    { id : ContextId, name : String }


type alias Note =
    { id : NoteId, body : String }


type ActionId
    = ActionId Int


type ActionState
    = Active
    | Done Time
    | Deleted Time


type ContextId
    = ContextId Int


type ProjectId
    = ProjectId Int


type NoteId
    = NoteId Int


empty : ProgrissStore
empty =
    ProgrissStore
        { projects = Dict.empty
        , actions = Dict.empty
        , contexts = Dict.empty
        , notes = Dict.empty
        , settings = SettingsStore.initialStore
        }


createAction : String -> ProgrissStore -> ( ActionId, ProgrissStore )
createAction description (ProgrissStore store) =
    let
        updatedActions =
            Dict.insert
                (getNextFreeId store.actions)
                (ActionData description Nothing Nothing Active)
                store.actions
    in
    ( ActionId (getNextFreeId store.actions)
    , ProgrissStore { store | actions = updatedActions }
    )


createContext : String -> ProgrissStore -> ProgrissStore
createContext name (ProgrissStore store) =
    let
        updatedContexts =
            Dict.insert (getNextFreeId store.contexts) (ContextData name) store.contexts
    in
    ProgrissStore { store | contexts = updatedContexts }


createProject : String -> ProgrissStore -> ProgrissStore
createProject name (ProgrissStore store) =
    let
        updatedProjects =
            Dict.insert (getNextFreeId store.projects) (ProjectData name) store.projects
    in
    ProgrissStore { store | projects = updatedProjects }


getNextFreeId : Dict Int a -> Int
getNextFreeId dictionary =
    dictionary
        |> Dict.keys
        |> List.maximum
        |> Maybe.withDefault 0
        |> (+) 1


updateAction : Action -> ProgrissStore -> ProgrissStore
updateAction action (ProgrissStore store) =
    case action.id of
        ActionId actionId ->
            let
                updatedActions =
                    Dict.update
                        actionId
                        (Maybe.map
                            (\actionData ->
                                { actionData
                                    | description = action.description
                                    , state = action.state
                                }
                            )
                        )
                        store.actions
            in
            ProgrissStore { store | actions = updatedActions }


toggleActionDone : ActionId -> ProgrissStore -> ProgrissStore
toggleActionDone (ActionId actionId) (ProgrissStore store) =
    let
        updatedActions =
            Dict.update
                actionId
                (Maybe.map actionsStateAfterToggle)
                store.actions
    in
    ProgrissStore { store | actions = updatedActions }


actionsStateAfterToggle : ActionData -> ActionData
actionsStateAfterToggle actionData =
    let
        updatedState =
            case actionData.state of
                Done time ->
                    Active

                Active ->
                    Done 0

                Deleted time ->
                    Deleted time
    in
    { actionData | state = updatedState }


associateActionToContext : ActionId -> ContextId -> ProgrissStore -> ProgrissStore
associateActionToContext (ActionId actionId) (ContextId contextId) (ProgrissStore store) =
    let
        updatedActions =
            Dict.update
                actionId
                (Maybe.map (\actionData -> { actionData | contextId = Just contextId }))
                store.actions
    in
    ProgrissStore { store | actions = updatedActions }


associateActionToProject : ActionId -> ProjectId -> ProgrissStore -> ProgrissStore
associateActionToProject (ActionId actionId) (ProjectId projectId) (ProgrissStore store) =
    let
        updatedActions =
            Dict.update
                actionId
                (Maybe.map (\actionData -> { actionData | projectId = Just projectId }))
                store.actions
    in
    ProgrissStore { store | actions = updatedActions }


updateSettings : ProgrissStore -> SettingsStore -> ProgrissStore
updateSettings (ProgrissStore store) settings =
    ProgrissStore { store | settings = settings }



--Retrieving data from the store


getAllActions : ProgrissStore -> List Action
getAllActions (ProgrissStore store) =
    store.actions
        |> Dict.toList
        |> List.map castActionDataToAction


getActionsWithoutContext : ProgrissStore -> List Action
getActionsWithoutContext (ProgrissStore store) =
    store.actions
        |> Dict.filter (\id actionData -> actionData.contextId == Nothing)
        |> Dict.toList
        |> List.map castActionDataToAction


getActionsWithoutProject : ProgrissStore -> List Action
getActionsWithoutProject (ProgrissStore store) =
    store.actions
        |> Dict.filter (\id actionData -> actionData.projectId == Nothing)
        |> Dict.toList
        |> List.map castActionDataToAction


getActionsForContext : ContextId -> ProgrissStore -> List Action
getActionsForContext (ContextId contextId) (ProgrissStore store) =
    Dict.toList store.actions
        |> List.filter (\( id, actionData ) -> actionData.contextId == Just contextId)
        |> List.map castActionDataToAction


getContext : ContextId -> ProgrissStore -> Maybe Context
getContext (ContextId contextId) (ProgrissStore store) =
    Dict.get contextId store.contexts
        |> Maybe.map (\contextData -> ( contextId, contextData ))
        |> Maybe.map castContextDataToContext


getActionsForProject : ProjectId -> ProgrissStore -> List Action
getActionsForProject (ProjectId projectId) (ProgrissStore store) =
    Dict.toList store.actions
        |> List.filter (\( id, actionData ) -> actionData.projectId == Just projectId)
        |> List.map castActionDataToAction


getNotesForProject : ProjectId -> ProgrissStore -> List Note
getNotesForProject (ProjectId projectId) (ProgrissStore store) =
    Dict.toList store.notes
        |> List.filter (\( id, noteData ) -> noteData.projectId == projectId)
        |> List.map castNoteDataToNote


getContextForAction : ActionId -> ProgrissStore -> Maybe Context
getContextForAction (ActionId actionId) (ProgrissStore store) =
    Dict.get actionId store.actions
        |> Maybe.andThen (\action -> action.contextId)
        |> Maybe.andThen (\contextId -> Maybe.map (\context -> ( contextId, context )) (Dict.get contextId store.contexts))
        |> Maybe.map castContextDataToContext


getProjectForAction : ActionId -> ProgrissStore -> Maybe Project
getProjectForAction (ActionId actionId) (ProgrissStore store) =
    Dict.get actionId store.actions
        |> Maybe.andThen (\action -> action.projectId)
        |> Maybe.andThen (\projectId -> Maybe.map (\project -> ( projectId, project )) (Dict.get projectId store.projects))
        |> Maybe.map castProjectDataToProject


getAllContexts : ProgrissStore -> List Context
getAllContexts (ProgrissStore store) =
    Dict.toList store.contexts
        |> List.map castContextDataToContext


getAllProjects : ProgrissStore -> List Project
getAllProjects (ProgrissStore store) =
    Dict.toList store.projects
        |> List.map castProjectDataToProject


getAllSettings : ProgrissStore -> SettingsStore
getAllSettings (ProgrissStore store) =
    store.settings


castActionDataToAction : ( Int, ActionData ) -> Action
castActionDataToAction ( id, actionData ) =
    Action (ActionId id) actionData.description actionData.state


castProjectDataToProject : ( Int, ProjectData ) -> Project
castProjectDataToProject ( id, projectData ) =
    Project (ProjectId id) projectData.title


castContextDataToContext : ( Int, ContextData ) -> Context
castContextDataToContext ( id, contextData ) =
    Context (ContextId id) contextData.name


castNoteDataToNote : ( Int, NoteData ) -> Note
castNoteDataToNote ( id, noteData ) =
    Note (NoteId id) noteData.body



-- Decoding


decoder : Decoder ProgrissStore
decoder =
    decode progrissStoreConstructor
        |> optional "actions" (Json.Decode.list actionDecoder) []
        |> optional "contexts" (Json.Decode.list contextDecoder) []
        |> optional "projects" (Json.Decode.list projectDecoder) []
        |> optional "notes" (Json.Decode.list noteDecoder) []


progrissStoreConstructor : List ( Int, ActionData ) -> List ( Int, ContextData ) -> List ( Int, ProjectData ) -> List ( Int, NoteData ) -> ProgrissStore
progrissStoreConstructor actions contexts projects notes =
    ProgrissStore
        { actions = Dict.fromList actions
        , contexts = Dict.fromList contexts
        , projects = Dict.fromList projects
        , notes = Dict.fromList notes
        , settings = SettingsStore.initialStore
        }


actionDecoder : Decoder ( Int, ActionData )
actionDecoder =
    decode actionDataConstructor
        |> required "id" int
        |> required "description" string
        |> optional "context_id" (nullable int) Nothing
        |> optional "project_id" (nullable int) Nothing
        |> optional "finished_at" (nullable float) Nothing
        |> optional "deleted_at" (nullable float) Nothing


noteDecoder : Decoder ( Int, NoteData )
noteDecoder =
    decode noteDataConstructor
        |> required "id" int
        |> required "body" string
        |> required "project_id" int


projectDecoder : Decoder ( Int, ProjectData )
projectDecoder =
    decode projectDataConstructor
        |> required "id" int
        |> required "title" string


contextDecoder : Decoder ( Int, ContextData )
contextDecoder =
    decode contextDataConstructor
        |> required "id" int
        |> required "name" string


actionDataConstructor : Int -> String -> Maybe Int -> Maybe Int -> Maybe Time -> Maybe Time -> ( Int, ActionData )
actionDataConstructor id description maybeContextId maybeProjectId finishedAt deletedAt =
    let
        state =
            case deletedAt of
                Nothing ->
                    case finishedAt of
                        Nothing ->
                            Active

                        Just time ->
                            Done time

                Just time ->
                    Deleted time
    in
    ( id, ActionData description maybeContextId maybeProjectId state )


contextDataConstructor : Int -> String -> ( Int, ContextData )
contextDataConstructor id name =
    ( id, ContextData name )


projectDataConstructor : Int -> String -> ( Int, ProjectData )
projectDataConstructor id title =
    ( id, ProjectData title )


noteDataConstructor : Int -> String -> Int -> ( Int, NoteData )
noteDataConstructor id content projectId =
    ( id, NoteData content projectId )



-- Encoding


encoder : ProgrissStore -> Json.Decode.Value
encoder (ProgrissStore store) =
    Json.Encode.object
        [ ( "actions", Json.Encode.list (List.map encodeAction (store.actions |> Dict.toList)) )
        , ( "contexts", Json.Encode.list (List.map encodeContext (store.contexts |> Dict.toList)) )
        , ( "projects", Json.Encode.list (List.map encodeProject (store.projects |> Dict.toList)) )
        , ( "notes", Json.Encode.list (List.map encodeNote (store.notes |> Dict.toList)) )
        ]


encodeAction : ( Int, ActionData ) -> Json.Decode.Value
encodeAction ( id, actionData ) =
    Json.Encode.object
        [ ( "id", Json.Encode.int id )
        , ( "description", Json.Encode.string actionData.description )
        , ( "context_id"
          , case actionData.contextId of
                Nothing ->
                    Json.Encode.null

                Just contextId ->
                    Json.Encode.int contextId
          )
        , ( "project_id"
          , case actionData.projectId of
                Nothing ->
                    Json.Encode.null

                Just projectId ->
                    Json.Encode.int projectId
          )
        , ( "deleted_at"
          , case actionData.state of
                Deleted time ->
                    Json.Encode.float time

                _ ->
                    Json.Encode.null
          )
        , ( "finished_at"
          , case actionData.state of
                Done time ->
                    Json.Encode.float time

                _ ->
                    Json.Encode.null
          )
        ]


encodeContext : ( Int, ContextData ) -> Json.Decode.Value
encodeContext ( id, contextData ) =
    Json.Encode.object
        [ ( "id", Json.Encode.int id )
        , ( "name", Json.Encode.string contextData.name )
        ]


encodeProject : ( Int, ProjectData ) -> Json.Decode.Value
encodeProject ( id, projectData ) =
    Json.Encode.object
        [ ( "id", Json.Encode.int id )
        , ( "title", Json.Encode.string projectData.title )
        ]


encodeNote : ( Int, NoteData ) -> Json.Decode.Value
encodeNote ( id, noteData ) =
    Json.Encode.object
        [ ( "id", Json.Encode.int id )
        , ( "body", Json.Encode.string noteData.body )
        , ( "project_id", Json.Encode.int noteData.projectId )
        ]
