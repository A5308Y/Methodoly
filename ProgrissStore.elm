module ProgrissStore
    exposing
        ( Action
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
        , getActionsForContext
        , getActionsForProject
        , getActionsWithoutContext
        , getActionsWithoutProject
        , getAllActions
        , getAllContexts
        , getAllProjects
        , getContextForAction
        , getNotesForProject
        , getProjectForAction
        , updateAction
        )

import Dict exposing (Dict)
import Json.Decode exposing (Decoder, float, int, nullable, string)
import Json.Decode.Pipeline exposing (decode, hardcoded, optional, required)


type ProgrissStore
    = ProgrissStore Store


type alias Store =
    { projects : Dict Int ProjectData
    , actions : Dict Int ActionData
    , contexts : Dict Int ContextData
    , notes : Dict Int NoteData
    }


type alias ActionData =
    { description : String, contextId : Maybe Int, projectId : Maybe Int }


type alias ProjectData =
    { title : String }


type alias ContextData =
    { name : String }


type alias NoteData =
    { body : String, projectId : Int }



{--Don't add the relationships to these records, because we might switch a graph implementation. Use
 the store to find a project or context for an action. --}


type alias Action =
    { id : ActionId, description : String }


type alias Project =
    { id : ProjectId, title : String }


type alias Context =
    { id : ContextId, name : String }


type alias Note =
    { id : NoteId, body : String }


type ActionId
    = ActionId Int


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
        }


createAction : String -> ProgrissStore -> ProgrissStore
createAction description (ProgrissStore store) =
    let
        updatedActions =
            Dict.insert
                (getNextFreeId store.actions)
                (ActionData description Nothing Nothing)
                store.actions
    in
    ProgrissStore { store | actions = updatedActions }


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
                        (Maybe.map (\actionData -> { actionData | description = action.description }))
                        store.actions
            in
            ProgrissStore { store | actions = updatedActions }


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


castActionDataToAction : ( Int, ActionData ) -> Action
castActionDataToAction ( id, actionData ) =
    Action (ActionId id) actionData.description


castProjectDataToProject : ( Int, ProjectData ) -> Project
castProjectDataToProject ( id, projectData ) =
    Project (ProjectId id) projectData.title


castContextDataToContext : ( Int, ContextData ) -> Context
castContextDataToContext ( id, contextData ) =
    Context (ContextId id) contextData.name


castNoteDataToNote : ( Int, NoteData ) -> Note
castNoteDataToNote ( id, noteData ) =
    Note (NoteId id) noteData.body


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
        }


actionDecoder : Decoder ( Int, ActionData )
actionDecoder =
    decode actionDataConstructor
        |> required "id" int
        |> required "description" string
        |> optional "context_id" (nullable int) Nothing
        |> optional "project_id" (nullable int) Nothing


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


actionDataConstructor : Int -> String -> Maybe Int -> Maybe Int -> ( Int, ActionData )
actionDataConstructor id description maybeContextId maybeProjectId =
    ( id, ActionData description maybeContextId maybeProjectId )


contextDataConstructor : Int -> String -> ( Int, ContextData )
contextDataConstructor id name =
    ( id, ContextData name )


projectDataConstructor : Int -> String -> ( Int, ProjectData )
projectDataConstructor id title =
    ( id, ProjectData title )


noteDataConstructor : Int -> String -> Int -> ( Int, NoteData )
noteDataConstructor id content projectId =
    ( id, NoteData content projectId )
