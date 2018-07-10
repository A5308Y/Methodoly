module ProgrissStore exposing (..)

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


type alias Action =
    { id : Int, description : String }


type alias Project =
    { id : Int, title : String }


type alias Context =
    { id : Int, name : String }


type alias Note =
    { id : Int, body : String }


empty : ProgrissStore
empty =
    ProgrissStore
        { projects = Dict.empty
        , actions = Dict.empty
        , contexts = Dict.empty
        , notes = Dict.empty
        }


getAllActions : ProgrissStore -> List Action
getAllActions (ProgrissStore store) =
    Dict.toList store.actions
        |> List.map castActionDataToAction


getActionsForContext : Int -> ProgrissStore -> List Action
getActionsForContext contextId (ProgrissStore store) =
    Dict.toList store.actions
        |> List.filter (\( id, actionData ) -> actionData.contextId == Just contextId)
        |> List.map castActionDataToAction


getContextForAction : Int -> ProgrissStore -> Maybe Context
getContextForAction actionId (ProgrissStore store) =
    Dict.get actionId store.actions
        |> Maybe.andThen (\action -> action.contextId)
        |> Maybe.andThen (\contextId -> Maybe.map (\context -> ( contextId, context )) (Dict.get contextId store.contexts))
        |> Maybe.map castContextDataToContext


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
    Action id actionData.description


castProjectDataToProject : ( Int, ProjectData ) -> Project
castProjectDataToProject ( id, projectData ) =
    Project id projectData.title


castContextDataToContext : ( Int, ContextData ) -> Context
castContextDataToContext ( id, contextData ) =
    Context id contextData.name


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
        |> required "content" string
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



--        ProjectActionsFilter projectId ->
--            store.actions
--                |> Dict.filter (\id action -> action.projectId == Just projectId)
--                |> Dict.toList
--        ContextActionsFilter contextId ->
--            store.actions
--                |> Dict.filter (\id action -> action.contextId == Just contextId)
--                |> Dict.toList
--getNotes : NoteFilter -> ProgrissStore -> List ( Int, Note )
--getNotes noteFilter (ProgrissStore store) =
--    case noteFilter of
--        AllNotes ->
--            Dict.toList store.notes
--        ProjectNotesFilter projectId ->
--            store.notes
--                |> Dict.filter (\id note -> note.projectId == projectId)
--                |> Dict.toList
--getContexts : ProgrissStore -> List ( Int, Context )
--getContexts (ProgrissStore store) =
--    Dict.toList store.contexts
--getProjects : ProgrissStore -> List ( Int, Project )
--getProjects (ProgrissStore store) =
--    Dict.toList store.projects
