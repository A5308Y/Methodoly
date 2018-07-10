module ProgrissStore exposing (..)

import Dict exposing (Dict)
import Json.Decode exposing (Decoder, float, int, nullable, string)
import Json.Decode.Pipeline exposing (decode, hardcoded, optional, required)


type ProgrissStore
    = ProgrissStore Store


type ActionFilter
    = AllActions
    | ProjectActionsFilter Int
    | ContextActionsFilter Int


type NoteFilter
    = AllNotes
    | ProjectNotesFilter Int


type alias Store =
    { projects : Dict Int Project
    , actions : Dict Int Action
    , contexts : Dict Int Context
    , notes : Dict Int Note
    }


type alias Action =
    { description : String, contextId : Maybe Int, projectId : Maybe Int }


type alias Project =
    { title : String }


type alias Context =
    { name : String }


type alias Note =
    { body : String, projectId : Int }


empty : ProgrissStore
empty =
    ProgrissStore
        { projects = Dict.empty
        , actions = Dict.empty
        , contexts = Dict.empty
        , notes = Dict.empty
        }


getAllActions : ProgrissStore -> List ( Int, Action )
getAllActions (ProgrissStore store) =
    Dict.toList store.actions


getAllProjects : ProgrissStore -> List ( Int, Project )
getAllProjects (ProgrissStore store) =
    Dict.toList store.projects


decoder : Decoder ProgrissStore
decoder =
    decode progrissStoreConstructor
        |> optional "actions" (Json.Decode.list actionDecoder) []
        |> optional "contexts" (Json.Decode.list contextDecoder) []
        |> optional "projects" (Json.Decode.list projectDecoder) []
        |> optional "notes" (Json.Decode.list noteDecoder) []


progrissStoreConstructor : List ( Int, Action ) -> List ( Int, Context ) -> List ( Int, Project ) -> List ( Int, Note ) -> ProgrissStore
progrissStoreConstructor actions contexts projects notes =
    ProgrissStore
        { actions = Dict.fromList actions
        , contexts = Dict.fromList contexts
        , projects = Dict.fromList projects
        , notes = Dict.fromList notes
        }


actionDecoder : Decoder ( Int, Action )
actionDecoder =
    decode actionDataConstructor
        |> required "id" int
        |> required "description" string
        |> optional "context_id" (nullable int) Nothing
        |> optional "project_id" (nullable int) Nothing


noteDecoder : Decoder ( Int, Note )
noteDecoder =
    decode noteDataConstructor
        |> required "id" int
        |> required "content" string
        |> required "project_id" int


projectDecoder : Decoder ( Int, Project )
projectDecoder =
    decode projectDataConstructor
        |> required "id" int
        |> required "title" string


contextDecoder : Decoder ( Int, Context )
contextDecoder =
    decode contextDataConstructor
        |> required "id" int
        |> required "name" string


actionDataConstructor : Int -> String -> Maybe Int -> Maybe Int -> ( Int, Action )
actionDataConstructor id description maybeContextId maybeProjectId =
    ( id, Action description maybeContextId maybeProjectId )


contextDataConstructor : Int -> String -> ( Int, Context )
contextDataConstructor id name =
    ( id, Context name )


projectDataConstructor : Int -> String -> ( Int, Project )
projectDataConstructor id title =
    ( id, Project title )


noteDataConstructor : Int -> String -> Int -> ( Int, Note )
noteDataConstructor id content projectId =
    ( id, Note content projectId )



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
