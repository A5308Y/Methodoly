module ProgrissStore
    exposing
        ( Action
        , ActionId(ActionId)
        , Context
        , ContextId(ContextId)
        , ProgrissStore
        , Project
        , ProjectId(ProjectId)
        , decoder
        , getActionsForContext
        , getAllActions
        , getAllContexts
        , getAllProjects
        , getContextForAction
        , getProjectForAction
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


getAllActions : ProgrissStore -> List Action
getAllActions (ProgrissStore store) =
    Dict.toList store.actions
        |> List.map castActionDataToAction


getActionsForContext : ContextId -> ProgrissStore -> List Action
getActionsForContext (ContextId contextId) (ProgrissStore store) =
    Dict.toList store.actions
        |> List.filter (\( id, actionData ) -> actionData.contextId == Just contextId)
        |> List.map castActionDataToAction


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
