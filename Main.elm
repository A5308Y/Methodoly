module Main exposing (..)

import Bootstrap.CDN as CDN
import Bootstrap.Card as Card
import Bootstrap.Card.Block as Block
import Bootstrap.Grid as Grid
import Bootstrap.ListGroup as ListGroup
import Html exposing (..)
import Html.Events exposing (..)
import ListHelper
import ProgrissStore as Store exposing (Action, Context, ContextId, ProgrissStore, Project, ProjectId)


type alias Model =
    { store : ProgrissStore
    , selectedContext : SelectedContext
    }


type SelectedContext
    = AnywhereContext
    | SpecificContext ContextId
    | AllContexts


type Msg
    = ChangeContext SelectedContext


initialModel : Model
initialModel =
    { store =
        Store.empty
            |> Store.createContext "Errands"
            |> Store.createContext "Office"
            |> Store.createProject "Build a house"
            |> Store.createProject "Buy a car"
            |> Store.createAction "Buy cat food"
    , selectedContext = AnywhereContext
    }


main : Program Never Model Msg
main =
    Html.beginnerProgram { model = initialModel, view = view, update = update }


update : Msg -> Model -> Model
update msg model =
    case msg of
        ChangeContext selectedContext ->
            { model | selectedContext = selectedContext }


view : Model -> Html Msg
view model =
    Grid.container []
        [ CDN.stylesheet
        , Grid.row []
            [ Grid.col [] [ renderContextMenu model.store model.selectedContext ]
            , Grid.col [] [ renderActions (actionsToRender model.store model.selectedContext) ]
            ]
        , hr [] []
        , projectsCardOverview model.store
        ]


renderContextMenu : ProgrissStore -> SelectedContext -> Html Msg
renderContextMenu store selectedContext =
    ListGroup.ul
        (store
            |> Store.getAllContexts
            |> List.map (clickableContext selectedContext)
            |> (::) (clickableAnywhereContext selectedContext)
            |> (::) (clickableAllContexts selectedContext)
        )


clickableAllContexts : SelectedContext -> ListGroup.Item Msg
clickableAllContexts selectedContext =
    ListGroup.li (clickableContextAttributes selectedContext AllContexts) [ text "All" ]


clickableAnywhereContext : SelectedContext -> ListGroup.Item Msg
clickableAnywhereContext selectedContext =
    ListGroup.li (clickableContextAttributes selectedContext AnywhereContext) [ text "Anywhere" ]


clickableContext : SelectedContext -> Context -> ListGroup.Item Msg
clickableContext selectedContext context =
    ListGroup.li
        (clickableContextAttributes selectedContext (SpecificContext context.id))
        [ text context.name ]


clickableContextAttributes : SelectedContext -> SelectedContext -> List (ListGroup.ItemOption Msg)
clickableContextAttributes selectedContext context =
    if selectedContext == context then
        [ ListGroup.active, ListGroup.attrs [ onClick (ChangeContext context) ] ]
    else
        [ ListGroup.attrs [ onClick (ChangeContext context) ] ]


actionsToRender : ProgrissStore -> SelectedContext -> List Action
actionsToRender store selectedContext =
    case selectedContext of
        AllContexts ->
            Store.getAllActions store

        AnywhereContext ->
            Store.getAllActionsWithoutContext store

        SpecificContext contextId ->
            Store.getActionsForContext contextId store


renderActions : List Action -> Html msg
renderActions actions =
    ul [] (List.map (\action -> li [] [ text action.description ]) actions)


getActions : Maybe ContextId -> ProgrissStore -> List Action
getActions selectedContext store =
    case selectedContext of
        Nothing ->
            Store.getAllActions store

        Just contextId ->
            Store.getActionsForContext contextId store


projectsCardOverview : ProgrissStore -> Html Msg
projectsCardOverview store =
    let
        allProjects =
            Store.getAllProjects store

        groupedProjects =
            ListHelper.groupsOf projectsPerRow allProjects []
    in
    div [] (List.map (\projectGroup -> Card.deck (List.map (projectCard store) projectGroup)) groupedProjects)


projectCard : ProgrissStore -> Project -> Card.Config Msg
projectCard store project =
    Card.config []
        |> Card.block [] [ Block.titleH3 [] [ text project.title ] ]
        |> Card.listGroup (projectCardActionList project.id store)



--|> Card.block [] [ Block.text [] [ projectCardNoteList project.id store ] ]


projectCardActionList : ProjectId -> ProgrissStore -> List (ListGroup.Item Msg)
projectCardActionList projectId store =
    List.map
        (\action -> ListGroup.li [] [ text action.description ])
        (Store.getActionsForProject projectId store)



--projectCardNoteList : Int -> ProgrissStore -> Html Msg
--projectCardNoteList projectId store =
--    ul []
--        (List.map
--            (\note -> li [] [ text note.content ])
--            (Store.getNotesFor (ProjectFilter projectId) store)
--        )


projectsPerRow : Int
projectsPerRow =
    3



-- Click on project card to see project in full width
-- Tests for interactive elements
-- Creating contexts in the interface
-- Creating actions in the interface
-- Creating projects in the interface
-- Proper interface that shows everything in the graph
-- DoneState = Done Int (Completed At)| Active | SomedayMaybe Int (Resubmit At) | Deleted Int (Deleted At) for Projects
-- DoneState = Done Int (Completed At)| Active | Deleted Int (Deleted At) for Actions
-- Store the DoneState in the record. I thought about calculating the state from stored Events int he Graph, but that doesn't seem a good idea, because the data structure allows me to store two DoneAt events for a single Action. What would this mean?
-- Benchmark to see if the store is fast
