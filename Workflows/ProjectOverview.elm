module Workflows.ProjectOverview exposing (Model, Msg, initialModel, update, view)

import Bootstrap.Button as Button
import Bootstrap.Card as Card
import Bootstrap.Card.Block as Block
import Bootstrap.Grid as Grid
import Bootstrap.ListGroup as ListGroup
import Html exposing (Html, i, li, text, ul)
import Html.Attributes exposing (href)
import ListHelper
import ProgrissStore as Store
    exposing
        ( ProgrissStore
        , Project
        , ProjectId
        , WorkflowSettings(..)
        , WorkflowSettingsQuery(..)
        )


type Msg
    = Never


type alias Model =
    {}


initialModel : Model
initialModel =
    {}


update : Msg -> ProgrissStore -> Model -> ( Model, ProgrissStore, Cmd Msg )
update msg store model =
    ( model, store, Cmd.none )


view : ProgrissStore -> Model -> Html Msg
view store model =
    let
        (ProjectOverviewSettingsItem workflowSettings) =
            Store.getSettingsForWorkflow ProjectOverviewSettingsQuery store

        allProjects =
            Store.getAllProjects store

        groupedProjects =
            ListHelper.groupsOf
                workflowSettings.projectsPerRow
                allProjects
                []
    in
    Grid.container []
        (List.map (\projectGroup -> Card.deck (List.map (projectCard store) projectGroup)) groupedProjects)


projectCard : ProgrissStore -> Project -> Card.Config Msg
projectCard store project =
    Card.config []
        |> Card.block [] [ Block.titleH3 [] [ text project.title ] ]
        |> Card.listGroup (projectCardActionList project.id store)
        |> Card.block []
            [ Block.text []
                [ projectCardNoteList project.id store
                , Button.button
                    [ Button.primary, Button.attrs [ Html.Attributes.class "bmd-btn-fab" ] ]
                    [ i [ Html.Attributes.class "material-icons" ] [ text "grade" ] ]
                ]
            ]


projectCardActionList : ProjectId -> ProgrissStore -> List (ListGroup.Item Msg)
projectCardActionList projectId store =
    List.map
        (\action -> ListGroup.li [] [ text action.description ])
        (Store.getActionsForProject projectId store)


projectCardNoteList : ProjectId -> ProgrissStore -> Html Msg
projectCardNoteList projectId store =
    ul [] <|
        List.map
            (\note -> li [] [ text note.body ])
            (Store.getNotesForProject projectId store)
