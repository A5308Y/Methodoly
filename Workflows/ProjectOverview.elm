module Workflows.ProjectOverview exposing (Model, Msg, initialModel, update, view)

import Bootstrap.Button as Button
import Bootstrap.Card as Card
import Bootstrap.Card.Block as Block
import Bootstrap.Grid as Grid
import Bootstrap.ListGroup as ListGroup
import Bootstrap.Utilities.Spacing as Spacing
import Html exposing (Html, div, i, li, text, ul)
import Html.Attributes exposing (class, href)
import ListHelper
import ProgrissStore as Store exposing (ProgrissStore, Project, ProjectId)
import SettingsStore


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
        projectsPerRow =
            Store.getAllSettings store
                |> SettingsStore.getSettingsForProjectOverview
                |> .projectsPerRow
    in
    ListHelper.groupsOf projectsPerRow (Store.getAllProjects store) []
        |> List.reverse
        |> List.map (projectCardColumn store)
        |> Grid.container []


projectCardColumn : ProgrissStore -> List Project -> Html Msg
projectCardColumn store projectGroup =
    div [ Spacing.m4 ]
        [ projectGroup
            |> List.map (projectCard store)
            |> Card.deck
        ]


projectCard : ProgrissStore -> Project -> Card.Config Msg
projectCard store project =
    Card.config []
        |> Card.block [] [ Block.titleH3 [] [ text project.title ] ]
        |> Card.listGroup (projectCardActionList project.id store)
        |> Card.block []
            [ Block.text []
                [ projectCardNoteList project.id store
                , Button.button
                    [ Button.primary, Button.attrs [ class "bmd-btn-fab" ] ]
                    [ i [ class "material-icons" ] [ text "grade" ] ]
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
