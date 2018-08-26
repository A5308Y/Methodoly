module Workflows.ProjectOverview exposing (Model, Msg, initialModel, update, view)

import Html exposing (Html, button, div, h5, i, li, text, ul)
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
    div [ class "container" ] (List.map (projectCardDeck store) (projectDecks store))


projectDecks store =
    let
        projectsPerRow =
            Store.getAllSettings store
                |> SettingsStore.getSettingsForProjectOverview
                |> .projectsPerRow
    in
    ListHelper.groupsOf projectsPerRow (Store.getAllProjects store) []
        |> List.reverse


projectCardDeck : ProgrissStore -> List Project -> Html Msg
projectCardDeck store projectGroup =
    div [ class "m-4" ]
        [ div [ class "card-deck" ] (List.map (projectCard store) projectGroup) ]


projectCard : ProgrissStore -> Project -> Html Msg
projectCard store project =
    div [ class "card" ]
        [ div [ class "card-body" ] [ h5 [ class "card-title" ] [ text project.title ] ]
        , ul [ class "list-group list-group-flush" ] (projectCardActionList project.id store)
        , div [ class "card-body" ]
            [ projectCardNoteList project.id store
            , button
                [ class "btn btn-primary bmd-btn-fab" ]
                [ i [ class "material-icons" ] [ text "grade" ] ]
            ]
        ]


projectCardActionList : ProjectId -> ProgrissStore -> List (Html Msg)
projectCardActionList projectId store =
    List.map
        (\action -> li [ class "list-group-item" ] [ text action.description ])
        (Store.getActionsForProject projectId store)


projectCardNoteList : ProjectId -> ProgrissStore -> Html Msg
projectCardNoteList projectId store =
    ul [] <|
        List.map
            (\note -> li [] [ text note.body ])
            (Store.getNotesForProject projectId store)
