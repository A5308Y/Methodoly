module Workflows.Settings exposing (Model, Msg, initialModel, update, view)

import Html exposing (Html, button, div, h1, h2, text)
import Html.Attributes exposing (class, classList)
import Html.Events exposing (onClick)
import ProgrissStore as Store exposing (ProgrissStore)
import SettingsStore


type Msg
    = ChangeNumberOfProjectsPerRow Int


type alias Model =
    {}


initialModel : Model
initialModel =
    {}


update : Msg -> ProgrissStore -> Model -> ( Model, ProgrissStore, Cmd Msg )
update msg store model =
    case msg of
        ChangeNumberOfProjectsPerRow newCount ->
            let
                oldSettings =
                    SettingsStore.getSettingsForProjectOverview (Store.getAllSettings store)
            in
            ( model
            , store
                |> Store.getAllSettings
                |> SettingsStore.updateSettingsForProjectOverview { oldSettings | projectsPerRow = newCount }
                |> Store.updateSettings store
            , Cmd.none
            )


view : ProgrissStore -> Model -> Html Msg
view store model =
    let
        projectsPerRow =
            Store.getAllSettings store
                |> SettingsStore.getSettingsForProjectOverview
                |> .projectsPerRow
    in
    div [ class "container" ]
        [ h1 [] [ text "Settings" ]
        , h2 [] [ text "Project Overview" ]
        , text "Projects per row: "
        , button
            [ classList
                [ ( "btn", True )
                , ( "btn-primary", True )
                , ( "active", projectsPerRow == 1 )
                ]
            , onClick (ChangeNumberOfProjectsPerRow 1)
            ]
            [ text "1" ]
        , button
            [ classList
                [ ( "btn", True )
                , ( "btn-primary", True )
                , ( "active", projectsPerRow == 2 )
                ]
            , onClick (ChangeNumberOfProjectsPerRow 2)
            ]
            [ text "2" ]
        , button
            [ classList
                [ ( "btn", True )
                , ( "btn-primary", True )
                , ( "active", projectsPerRow == 3 )
                ]
            , onClick (ChangeNumberOfProjectsPerRow 3)
            ]
            [ text "3" ]
        , button
            [ classList
                [ ( "btn", True )
                , ( "btn-primary", True )
                , ( "active", projectsPerRow == 4 )
                ]
            , onClick (ChangeNumberOfProjectsPerRow 4)
            ]
            [ text "4" ]
        ]
