module Workflows.Settings exposing (Model, Msg, initialModel, update, view)

import Bootstrap.Button as Button
import Bootstrap.Grid as Grid
import Html exposing (Html, h1, text)
import Html.Attributes exposing (classList)
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
    Grid.container []
        [ h1 [] [ text "projectOverviewSettings" ]
        , text ("projects per row: " ++ toString projectsPerRow)
        , Button.button
            [ Button.primary
            , Button.attrs
                [ onClick (ChangeNumberOfProjectsPerRow 1)
                , classList [ ( "active", projectsPerRow == 1 ) ]
                ]
            ]
            [ text "1" ]
        , Button.button
            [ Button.primary
            , Button.attrs
                [ onClick (ChangeNumberOfProjectsPerRow 2)
                , classList [ ( "active", projectsPerRow == 2 ) ]
                ]
            ]
            [ text "2" ]
        , Button.button
            [ Button.primary
            , Button.attrs
                [ onClick (ChangeNumberOfProjectsPerRow 3)
                , classList [ ( "active", projectsPerRow == 3 ) ]
                ]
            ]
            [ text "3" ]
        , Button.button
            [ Button.primary
            , Button.attrs
                [ onClick (ChangeNumberOfProjectsPerRow 4)
                , classList [ ( "active", projectsPerRow == 4 ) ]
                ]
            ]
            [ text "4" ]
        ]
