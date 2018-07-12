module Workflows.SimpleTodos exposing (Model, Msg, initialModel, update, view)

import Bootstrap.Button as Button
import Bootstrap.Card as Card
import Bootstrap.Card.Block as Block
import Bootstrap.Form as Form
import Bootstrap.Form.Input as Input
import Bootstrap.Form.InputGroup as InputGroup
import Bootstrap.Grid as Grid
import Bootstrap.ListGroup as ListGroup
import Html exposing (..)
import Html.Attributes exposing (href)
import Html.Events exposing (..)
import ProgrissStore as Store exposing (Action, ProgrissStore, decoder)


type Msg
    = UpdateNewActionDescription String
    | CreateNewAction


type alias Model =
    { newActionDescription : String
    }


initialModel : Model
initialModel =
    { newActionDescription = ""
    }


update : Msg -> ProgrissStore -> Model -> ( Model, ProgrissStore, Cmd Msg )
update msg store model =
    case msg of
        UpdateNewActionDescription newActionDescription ->
            ( { model | newActionDescription = newActionDescription }, store, Cmd.none )

        CreateNewAction ->
            let
                ( actionId, updatedStore ) =
                    Store.createAction model.newActionDescription store
            in
            ( { model | newActionDescription = "" }, updatedStore, Cmd.none )


view : ProgrissStore -> Model -> Html Msg
view store model =
    Grid.container []
        [ Grid.row []
            [ Grid.col []
                [ renderActions (Store.getAllActions store)
                , hr [] []
                , Form.form []
                    [ InputGroup.config
                        (InputGroup.text
                            [ Input.placeholder "Type action description here"
                            , Input.attrs
                                [ Html.Attributes.value model.newActionDescription
                                , Html.Events.onInput UpdateNewActionDescription
                                ]
                            ]
                        )
                        |> InputGroup.successors
                            [ InputGroup.button
                                [ Button.secondary, Button.attrs [ onClick CreateNewAction ] ]
                                [ text "Create Action" ]
                            ]
                        |> InputGroup.view
                    ]
                ]
            ]
        ]


renderActions : List Action -> Html Msg
renderActions actions =
    div [] (List.map (\action -> actionCard action) actions)


actionCard : Action -> Html Msg
actionCard action =
    Card.config []
        |> Card.block [] [ Block.text [] [ text action.description ] ]
        |> Card.view
