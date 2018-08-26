module Workflows.GtdActionLists exposing (Model, Msg, initialModel, navbarContent, update, view)

import Browser.Dom
import Html exposing (Html, a, button, div, form, header, hr, i, input, li, span, text, ul)
import Html.Attributes exposing (class, classList, disabled, href, id, placeholder, value)
import Html.Events exposing (custom, onClick, onInput, onSubmit)
import Json.Decode
import ProgrissStore as Store
    exposing
        ( Action
        , ActionId
        , ActionState(..)
        , Context
        , ContextId
        , ProgrissStore
        , Project
        , ProjectId
        )
import Task


type SelectedContext
    = AnywhereContext
    | SpecificContext ContextId
    | AllContexts


type EditingActionState
    = EditingAction ActionId ActionEditState
    | NotEditingAction


type ActionEditState
    = SelectingEditedAttribute
    | EditingDescription
    | EditingContext
    | EditingProject


type Msg
    = ChangeContext SelectedContext
    | UpdateNewActionDescription String
    | CreateNewAction
    | ToggleActionDone ActionId
    | ToggleContextMenu Bool
    | SelectActionToEdit EditingActionState
    | UpdateActionContextAnywhere ActionId
    | UpdateActionContext ActionId ContextId
    | UpdateActionNoProject ActionId
    | UpdateActionProject ActionId ProjectId
    | UpdateActionDescription Action String
    | SetFocusTo String
    | FocusResult (Result Browser.Dom.Error ())


type alias Model =
    { selectedContext : SelectedContext
    , newActionDescription : String
    , contextMenuVisible : Bool
    , editingAction : EditingActionState
    }


initialModel : Model
initialModel =
    { selectedContext = AllContexts
    , newActionDescription = ""
    , contextMenuVisible = False
    , editingAction = NotEditingAction
    }


update : Msg -> ProgrissStore -> Model -> ( Model, ProgrissStore, Cmd Msg )
update msg store model =
    case msg of
        UpdateNewActionDescription newActionDescription ->
            ( { model | newActionDescription = newActionDescription }, store, Cmd.none )

        ChangeContext selectedContext ->
            ( { model | selectedContext = selectedContext, contextMenuVisible = False }, store, Cmd.none )

        UpdateActionContext actionId contextId ->
            ( { model | editingAction = NotEditingAction }
            , Store.associateActionToContext actionId contextId store
            , Cmd.none
            )

        UpdateActionContextAnywhere actionId ->
            ( { model | editingAction = NotEditingAction }
            , Store.associateActionToAnywhereContext actionId store
            , Cmd.none
            )

        UpdateActionProject actionId projectId ->
            ( { model | editingAction = NotEditingAction }
            , Store.associateActionToProject actionId projectId store
            , Cmd.none
            )

        UpdateActionNoProject actionId ->
            ( { model | editingAction = NotEditingAction }
            , Store.associateActionToNoProject actionId store
            , Cmd.none
            )

        UpdateActionDescription action description ->
            let
                updatedStore =
                    Store.updateAction { action | description = description } store
            in
            ( model, updatedStore, Cmd.none )

        ToggleActionDone actionId ->
            ( { model | editingAction = NotEditingAction }, Store.toggleActionDone actionId store, Cmd.none )

        ToggleContextMenu state ->
            ( { model | contextMenuVisible = state }, store, Cmd.none )

        SelectActionToEdit editingAction ->
            case editingAction of
                EditingAction actionId actionEditState ->
                    let
                        domId =
                            "edit-" ++ Store.actionIdToString actionId
                    in
                    case actionEditState of
                        EditingDescription ->
                            ( { model | editingAction = editingAction }
                            , store
                            , Task.attempt FocusResult (Browser.Dom.focus domId)
                            )

                        _ ->
                            ( { model | editingAction = editingAction }, store, Cmd.none )

                NotEditingAction ->
                    ( { model | editingAction = editingAction }, store, Cmd.none )

        SetFocusTo domId ->
            ( model, store, Task.attempt FocusResult (Browser.Dom.focus domId) )

        FocusResult result ->
            case result of
                Err (Browser.Dom.NotFound id) ->
                    ( model, store, Cmd.none )

                Ok () ->
                    ( model, store, Cmd.none )

        CreateNewAction ->
            let
                ( actionId, storeWithNewAction ) =
                    Store.createAction model.newActionDescription store

                updatedStore =
                    case model.selectedContext of
                        SpecificContext contextId ->
                            storeWithNewAction
                                |> Store.associateActionToContext actionId contextId

                        AllContexts ->
                            storeWithNewAction

                        AnywhereContext ->
                            storeWithNewAction
            in
            ( { model | newActionDescription = "" }, updatedStore, Cmd.none )


view : ProgrissStore -> Model -> Html Msg
view store model =
    div []
        [ actionEditMenu model.editingAction store
        , contextMenu store model.selectedContext model.contextMenuVisible
        , actionContainer store model
        ]


actionContainer : ProgrissStore -> Model -> Html Msg
actionContainer store model =
    div [ class "container" ]
        [ renderActions store model.editingAction (actionsToRender store model.selectedContext)
        , renderNewActionFormCard model
        ]


renderNewActionFormCard : Model -> Html Msg
renderNewActionFormCard model =
    div [ class "card bg-light" ]
        [ div [ class "card-body" ]
            [ div [ class "row align-middle" ]
                [ div [ class "col-2 col-md-1" ]
                    [ button
                        [ class "btn btn-success bmd-btn-fab bmd-btn-fab-sm"
                        , onClick (SetFocusTo "new-action-description")
                        ]
                        [ i [ class "material-icons" ] [ text "add" ] ]
                    ]
                , div [ class "col" ] [ newActionForm model ]
                ]
            ]
        ]


newActionForm : Model -> Html Msg
newActionForm model =
    form
        [ onSubmit CreateNewAction ]
        [ div [ class "input-group" ]
            [ input
                [ placeholder "Add an Action"
                , class "form-control"
                , value model.newActionDescription
                , onInput UpdateNewActionDescription
                , id "new-action-description"
                ]
                []
            , div [ class "input-group-append", disabled (String.isEmpty model.newActionDescription) ]
                [ button [ class "btn btn-success" ] [ text "Create Action" ] ]
            ]
        ]


contextMenu : ProgrissStore -> SelectedContext -> Bool -> Html Msg
contextMenu store selectedContext contextMenuVisible =
    let
        selectableItems =
            store
                |> Store.getAllContexts
                |> List.map (clickableContext selectedContext)
                |> (::) (clickableAnywhereContext selectedContext)
                |> (::) (clickableAllContexts selectedContext)
    in
    if contextMenuVisible then
        visibleSideMenu "Switch Context" selectableItems

    else
        hiddenSideMenu


clickableAllContexts : SelectedContext -> Html Msg
clickableAllContexts selectedContext =
    a
        [ href "#"
        , classList [ ( "list-group-item", True ), ( "active", selectedContext == AllContexts ) ]
        , onClick (ChangeContext AllContexts)
        ]
        [ text "All" ]


clickableAnywhereContext : SelectedContext -> Html Msg
clickableAnywhereContext selectedContext =
    a
        [ href "#"
        , classList [ ( "list-group-item", True ), ( "active", selectedContext == AnywhereContext ) ]
        , onClick (ChangeContext AnywhereContext)
        ]
        [ text "Anywhere" ]


clickableContext : SelectedContext -> Context -> Html Msg
clickableContext selectedContext context =
    a
        [ href "#"
        , classList [ ( "list-group-item", True ), ( "active", selectedContext == SpecificContext context.id ) ]
        , onClick (ChangeContext (SpecificContext context.id))
        ]
        [ text context.name ]


actionsToRender : ProgrissStore -> SelectedContext -> List Action
actionsToRender store selectedContext =
    case selectedContext of
        AllContexts ->
            Store.getAllActions store

        AnywhereContext ->
            Store.getActionsWithoutContext store

        SpecificContext contextId ->
            Store.getActionsForContext contextId store


renderActions : ProgrissStore -> EditingActionState -> List Action -> Html Msg
renderActions store editingAction actions =
    div [] (List.map (actionCard store editingAction) actions)


actionCard : ProgrissStore -> EditingActionState -> Action -> Html Msg
actionCard store editingAction action =
    div [ class (cardClassForAction action) ]
        [ actionCardBlock editingAction action
        , actionCardFooter store editingAction action
        ]


actionCardBlock : EditingActionState -> Action -> Html Msg
actionCardBlock editingAction action =
    let
        cardClickEvent =
            if editingAction == EditingAction action.id SelectingEditedAttribute then
                [ onClick (SelectActionToEdit NotEditingAction) ]

            else if editingAction == EditingAction action.id EditingDescription then
                []

            else
                [ onClick (SelectActionToEdit (EditingAction action.id SelectingEditedAttribute)) ]
    in
    div ([ class "card-body" ] ++ cardClickEvent)
        [ div [ class "row align-items-center" ]
            [ div [ class "col-2 col-md-1" ] [ toggleDoneButton action ]
            , if editingAction == EditingAction action.id EditingDescription then
                div [ class "col" ] [ editActionForm action ]

              else
                div [ class "col" ] [ text action.description ]
            ]
        ]


toggleDoneButton action =
    button
        [ class ("btn bmd-btn-fab bmd-btn-fab-sm " ++ buttonColorForActionState action.state)
        , custom "click"
            (Json.Decode.succeed
                { message = ToggleActionDone action.id
                , preventDefault = True
                , stopPropagation = True
                }
            )
        ]
        [ i [ class "material-icons" ]
            [ text (iconForActionState action.state) ]
        ]


editActionForm action =
    div [ class "col" ]
        [ form
            [ class "edit-action-form"
            , onSubmit (SelectActionToEdit NotEditingAction)
            ]
            [ div [ class "input-group" ]
                [ input
                    [ class "form-control form-control-sm"
                    , onInput (UpdateActionDescription action)
                    , value action.description
                    , id ("edit-" ++ Store.actionIdToString action.id)
                    ]
                    []
                , div [ class "input-group-append", disabled (String.isEmpty action.description) ]
                    [ button [ class "btn btn-success" ] [ text "Save" ] ]
                ]
            ]
        ]


actionCardFooter : ProgrissStore -> EditingActionState -> Action -> Html Msg
actionCardFooter store editingAction action =
    let
        footerClass =
            if
                (editingAction == EditingAction action.id SelectingEditedAttribute)
                    || (editingAction == EditingAction action.id EditingContext)
                    || (editingAction == EditingAction action.id EditingProject)
            then
                "card-body visible-footer"

            else
                "card-body hidden-footer"
    in
    div [ class footerClass ]
        [ div [ class "btn-group btn-group-sm footer-button-group" ]
            [ button
                [ onClick (SelectActionToEdit (EditingAction action.id EditingDescription))
                , class "btn btn-primary footer-button"
                ]
                [ text "Edit" ]
            , button
                [ onClick (SelectActionToEdit (EditingAction action.id EditingContext))
                , class "btn btn-primary footer-button"
                ]
                [ text (contextName store action) ]
            , button
                [ onClick (SelectActionToEdit (EditingAction action.id EditingProject))
                , class "btn btn-primary footer-button"
                ]
                [ text (projectName store action) ]
            ]
        ]


contextName : ProgrissStore -> Action -> String
contextName store action =
    case Store.getContextForAction action.id store of
        Nothing ->
            "Anywhere"

        Just context ->
            context.name


projectName : ProgrissStore -> Action -> String
projectName store action =
    case Store.getProjectForAction action.id store of
        Nothing ->
            "No Project"

        Just project ->
            project.title


buttonColorForActionState : ActionState -> String
buttonColorForActionState state =
    case state of
        Done time ->
            "btn-success"

        _ ->
            "btn-primary"


iconForActionState : ActionState -> String
iconForActionState state =
    case state of
        Done time ->
            "done"

        _ ->
            "check_box_outline_blank"


cardClassForAction : Action -> String
cardClassForAction action =
    case action.state of
        Done time ->
            "card bg-light"

        _ ->
            "card"


navbarContent : ProgrissStore -> Model -> Html Msg
navbarContent store model =
    Html.button
        [ class "btn btn-outline-primary"
        , onClick (ToggleContextMenu (not model.contextMenuVisible))
        ]
        [ text (selectedContextName store model.selectedContext) ]


selectedContextName : ProgrissStore -> SelectedContext -> String
selectedContextName store selectedContext =
    case selectedContext of
        AllContexts ->
            "All"

        AnywhereContext ->
            "Anywhere"

        SpecificContext contextId ->
            Store.getContext contextId store
                |> Maybe.map .name
                |> Maybe.withDefault ""


actionEditMenu : EditingActionState -> ProgrissStore -> Html Msg
actionEditMenu editingAction store =
    case editingAction of
        EditingAction actionId actionEditState ->
            case actionEditState of
                EditingContext ->
                    let
                        selectableItems =
                            [ a
                                [ href "#"
                                , classList
                                    [ ( "list-group-item", True )
                                    , ( "active"
                                      , Store.getContextForAction actionId store == Nothing
                                      )
                                    ]
                                , onClick (UpdateActionContextAnywhere actionId)
                                ]
                                [ text "Anywhere" ]
                            ]
                                ++ List.map (contextSelectLink store actionId) (Store.getAllContexts store)
                    in
                    visibleSideMenu "Change Context" selectableItems

                EditingProject ->
                    let
                        selectableItems =
                            [ a
                                [ href "#"
                                , classList
                                    [ ( "list-group-item", True )
                                    , ( "active"
                                      , Store.getProjectForAction actionId store == Nothing
                                      )
                                    ]
                                , onClick (UpdateActionNoProject actionId)
                                ]
                                [ text "No Project" ]
                            ]
                                ++ List.map (projectSelectLink store actionId) (Store.getAllProjects store)
                    in
                    visibleSideMenu "Change Project" selectableItems

                _ ->
                    hiddenSideMenu

        _ ->
            hiddenSideMenu


visibleSideMenu : String -> List (Html Msg) -> Html Msg
visibleSideMenu title selectableItems =
    div
        [ classList
            [ ( "bmd-layout-container", True )
            , ( "bmd-drawer-f-r", True )
            , ( "bmd-drawer-overlay", True )
            , ( "bmd-drawer-in", True )
            , ( "side-menu", True )
            ]
        ]
        [ div
            [ classList [ ( "bmd-layout-drawer", True ), ( "bg-faded", True ) ] ]
            [ header [] [ a [ class "navbar-brand" ] [ text title ] ]
            , ul [ class "list-group" ] selectableItems
            ]
        ]


hiddenSideMenu : Html Msg
hiddenSideMenu =
    div
        [ classList
            [ ( "bmd-layout-container", True )
            , ( "bmd-drawer-f-r", True )
            , ( "bmd-drawer-overlay", True )
            , ( "bmd-drawer-in", False )
            , ( "side-menu", True )
            ]
        ]
        [ div
            [ classList [ ( "bmd-layout-drawer", True ), ( "bg-faded", True ) ] ]
            []
        ]


contextSelectLink : ProgrissStore -> ActionId -> Context -> Html Msg
contextSelectLink store actionId context =
    a
        [ href "#"
        , classList
            [ ( "list-group-item", True )
            , ( "active", Store.getContextForAction actionId store == Just context )
            ]
        , onClick (UpdateActionContext actionId context.id)
        ]
        [ text context.name ]


projectSelectLink : ProgrissStore -> ActionId -> Project -> Html Msg
projectSelectLink store actionId project =
    a
        [ href "#"
        , classList
            [ ( "list-group-item", True )
            , ( "active", Store.getProjectForAction actionId store == Just project )
            ]
        , onClick (UpdateActionProject actionId project.id)
        ]
        [ text project.title ]
