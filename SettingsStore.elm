module SettingsStore exposing
    ( SettingsStore
    , getSettingsForProjectOverview
    , initialStore
    , updateSettingsForProjectOverview
    )


type SettingsStore
    = SettingsStore Store


type alias Store =
    { projectOverviewSettings : ProjectOverviewSettings }


type alias ProjectOverviewSettings =
    { projectsPerRow : Int }


updateSettingsForProjectOverview : ProjectOverviewSettings -> SettingsStore -> SettingsStore
updateSettingsForProjectOverview projectOverviewSettings (SettingsStore store) =
    SettingsStore { store | projectOverviewSettings = projectOverviewSettings }


getSettingsForProjectOverview : SettingsStore -> ProjectOverviewSettings
getSettingsForProjectOverview (SettingsStore store) =
    store.projectOverviewSettings


initialStore : SettingsStore
initialStore =
    SettingsStore { projectOverviewSettings = { projectsPerRow = 3 } }
