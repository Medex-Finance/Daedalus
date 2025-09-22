port module Main exposing (main)

import Browser
import Browser.Navigation as Nav
import Html exposing (Html, a, button, div, form, h1, h2, h3, h4, input, label, li, main_, nav, p, pre, section, span, strong, textarea, text, ul)
import Html.Attributes exposing (class, disabled, href, min, placeholder, rel, target, type_, value)
import Html.Events exposing (onClick, onInput, onSubmit)
import Http
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode
import String
import Url exposing (Url)


-- PORTS ---------------------------------------------------------------------

port openStatusStream : { taskId : Int, path : String } -> Cmd msg

port closeStatusStream : Int -> Cmd msg

port receiveStatusEvent : (Decode.Value -> msg) -> Sub msg


-- MAIN ----------------------------------------------------------------------

type alias Flags =
    { backendBase : String }


main : Program Flags Model Msg
main =
    Browser.application
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        , onUrlRequest = LinkClicked
        , onUrlChange = UrlChanged
        }


-- MODEL ---------------------------------------------------------------------

normalizeBase : String -> String
normalizeBase raw =
    let
        trimmed = String.trim raw
    in
    if trimmed == "" then
        ""

    else
        removeTrailingSlashes trimmed


removeTrailingSlashes : String -> String
removeTrailingSlashes str =
    if String.endsWith "/" str && String.length str > 0 then
        removeTrailingSlashes (String.dropRight 1 str)

    else
        str


type alias Model =
    { key : Nav.Key
    , backendBase : String
    , apiBase : String
    , page : Page
    , error : Maybe String
    , activeStream : Maybe Int
    }


type Page
    = PageLoading Route
    | DashboardPage DashboardModel
    | TaskDetailPage TaskDetailModel
    | SettingsPage SettingsModel
    | NotFound


type alias DashboardModel =
    { tasks : RemoteData (List TaskSummary)
    , snapshot : RemoteData OrchestratorSnapshot
    , form : CreateTaskForm
    , submitting : Bool
    , submitError : Maybe String
    }


type alias CreateTaskForm =
    { title : String
    , description : String
    , repoRoot : String
    , branch : String
    }


type alias TaskDetailModel =
    { id : Int
    , detail : RemoteData TaskDetail
    , events : List StatusEvent
    , message : String
    , messageState : RequestState
    , statusState : RequestState
    , pingState : RequestState
    , cancelState : RequestState
    }


type alias SettingsModel =
    { settings : RemoteData Settings
    , draft : SettingsDraft
    , saving : RequestState
    , prompts : RemoteData (List PromptTemplate)
    , promptEdits : PromptEdits
    }


type alias SettingsDraft =
    { repoRoot : String
    , branch : String
    , testCommand : String
    , previewCommand : String
    , inactivityMinutes : String
    }


type alias PromptEdits =
    List PromptEdit


type alias PromptEdit =
    { key : String
    , content : String
    , description : String
    , version : Int
    , status : RequestState
    }


type RequestState
    = Idle
    | Working
    | Failed String
    | Completed


type RemoteData a
    = NotAsked
    | Loading
    | Success a
    | Failure String


remoteDataMap : (a -> b) -> RemoteData a -> RemoteData b
remoteDataMap fn data =
    case data of
        Success value ->
            Success (fn value)

        Failure err ->
            Failure err

        Loading ->
            Loading

        NotAsked ->
            NotAsked


-- ROUTING -------------------------------------------------------------------

type Route
    = RouteDashboard
    | RouteTask Int
    | RouteSettings
    | RouteUnknown


parseRoute : Url -> Route
parseRoute url =
    let
        segments =
            url.path
                |> String.split "/"
                |> List.filter ((/=) "")
    in
    case segments of
        [] ->
            RouteDashboard

        [ "settings" ] ->
            RouteSettings

        [ "task", taskIdStr ] ->
            case String.toInt taskIdStr of
                Just taskId ->
                    RouteTask taskId

                Nothing ->
                    RouteUnknown

        _ ->
            RouteUnknown


-- DATA TYPES ----------------------------------------------------------------

type TaskStatus
    = TaskStatusPending
    | TaskStatusDesigning
    | TaskStatusImplementing
    | TaskStatusReviewing
    | TaskStatusQa
    | TaskStatusBlocked
    | TaskStatusCompleted
    | TaskStatusDiscarded
    | TaskStatusUnknown String


taskStatusToString : TaskStatus -> String
taskStatusToString status =
    case status of
        TaskStatusPending ->
            "TaskStatusPending"

        TaskStatusDesigning ->
            "TaskStatusDesigning"

        TaskStatusImplementing ->
            "TaskStatusImplementing"

        TaskStatusReviewing ->
            "TaskStatusReviewing"

        TaskStatusQa ->
            "TaskStatusQa"

        TaskStatusBlocked ->
            "TaskStatusBlocked"

        TaskStatusCompleted ->
            "TaskStatusCompleted"

        TaskStatusDiscarded ->
            "TaskStatusDiscarded"

        TaskStatusUnknown raw ->
            raw


taskStatusFromString : String -> TaskStatus
taskStatusFromString raw =
    case raw of
        "TaskStatusPending" ->
            TaskStatusPending

        "TaskStatusDesigning" ->
            TaskStatusDesigning

        "TaskStatusImplementing" ->
            TaskStatusImplementing

        "TaskStatusReviewing" ->
            TaskStatusReviewing

        "TaskStatusQa" ->
            TaskStatusQa

        "TaskStatusBlocked" ->
            TaskStatusBlocked

        "TaskStatusCompleted" ->
            TaskStatusCompleted

        "TaskStatusDiscarded" ->
            TaskStatusDiscarded

        _ ->
            TaskStatusUnknown raw


type WorkflowStep
    = StepIntake
    | StepDesign
    | StepImplementation
    | StepPmReview
    | StepQaReview
    | StepFixIteration
    | StepCommit
    | StepPreview
    | StepFinalize
    | StepUnknown String


workflowStepFromString : String -> WorkflowStep
workflowStepFromString raw =
    case raw of
        "StepIntake" ->
            StepIntake

        "StepDesign" ->
            StepDesign

        "StepImplementation" ->
            StepImplementation

        "StepPmReview" ->
            StepPmReview

        "StepQaReview" ->
            StepQaReview

        "StepFixIteration" ->
            StepFixIteration

        "StepCommit" ->
            StepCommit

        "StepPreview" ->
            StepPreview

        "StepFinalize" ->
            StepFinalize

        _ ->
            StepUnknown raw


workflowStepToString : WorkflowStep -> String
workflowStepToString step =
    case step of
        StepIntake ->
            "StepIntake"

        StepDesign ->
            "StepDesign"

        StepImplementation ->
            "StepImplementation"

        StepPmReview ->
            "StepPmReview"

        StepQaReview ->
            "StepQaReview"

        StepFixIteration ->
            "StepFixIteration"

        StepCommit ->
            "StepCommit"

        StepPreview ->
            "StepPreview"

        StepFinalize ->
            "StepFinalize"

        StepUnknown raw ->
            raw


type PreviewStatus
    = PreviewOffline
    | PreviewLaunching
    | PreviewOnline
    | PreviewFailedStatus
    | PreviewStatusUnknown String


previewStatusFromString : String -> PreviewStatus
previewStatusFromString raw =
    case raw of
        "PreviewOffline" ->
            PreviewOffline

        "PreviewLaunching" ->
            PreviewLaunching

        "PreviewOnline" ->
            PreviewOnline

        "PreviewFailed" ->
            PreviewFailedStatus

        _ ->
            PreviewStatusUnknown raw


previewStatusToString : PreviewStatus -> String
previewStatusToString status =
    case status of
        PreviewOffline ->
            "PreviewOffline"

        PreviewLaunching ->
            "PreviewLaunching"

        PreviewOnline ->
            "PreviewOnline"

        PreviewFailedStatus ->
            "PreviewFailed"

        PreviewStatusUnknown raw ->
            raw


type ArtifactKind
    = ArtifactDesign
    | ArtifactDiff
    | ArtifactTestLog
    | ArtifactCommitLog
    | ArtifactPreviewLog
    | ArtifactPreviewPing
    | ArtifactAgentTranscript
    | ArtifactUnknown String


artifactKindFromString : String -> ArtifactKind
artifactKindFromString raw =
    case raw of
        "ArtifactDesign" ->
            ArtifactDesign

        "ArtifactDiff" ->
            ArtifactDiff

        "ArtifactTestLog" ->
            ArtifactTestLog

        "ArtifactCommitLog" ->
            ArtifactCommitLog

        "ArtifactPreviewLog" ->
            ArtifactPreviewLog

        "ArtifactPreviewPing" ->
            ArtifactPreviewPing

        "ArtifactAgentTranscript" ->
            ArtifactAgentTranscript

        _ ->
            ArtifactUnknown raw


-- API TYPES -----------------------------------------------------------------

type alias TaskSummary =
    { id : Int
    , title : String
    , status : TaskStatus
    , repoRoot : String
    , branch : String
    , featureBranch : Maybe String
    , updatedAt : String
    , previewUrl : Maybe String
    , previewStatus : PreviewStatus
    }


type alias TaskRun =
    { ordinal : Int
    , step : WorkflowStep
    , summary : Maybe String
    , createdAt : String
    , updatedAt : String
    }


type alias StatusEvent =
    { step : WorkflowStep
    , message : String
    , createdAt : String
    , payload : Maybe Decode.Value
    }


type alias Artifact =
    { kind : ArtifactKind
    , label : String
    , body : Maybe Decode.Value
    , path : Maybe String
    , createdAt : String
    }


type alias TaskDetail =
    { summary : TaskSummary
    , runs : List TaskRun
    , events : List StatusEvent
    , artifacts : List Artifact
    }


type alias OrchestratorSnapshot =
    { activeTasks : List TaskSummary
    , queueDepth : Int
    }


type alias Settings =
    { repoRoot : String
    , branch : String
    , testCommand : String
    , previewCommand : Maybe String
    , inactivityMinutes : Int
    , updatedAt : String
    }


type alias PromptTemplate =
    { key : String
    , description : String
    , content : String
    , version : Int
    , updatedAt : String
    , isCustom : Bool
    }


type alias PreviewPingResponse =
    { status : PreviewStatus
    }

-- DECODERS ------------------------------------------------------------------

maybeStringDecoder : Decoder (Maybe String)
maybeStringDecoder =
    Decode.oneOf [ Decode.null Nothing, Decode.map Just Decode.string ]


taskStatusDecoder : Decoder TaskStatus
taskStatusDecoder =
    Decode.string |> Decode.map taskStatusFromString


workflowStepDecoder : Decoder WorkflowStep
workflowStepDecoder =
    Decode.string |> Decode.map workflowStepFromString


previewStatusDecoder : Decoder PreviewStatus
previewStatusDecoder =
    Decode.string |> Decode.map previewStatusFromString


artifactKindDecoder : Decoder ArtifactKind
artifactKindDecoder =
    Decode.string |> Decode.map artifactKindFromString


taskSummaryDecoder : Decoder TaskSummary
taskSummaryDecoder =
    Decode.succeed TaskSummary
        |> andMap (Decode.field "taskSummaryId" Decode.int)
        |> andMap (Decode.field "taskSummaryTitle" Decode.string)
        |> andMap (Decode.field "taskSummaryStatus" taskStatusDecoder)
        |> andMap (Decode.field "taskSummaryRepoRoot" Decode.string)
        |> andMap (Decode.field "taskSummaryBranch" Decode.string)
        |> andMap (Decode.field "taskSummaryFeatureBranch" maybeStringDecoder)
        |> andMap (Decode.field "taskSummaryUpdatedAt" Decode.string)
        |> andMap (Decode.field "taskSummaryPreviewUrl" maybeStringDecoder)
        |> andMap (Decode.field "taskSummaryPreviewStatus" previewStatusDecoder)


runDecoder : Decoder TaskRun
runDecoder =
    Decode.map5 TaskRun
        (Decode.field "taskRunOrdinal" Decode.int)
        (Decode.field "taskRunCurrentStep" workflowStepDecoder)
        (Decode.field "taskRunPmSummary" maybeStringDecoder)
        (Decode.field "taskRunCreatedAt" Decode.string)
        (Decode.field "taskRunUpdatedAt" Decode.string)


statusEventDecoder : Decoder StatusEvent
statusEventDecoder =
    Decode.map4 StatusEvent
        (Decode.field "statusEventStep" workflowStepDecoder)
        (Decode.field "statusEventMessage" Decode.string)
        (Decode.field "statusEventCreatedAt" Decode.string)
        (Decode.field "statusEventPayload" (Decode.nullable Decode.value))


artifactDecoder : Decoder Artifact
artifactDecoder =
    Decode.map5 Artifact
        (Decode.field "artifactKind" artifactKindDecoder)
        (Decode.field "artifactLabel" Decode.string)
        (Decode.field "artifactBody" (Decode.nullable Decode.value))
        (Decode.field "artifactPath" maybeStringDecoder)
        (Decode.field "artifactCreatedAt" Decode.string)


taskDetailDecoder : Decoder TaskDetail
taskDetailDecoder =
    Decode.map4 TaskDetail
        (Decode.field "taskDetailSummary" taskSummaryDecoder)
        (Decode.field "taskDetailRuns" (Decode.list runDecoder))
        (Decode.field "taskDetailEvents" (Decode.list statusEventDecoder))
        (Decode.field "taskDetailArtifacts" (Decode.list artifactDecoder))


snapshotDecoder : Decoder OrchestratorSnapshot
snapshotDecoder =
    Decode.map2 OrchestratorSnapshot
        (Decode.field "snapshotActiveTasks" (Decode.list taskSummaryDecoder))
        (Decode.field "snapshotQueueDepth" Decode.int)


settingsDecoder : Decoder Settings
settingsDecoder =
    Decode.map6 Settings
        (Decode.field "settingsDefaultRepoRoot" Decode.string)
        (Decode.field "settingsDefaultBranch" Decode.string)
        (Decode.field "settingsTestCommand" Decode.string)
        (Decode.field "settingsPreviewCommand" maybeStringDecoder)
        (Decode.field "settingsInactivityMinutes" Decode.int)
        (Decode.field "settingsUpdatedAt" Decode.string)


promptTemplateDecoder : Decoder PromptTemplate
promptTemplateDecoder =
    Decode.map6 PromptTemplate
        (Decode.field "promptTemplateKey" Decode.string)
        (Decode.field "promptTemplateDescription" Decode.string)
        (Decode.field "promptTemplateContent" Decode.string)
        (Decode.field "promptTemplateVersion" Decode.int)
        (Decode.field "promptTemplateUpdatedAt" Decode.string)
        (Decode.field "promptTemplateIsCustom" Decode.bool)


previewPingDecoder : Decoder PreviewPingResponse
previewPingDecoder =
    Decode.map PreviewPingResponse
        (Decode.field "status" previewStatusDecoder)


andMap : Decoder a -> Decoder (a -> b) -> Decoder b
andMap valueDecoder funcDecoder =
    Decode.map2 (\value func -> func value) valueDecoder funcDecoder


type alias SsePayload =
    { taskId : Int
    , event : StatusEvent
    }


ssePayloadDecoder : Decoder SsePayload
ssePayloadDecoder =
    Decode.map2 SsePayload
        (Decode.field "taskId" Decode.int)
        (Decode.field "event" statusEventDecoder)


-- INIT ----------------------------------------------------------------------

init : Flags -> Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url key =
    let
        base = normalizeBase flags.backendBase
        apiBase =
            if base == "" then
                "/api"

            else
                base ++ "/api"

        initialModel =
            { key = key
            , backendBase = base
            , apiBase = apiBase
            , page = PageLoading RouteDashboard
            , error = Nothing
            , activeStream = Nothing
            }
    in
    changeRoute (parseRoute url) initialModel


-- MESSAGES ------------------------------------------------------------------

type Msg
    = LinkClicked Browser.UrlRequest
    | UrlChanged Url
    | GotTaskList (Result Http.Error (List TaskSummary))
    | GotSnapshot (Result Http.Error OrchestratorSnapshot)
    | UpdateCreateTitle String
    | UpdateCreateDescription String
    | UpdateCreateRepo String
    | UpdateCreateBranch String
    | SubmitCreateTask
    | CreatedTask (Result Http.Error TaskSummary)
    | GotTaskDetail Int (Result Http.Error TaskDetail)
    | UpdateMessageInput String
    | SubmitMessage
    | MessageSent (Result Http.Error ())
    | UpdateTaskStatus TaskStatus
    | TaskStatusUpdated (Result Http.Error TaskDetail)
    | TriggerPreviewPing
    | PreviewPinged (Result Http.Error PreviewPingResponse)
    | CancelTask
    | TaskCancelled (Result Http.Error TaskDetail)
    | ReceiveStatusEvent Decode.Value
    | RefreshTaskDetail
    | GotSettings (Result Http.Error Settings)
    | UpdateSettingsRepo String
    | UpdateSettingsBranch String
    | UpdateSettingsTestCommand String
    | UpdateSettingsPreviewCommand String
    | UpdateSettingsInactivity String
    | SubmitSettings
    | SettingsSaved (Result Http.Error Settings)
    | GotPrompts (Result Http.Error (List PromptTemplate))
    | UpdatePromptContent String String
    | UpdatePromptDescription String String
    | SavePrompt String
    | PromptSaved String (Result Http.Error PromptTemplate)
    | ResetPrompt String
    | PromptReset String (Result Http.Error PromptTemplate)
    | DismissError


-- UPDATE --------------------------------------------------------------------

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        LinkClicked urlRequest ->
            case urlRequest of
                Browser.Internal url ->
                    ( model, Nav.pushUrl model.key (Url.toString url) )

                Browser.External href ->
                    ( model, Nav.load href )

        UrlChanged url ->
            changeRoute (parseRoute url) model

        DismissError ->
            ( { model | error = Nothing }, Cmd.none )

        GotTaskList result ->
            case model.page of
                DashboardPage dash ->
                    let
                        updatedDash =
                            { dash
                                | tasks = result |> resultToRemote
                            }
                    in
                    ( { model | page = DashboardPage updatedDash }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        GotSnapshot result ->
            case model.page of
                DashboardPage dash ->
                    let
                        updated =
                            { dash | snapshot = result |> resultToRemote }
                    in
                    ( { model | page = DashboardPage updated }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        UpdateCreateTitle str ->
            updateDashboardForm model (
                \form dash ->
                    { dash | form = { form | title = str } }
            )

        UpdateCreateDescription str ->
            updateDashboardForm model (
                \form dash ->
                    { dash | form = { form | description = str } }
            )

        UpdateCreateRepo str ->
            updateDashboardForm model (
                \form dash ->
                    { dash | form = { form | repoRoot = str } }
            )

        UpdateCreateBranch str ->
            updateDashboardForm model (
                \form dash ->
                    { dash | form = { form | branch = str } }
            )

        SubmitCreateTask ->
            case model.page of
                DashboardPage dash ->
                    if dash.submitting || String.trim dash.form.title == "" || String.trim dash.form.description == "" then
                        ( model, Cmd.none )

                    else
                        let
                            cmd =
                                createTask model dash.form
                        in
                        ( { model
                            | page =
                                DashboardPage
                                    { dash
                                        | submitting = True
                                        , submitError = Nothing
                                    }
                          }
                        , cmd
                        )

                _ ->
                    ( model, Cmd.none )

        CreatedTask result ->
            case model.page of
                DashboardPage dash ->
                    case result of
                        Ok summary ->
                            let
                                updatedTasks =
                                    case dash.tasks of
                                        Success items ->
                                            Success (summary :: items)

                                        _ ->
                                            dash.tasks

                                resetForm =
                                    { title = "", description = "", repoRoot = "", branch = "" }

                                newDash =
                                    { dash
                                        | submitting = False
                                        , submitError = Nothing
                                        , tasks = updatedTasks
                                        , form = resetForm
                                    }
                            in
                            ( { model | page = DashboardPage newDash }, Cmd.none )

                        Err err ->
                            let
                                newDash =
                                    { dash
                                        | submitting = False
                                        , submitError = Just (httpErrorToString err)
                                    }
                            in
                            ( { model | page = DashboardPage newDash }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        GotTaskDetail taskId result ->
            case model.page of
                TaskDetailPage detailModel ->
                    if detailModel.id /= taskId then
                        ( model, Cmd.none )

                    else
                        case result of
                            Ok detail ->
                                let
                                    newModel =
                                        { detailModel
                                            | detail = Success detail
                                            , events = detail.events
                                        }
                                in
                                ( { model | page = TaskDetailPage newModel }, Cmd.none )

                            Err err ->
                                let
                                    newDetail =
                                        { detailModel
                                            | detail = Failure (httpErrorToString err)
                                        }
                                in
                                ( { model | page = TaskDetailPage newDetail }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        RefreshTaskDetail ->
            case model.page of
                TaskDetailPage detailModel ->
                    ( model, fetchTaskDetail model detailModel.id )

                _ ->
                    ( model, Cmd.none )

        UpdateMessageInput str ->
            case model.page of
                TaskDetailPage detailModel ->
                    let
                        nextState =
                            case detailModel.messageState of
                                Working ->
                                    Working

                                _ ->
                                    Idle

                        updated =
                            { detailModel | message = str, messageState = nextState }
                    in
                    ( { model | page = TaskDetailPage updated }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        SubmitMessage ->
            case model.page of
                TaskDetailPage detailModel ->
                    if String.trim detailModel.message == "" || detailModel.messageState == Working then
                        ( model, Cmd.none )

                    else
                        ( { model | page = TaskDetailPage { detailModel | messageState = Working } }
                        , sendTaskMessage model detailModel.id detailModel.message
                        )

                _ ->
                    ( model, Cmd.none )

        MessageSent result ->
            case model.page of
                TaskDetailPage detailModel ->
                    case result of
                        Ok _ ->
                            ( { model | page = TaskDetailPage { detailModel | message = "", messageState = Completed } }, Cmd.none )

                        Err err ->
                            ( { model | page = TaskDetailPage { detailModel | messageState = Failed (httpErrorToString err) } }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        UpdateTaskStatus status ->
            case model.page of
                TaskDetailPage detailModel ->
                    ( { model | page = TaskDetailPage { detailModel | statusState = Working } }
                    , updateTaskStatus model detailModel.id status
                    )

                _ ->
                    ( model, Cmd.none )

        TaskStatusUpdated result ->
            case model.page of
                TaskDetailPage detailModel ->
                    case result of
                        Ok detail ->
                            ( { model
                                | page =
                                    TaskDetailPage
                                        { detailModel
                                            | detail = Success detail
                                            , statusState = Completed
                                        }
                              }
                            , Cmd.none
                            )

                        Err err ->
                            ( { model | page = TaskDetailPage { detailModel | statusState = Failed (httpErrorToString err) } }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        TriggerPreviewPing ->
            case model.page of
                TaskDetailPage detailModel ->
                    ( { model | page = TaskDetailPage { detailModel | pingState = Working } }
                    , pingPreview model detailModel.id
                    )

                _ ->
                    ( model, Cmd.none )

        PreviewPinged result ->
            case model.page of
                TaskDetailPage detailModel ->
                    case ( result, detailModel.detail ) of
                        ( Ok resp, Success detail ) ->
                            let
                                summary = detail.summary

                                updatedSummary =
                                    { summary | previewStatus = resp.status }

                                newDetail =
                                    { detail | summary = updatedSummary }
                            in
                            ( { model
                                | page =
                                    TaskDetailPage
                                        { detailModel
                                            | detail = Success newDetail
                                            , pingState = Completed
                                        }
                              }
                            , Cmd.none
                            )

                        ( Err err, _ ) ->
                            ( { model | page = TaskDetailPage { detailModel | pingState = Failed (httpErrorToString err) } }, Cmd.none )

                        _ ->
                            ( { model | page = TaskDetailPage { detailModel | pingState = Completed } }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        CancelTask ->
            case model.page of
                TaskDetailPage detailModel ->
                    if detailModel.cancelState == Working then
                        ( model, Cmd.none )

                    else
                        ( { model | page = TaskDetailPage { detailModel | cancelState = Working } }
                        , cancelTaskRequest model detailModel.id
                        )

                _ ->
                    ( model, Cmd.none )

        TaskCancelled result ->
            case model.page of
                TaskDetailPage detailModel ->
                    case result of
                        Ok detail ->
                            ( { model
                                | page =
                                    TaskDetailPage
                                        { detailModel
                                            | detail = Success detail
                                            , events = detail.events
                                            , cancelState = Completed
                                        }
                              }
                            , Cmd.none
                            )

                        Err err ->
                            ( { model | page = TaskDetailPage { detailModel | cancelState = Failed (httpErrorToString err) } }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        ReceiveStatusEvent value ->
            case Decode.decodeValue ssePayloadDecoder value of
                Ok payload ->
                    case model.page of
                        TaskDetailPage detailModel ->
                            if payload.taskId /= detailModel.id then
                                ( model, Cmd.none )

                            else
                                let
                                    updatedDetail =
                                        remoteDataMap (
                                            \d -> { d | events = payload.event :: d.events }
                                        ) detailModel.detail

                                    updatedModel =
                                        { detailModel
                                            | events = payload.event :: detailModel.events
                                            , detail = updatedDetail
                                        }
                                in
                                ( { model | page = TaskDetailPage updatedModel }, Cmd.none )

                        _ ->
                            ( model, Cmd.none )

                Err _ ->
                    ( model, Cmd.none )

        GotSettings result ->
            case model.page of
                SettingsPage settingsModel ->
                    case result of
                        Ok settings ->
                            let
                                draft =
                                    { repoRoot = settings.repoRoot
                                    , branch = settings.branch
                                    , testCommand = settings.testCommand
                                    , previewCommand = Maybe.withDefault "" settings.previewCommand
                                    , inactivityMinutes = String.fromInt settings.inactivityMinutes
                                    }

                                updated =
                                    { settingsModel
                                        | settings = Success settings
                                        , draft = draft
                                    }
                            in
                            ( { model | page = SettingsPage updated }, Cmd.none )

                        Err err ->
                            ( { model | page = SettingsPage { settingsModel | settings = Failure (httpErrorToString err) } }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        UpdateSettingsRepo str ->
            modifySettingsDraft model (
                \draft settingsModel ->
                    { settingsModel | draft = { draft | repoRoot = str } }
            )

        UpdateSettingsBranch str ->
            modifySettingsDraft model (
                \draft settingsModel ->
                    { settingsModel | draft = { draft | branch = str } }
            )

        UpdateSettingsTestCommand str ->
            modifySettingsDraft model (
                \draft settingsModel ->
                    { settingsModel | draft = { draft | testCommand = str } }
            )

        UpdateSettingsPreviewCommand str ->
            modifySettingsDraft model (
                \draft settingsModel ->
                    { settingsModel | draft = { draft | previewCommand = str } }
            )

        UpdateSettingsInactivity str ->
            modifySettingsDraft model (
                \draft settingsModel ->
                    { settingsModel | draft = { draft | inactivityMinutes = str } }
            )

        SubmitSettings ->
            case model.page of
                SettingsPage settingsModel ->
                    if settingsModel.saving == Working then
                        ( model, Cmd.none )

                    else
                        let
                            trimmed = String.trim settingsModel.draft.inactivityMinutes
                            parsed =
                                case String.toInt trimmed of
                                    Just value ->
                                        if value > 0 then
                                            Just value

                                        else
                                            Nothing

                                    Nothing ->
                                        Nothing
                        in
                        case parsed of
                            Nothing ->
                                ( { model
                                    | page =
                                        SettingsPage
                                            { settingsModel
                                                | saving = Failed "Inactivity timeout must be a positive integer"
                                            }
                                  }
                                , Cmd.none
                                )

                            Just minutes ->
                                ( { model | page = SettingsPage { settingsModel | saving = Working } }
                                , saveSettings model settingsModel.draft minutes
                                )

                _ ->
                    ( model, Cmd.none )

        SettingsSaved result ->
            case model.page of
                SettingsPage settingsModel ->
                    case result of
                        Ok settings ->
                            let
                                draft =
                                    { repoRoot = settings.repoRoot
                                    , branch = settings.branch
                                    , testCommand = settings.testCommand
                                    , previewCommand = Maybe.withDefault "" settings.previewCommand
                                    , inactivityMinutes = String.fromInt settings.inactivityMinutes
                                    }
                            in
                            ( { model
                                | page =
                                    SettingsPage
                                        { settingsModel
                                            | saving = Completed
                                            , settings = Success settings
                                            , draft = draft
                                        }
                              }
                            , Cmd.none
                            )

                        Err err ->
                            ( { model | page = SettingsPage { settingsModel | saving = Failed (httpErrorToString err) } }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        GotPrompts result ->
            case model.page of
                SettingsPage settingsModel ->
                    case result of
                        Ok prompts ->
                            let
                                edits =
                                    List.map (
                                        \prompt ->
                                            { key = prompt.key
                                            , content = prompt.content
                                            , description = prompt.description
                                            , version = prompt.version
                                            , status = Idle
                                            }
                                    ) prompts
                            in
                            ( { model
                                | page =
                                    SettingsPage
                                        { settingsModel
                                            | prompts = Success prompts
                                            , promptEdits = edits
                                        }
                              }
                            , Cmd.none
                            )

                        Err err ->
                            ( { model | page = SettingsPage { settingsModel | prompts = Failure (httpErrorToString err), promptEdits = [] } }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        UpdatePromptContent key content ->
            updatePromptEdit model key (
                \edit -> { edit | content = content }
            )

        UpdatePromptDescription key desc ->
            updatePromptEdit model key (
                \edit -> { edit | description = desc }
            )

        SavePrompt key ->
            case model.page of
                SettingsPage settingsModel ->
                    let
                        updatedEdits =
                            List.map
                                (
                                    \edit ->
                                        if edit.key == key then
                                            { edit | status = Working }

                                        else
                                            edit
                                )
                                settingsModel.promptEdits
                    in
                    ( { model | page = SettingsPage { settingsModel | promptEdits = updatedEdits } }
                    , case List.filter (\e -> e.key == key) settingsModel.promptEdits of
                        edit :: _ ->
                            savePrompt model edit

                        [] ->
                            Cmd.none
                    )

                _ ->
                    ( model, Cmd.none )

        PromptSaved key result ->
            case model.page of
                SettingsPage settingsModel ->
                    case result of
                        Ok prompt ->
                            let
                                updatePromptList prompts =
                                    List.map (
                                        \existing ->
                                            if existing.key == prompt.key then
                                                prompt

                                            else
                                                existing
                                    ) prompts

                                updatedPrompts =
                                    settingsModel.prompts |> remoteDataMap updatePromptList

                                updatedEdits =
                                    List.map
                                        (
                                            \edit ->
                                                if edit.key == key then
                                                    { edit
                                                        | status = Completed
                                                        , content = prompt.content
                                                        , description = prompt.description
                                                        , version = prompt.version
                                                    }

                                                else
                                                    edit
                                        )
                                        settingsModel.promptEdits
                            in
                            ( { model | page = SettingsPage { settingsModel | prompts = updatedPrompts, promptEdits = updatedEdits } }, Cmd.none )

                        Err err ->
                            let
                                updatedEdits =
                                    List.map
                                        (
                                            \edit ->
                                                if edit.key == key then
                                                    { edit | status = Failed (httpErrorToString err) }

                                                else
                                                    edit
                                        )
                                        settingsModel.promptEdits
                            in
                            ( { model | page = SettingsPage { settingsModel | promptEdits = updatedEdits } }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        ResetPrompt key ->
            case model.page of
                SettingsPage settingsModel ->
                    let
                        updatedEdits =
                            List.map
                                (
                                    \edit ->
                                        if edit.key == key then
                                            { edit | status = Working }

                                        else
                                            edit
                                )
                                settingsModel.promptEdits
                    in
                    ( { model | page = SettingsPage { settingsModel | promptEdits = updatedEdits } }
                    , resetPromptRequest model key
                    )

                _ ->
                    ( model, Cmd.none )

        PromptReset key result ->
            case model.page of
                SettingsPage settingsModel ->
                    case result of
                        Ok prompt ->
                            let
                                updatedPrompts =
                                    settingsModel.prompts
                                        |> remoteDataMap
                                            (List.map (
                                                \existing ->
                                                    if existing.key == prompt.key then
                                                        prompt

                                                    else
                                                        existing
                                            ))

                                updatedEdits =
                                    List.map
                                        (
                                            \edit ->
                                                if edit.key == key then
                                                    { edit
                                                        | status = Completed
                                                        , content = prompt.content
                                                        , description = prompt.description
                                                        , version = prompt.version
                                                    }

                                                else
                                                    edit
                                        )
                                        settingsModel.promptEdits
                            in
                            ( { model | page = SettingsPage { settingsModel | prompts = updatedPrompts, promptEdits = updatedEdits } }, Cmd.none )

                        Err err ->
                            let
                                updatedEdits =
                                    List.map
                                        (
                                            \edit ->
                                                if edit.key == key then
                                                    { edit | status = Failed (httpErrorToString err) }

                                                else
                                                    edit
                                        )
                                        settingsModel.promptEdits
                            in
                            ( { model | page = SettingsPage { settingsModel | promptEdits = updatedEdits } }, Cmd.none )

                _ ->
                    ( model, Cmd.none )


-- UPDATE HELPERS ------------------------------------------------------------

updateDashboardForm : Model -> (CreateTaskForm -> DashboardModel -> DashboardModel) -> ( Model, Cmd Msg )
updateDashboardForm model updater =
    case model.page of
        DashboardPage dash ->
            let
                updated =
                    updater dash.form dash
            in
            ( { model | page = DashboardPage updated }, Cmd.none )

        _ ->
            ( model, Cmd.none )


modifySettingsDraft : Model -> (SettingsDraft -> SettingsModel -> SettingsModel) -> ( Model, Cmd Msg )
modifySettingsDraft model updater =
    case model.page of
        SettingsPage settingsModel ->
            let
                updated =
                    updater settingsModel.draft settingsModel
            in
            ( { model | page = SettingsPage updated }, Cmd.none )

        _ ->
            ( model, Cmd.none )


updatePromptEdit : Model -> String -> (PromptEdit -> PromptEdit) -> ( Model, Cmd Msg )
updatePromptEdit model key transformer =
    case model.page of
        SettingsPage settingsModel ->
            let
                updatedEdits =
                    List.map
                        (
                            \edit ->
                                if edit.key == key then
                                    transformer edit
                                else
                                    edit
                        )
                        settingsModel.promptEdits
            in
            ( { model | page = SettingsPage { settingsModel | promptEdits = updatedEdits } }, Cmd.none )

        _ ->
            ( model, Cmd.none )


resultToRemote : Result Http.Error a -> RemoteData a
resultToRemote result =
    case result of
        Ok value ->
            Success value

        Err err ->
            Failure (httpErrorToString err)


httpErrorToString : Http.Error -> String
httpErrorToString err =
    case err of
        Http.BadUrl url ->
            "Bad URL: " ++ url

        Http.Timeout ->
            "Request timed out"

        Http.NetworkError ->
            "Network error"

        Http.BadStatus status ->
            "Unexpected status: " ++ String.fromInt status

        Http.BadBody body ->
            if String.startsWith "Decode error: " body then
                "Failed to decode response: " ++ String.dropLeft 14 body

            else
                body


-- ROUTE HANDLING ------------------------------------------------------------

changeRoute : Route -> Model -> ( Model, Cmd Msg )
changeRoute route model =
    let
        closeCmd =
            case model.activeStream of
                Just taskId ->
                    closeStatusStream taskId

                Nothing ->
                    Cmd.none

        modelCleared =
            { model | activeStream = Nothing }
    in
    case route of
        RouteDashboard ->
            let
                dashModel =
                    { tasks = Loading
                    , snapshot = Loading
                    , form = { title = "", description = "", repoRoot = "", branch = "" }
                    , submitting = False
                    , submitError = Nothing
                    }

                cmd =
                    Cmd.batch
                        [ fetchTaskList modelCleared
                        , fetchSnapshot modelCleared
                        ]
            in
            ( { modelCleared | page = DashboardPage dashModel }, Cmd.batch [ closeCmd, cmd ] )

        RouteTask taskId ->
            let
                detailModel =
                    { id = taskId
                    , detail = Loading
                    , events = []
                    , message = ""
                    , messageState = Idle
                    , statusState = Idle
                    , pingState = Idle
                    , cancelState = Idle
                    }

                openCmd =
                    openStatusStream { taskId = taskId, path = statusStreamPath modelCleared taskId }
            in
            ( { modelCleared | page = TaskDetailPage detailModel, activeStream = Just taskId }
            , Cmd.batch
                [ closeCmd
                , fetchTaskDetail modelCleared taskId
                , openCmd
                ]
            )

        RouteSettings ->
            let
                settingsModel =
                    { settings = Loading
                    , draft = { repoRoot = "", branch = "", testCommand = "", previewCommand = "", inactivityMinutes = "" }
                    , saving = Idle
                    , prompts = Loading
                    , promptEdits = []
                    }
            in
            ( { modelCleared | page = SettingsPage settingsModel }
            , Cmd.batch [ closeCmd, fetchSettings modelCleared, fetchPrompts modelCleared ]
            )

        RouteUnknown ->
            ( { modelCleared | page = NotFound }, closeCmd )


statusStreamPath : Model -> Int -> String
statusStreamPath model taskId =
    "/api/tasks/" ++ String.fromInt taskId ++ "/status-stream"


-- API HELPERS ----------------------------------------------------------------

getJson : String -> (Result Http.Error a -> msg) -> Decoder a -> Cmd msg
getJson url toMsg decoder =
    Http.get
        { url = url
        , expect = expectJsonWithError decoder toMsg
        }


patchJson : String -> Encode.Value -> (Result Http.Error a -> msg) -> Decoder a -> Cmd msg
patchJson url payload toMsg decoder =
    Http.request
        { method = "PATCH"
        , headers = []
        , url = url
        , body = Http.jsonBody payload
        , expect = expectJsonWithError decoder toMsg
        , timeout = Nothing
        , tracker = Nothing
        }


putJson : String -> Encode.Value -> (Result Http.Error a -> msg) -> Decoder a -> Cmd msg
putJson url payload toMsg decoder =
    Http.request
        { method = "PUT"
        , headers = []
        , url = url
        , body = Http.jsonBody payload
        , expect = expectJsonWithError decoder toMsg
        , timeout = Nothing
        , tracker = Nothing
        }


plainPost : String -> Encode.Value -> (Result Http.Error a -> msg) -> Decoder a -> Cmd msg
plainPost url payload toMsg decoder =
    Http.request
        { method = "POST"
        , headers = []
        , url = url
        , body = Http.jsonBody payload
        , expect = expectJsonWithError decoder toMsg
        , timeout = Nothing
        , tracker = Nothing
        }


apiUrl : Model -> String -> String
apiUrl model path =
    if String.startsWith "/" path then
        model.apiBase ++ path

    else
        model.apiBase ++ "/" ++ path


expectJsonWithError : Decoder a -> (Result Http.Error a -> msg) -> Http.Expect msg
expectJsonWithError decoder toMsg =
    let
        toMsgWithHttp result =
            case result of
                Ok value ->
                    toMsg (Ok value)

                Err message ->
                    toMsg (Err (Http.BadBody message))

        handler : Http.Response String -> Result String a
        handler response =
            case response of
                Http.BadUrl_ url ->
                    Err ("Bad URL: " ++ url)

                Http.Timeout_ ->
                    Err "Request timed out"

                Http.NetworkError_ ->
                    Err "Network error"

                Http.BadStatus_ metadata body ->
                    case Decode.decodeString errorMessageDecoder body of
                        Ok message ->
                            Err message

                        Err _ ->
                            Err (fallbackErrorMessage metadata.statusCode body)

                Http.GoodStatus_ _ body ->
                    case Decode.decodeString decoder body of
                        Ok value ->
                            Ok value

                        Err decodeErr ->
                            Err ("Decode error: " ++ Decode.errorToString decodeErr)
    in
    Http.expectStringResponse toMsgWithHttp handler


errorMessageDecoder : Decoder String
errorMessageDecoder =
    Decode.oneOf
        [ Decode.field "error" Decode.string
        , Decode.field "message" Decode.string
        , Decode.field "error" (Decode.field "message" Decode.string)
        ]

fallbackErrorMessage : Int -> String -> String
fallbackErrorMessage statusCode body =
    let
        cleaned =
            body
                |> String.replace "<br>" " "
                |> String.replace "<br/>" " "
                |> String.replace "<li>" " "
                |> String.replace "</li>" " "
                |> String.replace "<ul>" " "
                |> String.replace "</ul>" " "
                |> String.replace "<h1>" " "
                |> String.replace "</h1>" " "
                |> String.replace "<title>" " "
                |> String.replace "</title>" " "
                |> String.replace "</body>" " "
                |> String.replace "</html>" " "
                |> String.trim
    in
    if String.isEmpty cleaned then
        "HTTP " ++ String.fromInt statusCode

    else
        cleaned


-- API COMMANDS ---------------------------------------------------------------

fetchTaskList : Model -> Cmd Msg
fetchTaskList model =
    getJson (apiUrl model "/tasks") GotTaskList (Decode.list taskSummaryDecoder)


fetchSnapshot : Model -> Cmd Msg
fetchSnapshot model =
    getJson (apiUrl model "/snapshot") GotSnapshot snapshotDecoder


createTask : Model -> CreateTaskForm -> Cmd Msg
createTask model form =
    let
        payload =
            Encode.object
                [ ( "taskReqTitle", Encode.string form.title )
                , ( "taskReqDescription", Encode.string form.description )
                , ( "taskReqRepoRoot"
                  , case String.trim form.repoRoot of
                        "" -> Encode.null
                        repo -> Encode.string repo
                  )
                , ( "taskReqBranch"
                  , case String.trim form.branch of
                        "" -> Encode.null
                        b -> Encode.string b
                  )
                ]
    in
    plainPost (apiUrl model "/tasks") payload CreatedTask taskSummaryDecoder


fetchTaskDetail : Model -> Int -> Cmd Msg
fetchTaskDetail model taskId =
    getJson (apiUrl model ("/tasks/" ++ String.fromInt taskId)) (GotTaskDetail taskId) taskDetailDecoder


sendTaskMessage : Model -> Int -> String -> Cmd Msg
sendTaskMessage model taskId message =
    let
        payload =
            Encode.object
                [ ( "agentMessage", Encode.string message ) ]
    in
    plainPost (apiUrl model ("/tasks/" ++ String.fromInt taskId ++ "/messages")) payload (
        \result ->
            case result of
                Ok _ ->
                    MessageSent (Ok ())

                Err err ->
                    MessageSent (Err err)
        )
        (Decode.field "ok" Decode.bool |> Decode.andThen (
            \isOk ->
                if isOk then
                    Decode.succeed ()

                else
                    Decode.fail "message send failed"
        ))


updateTaskStatus : Model -> Int -> TaskStatus -> Cmd Msg
updateTaskStatus model taskId status =
    patchJson
        (apiUrl model ("/tasks/" ++ String.fromInt taskId))
        (Encode.object
            [ ( "taskStatus", Encode.string (taskStatusToString status) ) ]
        )
        TaskStatusUpdated
        taskDetailDecoder


pingPreview : Model -> Int -> Cmd Msg
pingPreview model taskId =
    plainPost (apiUrl model ("/preview/" ++ String.fromInt taskId ++ "/ping"))
        Encode.null
        PreviewPinged
        previewPingDecoder


cancelTaskRequest : Model -> Int -> Cmd Msg
cancelTaskRequest model taskId =
    plainPost (apiUrl model ("/tasks/" ++ String.fromInt taskId ++ "/cancel"))
        (Encode.object [])
        TaskCancelled
        taskDetailDecoder


fetchSettings : Model -> Cmd Msg
fetchSettings model =
    getJson (apiUrl model "/settings") GotSettings settingsDecoder


saveSettings : Model -> SettingsDraft -> Int -> Cmd Msg
saveSettings model draft inactivityValue =
    let
        payload =
            Encode.object
                [ ( "settingsRepoRoot", Encode.string draft.repoRoot )
                , ( "settingsBranch", Encode.string draft.branch )
                , ( "settingsUpdateTestCommand", Encode.string draft.testCommand )
                , ( "settingsUpdatePreviewCommand"
                  , case String.trim draft.previewCommand of
                        "" -> Encode.null
                        value -> Encode.string value
                  )
                , ( "settingsUpdateInactivityMinutes", Encode.int inactivityValue )
                ]
    in
    putJson (apiUrl model "/settings") payload SettingsSaved settingsDecoder


fetchPrompts : Model -> Cmd Msg
fetchPrompts model =
    getJson (apiUrl model "/prompts") GotPrompts (Decode.list promptTemplateDecoder)


savePrompt : Model -> PromptEdit -> Cmd Msg
savePrompt model edit =
    let
        payload =
            Encode.object
                [ ( "promptUpdateContent", Encode.string edit.content )
                , ( "promptUpdateDescription", Encode.string edit.description )
                , ( "promptUpdateVersion", Encode.int edit.version )
                ]

        url = apiUrl model ("/prompts/" ++ edit.key)
    in
    putJson url payload (PromptSaved edit.key) promptTemplateDecoder


resetPromptRequest : Model -> String -> Cmd Msg
resetPromptRequest model key =
    plainPost (apiUrl model ("/prompts/" ++ key ++ "/reset")) (Encode.object []) (PromptReset key) promptTemplateDecoder


-- SUBSCRIPTIONS --------------------------------------------------------------

subscriptions : Model -> Sub Msg
subscriptions model =
    case model.activeStream of
        Just _ ->
            receiveStatusEvent ReceiveStatusEvent

        Nothing ->
            Sub.none


-- VIEW ----------------------------------------------------------------------

view : Model -> Browser.Document Msg
view model =
    { title = "AI Project Manager"
    , body =
        [ main_ [ class "min-h-screen bg-slate-950 text-slate-100" ]
            [ viewHeader model
            , case model.page of
                DashboardPage dash ->
                    viewDashboard dash

                TaskDetailPage detail ->
                    viewTaskDetail detail

                SettingsPage settingsModel ->
                    viewSettings settingsModel

                PageLoading _ ->
                    div [ class "p-8" ] [ text "Loading…" ]

                NotFound ->
                    div [ class "p-8" ] [ text "Not found" ]
            , viewError model.error
            ]
        ]
    }


viewHeader : Model -> Html Msg
viewHeader model =
    nav [ class "flex items-center justify-between border-b border-slate-800 bg-slate-900/80 px-8 py-4" ]
        [ div []
            [ h1 [ class "text-xl font-semibold" ] [ text "AI Project Manager" ]
            , p [ class "text-xs text-slate-400" ] [ text "Automated Codex workflow orchestrator" ]
            ]
        , div [ class "flex items-center gap-4 text-sm" ]
            [ headerLink "/" "Tasks"
            , headerLink "/settings" "Settings"
            ]
        ]


headerLink : String -> String -> Html Msg
headerLink path labelText =
    a
        [ href path
        , class "rounded px-3 py-1 text-sm text-slate-300 hover:bg-slate-800 hover:text-indigo-200"
        ]
        [ text labelText ]


viewError : Maybe String -> Html Msg
viewError maybeError =
    case maybeError of
        Nothing ->
            text ""

        Just message ->
            div [ class "fixed bottom-4 right-4 max-w-sm rounded-lg border border-rose-500/60 bg-rose-500/15 p-4" ]
                [ div [ class "flex items-start justify-between gap-4" ]
                    [ div []
                        [ h3 [ class "text-sm font-semibold text-rose-200" ] [ text "Error" ]
                        , p [ class "text-xs text-rose-100" ] [ text message ]
                        ]
                    , button [ class "text-xs text-rose-200", onClick DismissError ] [ text "Dismiss" ]
                    ]
                ]


-- DASHBOARD VIEW -------------------------------------------------------------

viewDashboard : DashboardModel -> Html Msg
viewDashboard model =
    div [ class "space-y-8 p-8" ]
        [ section [ class "rounded-xl border border-slate-900 bg-slate-900/80 p-6" ]
            [ h2 [ class "text-lg font-semibold" ] [ text "Create Task" ]
            , form [ class "mt-4 grid gap-4 md:grid-cols-2", onSubmit SubmitCreateTask ]
                [ viewInput "Title" model.form.title UpdateCreateTitle True
                , viewInput "Repo Root (optional)" model.form.repoRoot UpdateCreateRepo False
                , viewTextarea "Description" model.form.description UpdateCreateDescription
                , viewInput "Branch (optional)" model.form.branch UpdateCreateBranch False
                , div [ class "md:col-span-2 flex justify-end" ]
                    [ button
                        [ class "rounded bg-indigo-600 px-4 py-2 text-sm font-medium text-white hover:bg-indigo-500 disabled:opacity-40"
                        , disabled (model.submitting || String.trim model.form.title == "" || String.trim model.form.description == "")
                        ]
                        [ text
                            (if model.submitting then
                                "Creating…"

                             else
                                "Create Task"
                            )
                        ]
                    ]
                ]
            , case model.submitError of
                Nothing ->
                    text ""

                Just err ->
                    p [ class "mt-3 text-sm text-rose-300" ] [ text err ]
            ]
        , section [ class "rounded-xl border border-slate-900 bg-slate-900/80" ]
            [ viewSnapshot model.snapshot
            , div [] (viewTaskTable model.tasks)
            ]
        ]


viewSnapshot : RemoteData OrchestratorSnapshot -> Html Msg
viewSnapshot data =
    case data of
        Success snapshot ->
            div [ class "flex items-center justify-between border-b border-slate-800 px-6 py-4" ]
                [ div []
                    [ h3 [ class "text-lg font-semibold" ] [ text "Task Queue" ]
                    , p [ class "text-xs text-slate-400" ] [ text ("Queue depth: " ++ String.fromInt snapshot.queueDepth) ]
                    ]
                , div [ class "flex gap-2" ]
                    (List.map viewSnapshotTask (List.take 3 snapshot.activeTasks))
                ]

        Loading ->
            div [ class "border-b border-slate-800 px-6 py-4 text-sm text-slate-400" ] [ text "Loading snapshot…" ]

        Failure err ->
            div [ class "border-b border-slate-800 px-6 py-4 text-sm text-rose-300" ] [ text err ]

        NotAsked ->
            text ""


viewSnapshotTask : TaskSummary -> Html Msg
viewSnapshotTask summary =
    span [ class "rounded border border-slate-800 px-3 py-1 text-xs text-slate-200" ]
        [ text summary.title ]


viewTaskTable : RemoteData (List TaskSummary) -> List (Html Msg)
viewTaskTable data =
    case data of
        Loading ->
            [ div [ class "px-6 py-6 text-sm text-slate-400" ] [ text "Loading tasks…" ] ]

        Failure err ->
            [ div [ class "px-6 py-6 text-sm text-rose-300" ] [ text err ] ]

        Success tasks ->
            if List.isEmpty tasks then
                [ div [ class "px-6 py-6 text-center text-sm text-slate-500" ] [ text "No tasks yet." ] ]

            else
                List.map viewTaskRow tasks

        NotAsked ->
            []


viewTaskRow : TaskSummary -> Html Msg
viewTaskRow summary =
    div [ class "border-t border-slate-900 px-6 py-4" ]
        [ div [ class "flex items-start justify-between" ]
            [ div []
                [ h4 [ class "text-base font-semibold text-slate-100" ] [ text summary.title ]
                , p [ class "text-xs text-slate-500" ]
                    [ text summary.repoRoot
                    , text " · base "
                    , text summary.branch
                    , case summary.featureBranch of
                        Just feature ->
                            text (" · feature " ++ feature)

                        Nothing ->
                            text ""
                    ]
                ]
            , a [ class "text-xs text-indigo-300 hover:text-indigo-100", href ("/task/" ++ String.fromInt summary.id) ] [ text "View" ]
            ]
        , div [ class "mt-3 flex items-center gap-3 text-xs" ]
            [ statusBadge summary.status
            , previewBadge summary.previewStatus
            , case summary.previewUrl of
                Just url ->
                    a [ class "text-indigo-300 hover:text-indigo-100", href url, target "_blank", rel "noreferrer" ] [ text "Preview" ]

                Nothing ->
                    span [ class "text-slate-500" ] [ text "No preview" ]
            ]
        ]


viewInput : String -> String -> (String -> Msg) -> Bool -> Html Msg
viewInput labelText current msg _ =
    label [ class "flex flex-col gap-1" ]
        [ span [ class "text-xs font-semibold uppercase tracking-wide text-slate-400" ] [ text labelText ]
        , input
            [ class "rounded bg-slate-800 px-3 py-2 text-sm text-slate-100 focus:outline-none focus:ring-2 focus:ring-indigo-500"
            , type_ "text"
            , value current
            , onInput msg
            ]
            []
        ]


viewNumberInput : String -> String -> (String -> Msg) -> Html Msg
viewNumberInput labelText current msg =
    label [ class "flex flex-col gap-1" ]
        [ span [ class "text-xs font-semibold uppercase tracking-wide text-slate-400" ] [ text labelText ]
        , input
            [ class "rounded bg-slate-800 px-3 py-2 text-sm text-slate-100 focus:outline-none focus:ring-2 focus:ring-indigo-500"
            , type_ "number"
            , min "1"
            , value current
            , onInput msg
            ]
            []
        ]


viewTextarea : String -> String -> (String -> Msg) -> Html Msg
viewTextarea labelText current msg =
    label [ class "flex flex-col gap-1 md:col-span-2" ]
        [ span [ class "text-xs font-semibold uppercase tracking-wide text-slate-400" ] [ text labelText ]
        , textarea
            [ class "min-h-[120px] rounded bg-slate-800 px-3 py-2 text-sm text-slate-100 focus:outline-none focus:ring-2 focus:ring-indigo-500"
            , value current
            , onInput msg
            ]
            []
        ]


statusBadge : TaskStatus -> Html Msg
statusBadge status =
    let
        ( labelText, classes ) =
            case status of
                TaskStatusPending ->
                    ( "Pending", "border-slate-700 text-slate-300" )

                TaskStatusDesigning ->
                    ( "Designing", "border-indigo-500/50 text-indigo-200" )

                TaskStatusImplementing ->
                    ( "Implementing", "border-blue-500/50 text-blue-200" )

                TaskStatusReviewing ->
                    ( "Reviewing", "border-amber-500/40 text-amber-200" )

                TaskStatusQa ->
                    ( "QA", "border-emerald-500/40 text-emerald-200" )

                TaskStatusBlocked ->
                    ( "Blocked", "border-rose-500/60 text-rose-200" )

                TaskStatusCompleted ->
                    ( "Completed", "border-emerald-500/60 text-emerald-200" )

                TaskStatusDiscarded ->
                    ( "Discarded", "border-slate-700 text-slate-400" )

                TaskStatusCancelled ->
                    ( "Cancelled", "border-slate-700 text-slate-500" )

                TaskStatusUnknown raw ->
                    ( raw, "border-slate-700 text-slate-300" )
    in
    span [ class ("rounded border px-2 py-1 text-xs font-semibold " ++ classes) ] [ text labelText ]


previewBadge : PreviewStatus -> Html Msg
previewBadge status =
    let
        ( labelText, classes ) =
            case status of
                PreviewOffline ->
                    ( "Preview offline", "border-slate-700 text-slate-400" )

                PreviewLaunching ->
                    ( "Launching", "border-amber-500/40 text-amber-200" )

                PreviewOnline ->
                    ( "Preview live", "border-emerald-500/60 text-emerald-200" )

                PreviewFailedStatus ->
                    ( "Preview failed", "border-rose-500/60 text-rose-200" )

                PreviewStatusUnknown raw ->
                    ( raw, "border-slate-700 text-slate-300" )
    in
    span [ class ("rounded border px-2 py-1 text-xs " ++ classes) ] [ text labelText ]


-- TASK DETAIL VIEW -----------------------------------------------------------

viewTaskDetail : TaskDetailModel -> Html Msg
viewTaskDetail model =
    div [ class "space-y-8 p-8" ]
        [ viewTaskSummary model.detail
        , viewTimeline model
        , viewTaskActions model
        , viewArtifacts model.detail
        ]


viewTaskSummary : RemoteData TaskDetail -> Html Msg
viewTaskSummary data =
    case data of
        Success detail ->
            let
                summary = detail.summary
            in
            section [ class "rounded-xl border border-slate-900 bg-slate-900/80 p-6" ]
                [ div [ class "flex flex-col gap-4 md:flex-row md:items-center md:justify-between" ]
                    [ div []
                        [ div [ class "flex items-center gap-3" ]
                            [ statusBadge summary.status
                            , h2 [ class " text-2xl font-semibold" ] [ text summary.title ]
                            ]
                        , p [ class "mt-2 text-xs text-slate-400" ]
                            [ text ("Repo: " ++ summary.repoRoot)
                            , text " — Base: "
                            , text summary.branch
                            , case summary.featureBranch of
                                Just feature ->
                                    text (" — Feature: " ++ feature)

                                Nothing ->
                                    text ""
                            ]
                        ]
                    , div [ class "flex flex-col items-end gap-2 text-xs text-slate-400" ]
                        [ previewBadge summary.previewStatus
                        , case summary.previewUrl of
                            Just url ->
                                a [ class "text-indigo-300 hover:text-indigo-100", href url, target "_blank", rel "noreferrer" ] [ text "Open preview" ]

                            Nothing ->
                                span [] [ text "No preview yet" ]
                        , button
                            [ class "rounded border border-slate-700 px-3 py-1 text-xs text-slate-300 hover:border-indigo-400"
                            , onClick TriggerPreviewPing
                            ]
                            [ text "Ping preview" ]
                        ]
                    ]
                ]

        Loading ->
            div [ class "rounded-xl border border-slate-900 bg-slate-900/80 p-6" ] [ text "Loading…" ]

        Failure err ->
            div [ class "rounded-xl border border-slate-900 bg-slate-900/80 p-6 text-rose-300" ] [ text err ]

        NotAsked ->
            text ""


viewTimeline : TaskDetailModel -> Html Msg
viewTimeline model =
    section [ class "rounded-xl border border-slate-900 bg-slate-900/80 p-6" ]
        [ h3 [ class "text-lg font-semibold mb-4" ] [ text "Live Timeline" ]
        , case model.detail of
            Success _ ->
                ul [ class "space-y-4" ]
                    (List.map viewEvent model.events)

            Loading ->
                p [ class "text-sm text-slate-400" ] [ text "Loading events…" ]

            Failure err ->
                p [ class "text-sm text-rose-300" ] [ text err ]

            NotAsked ->
                text ""
        ]


viewEvent : StatusEvent -> Html Msg
viewEvent event =
    li [ class "border border-slate-900 bg-slate-950/70 p-4 rounded" ]
        [ div [ class "text-xs uppercase tracking-wide text-slate-500" ] [ text (workflowStepToString event.step) ]
        , p [ class "mt-1 text-sm text-slate-200" ] [ text event.message ]
        , span [ class "mt-1 block text-xs text-slate-500" ] [ text event.createdAt ]
        , case event.payload of
            Nothing ->
                text ""

            Just payloadValue ->
                pre [ class "mt-3 whitespace-pre-wrap text-xs text-slate-300" ] [ text (Encode.encode 2 payloadValue) ]
        ]


viewTaskActions : TaskDetailModel -> Html Msg
viewTaskActions model =
    section [ class "grid gap-6 md:grid-cols-[2fr,1fr]" ]
        [ div [ class "rounded-xl border border-slate-900 bg-slate-900/80 p-6" ]
            [ h3 [ class "text-sm font-semibold uppercase tracking-wide text-slate-400" ] [ text "Human Intervention" ]
            , form [ class "mt-3 flex flex-col gap-2", onSubmit SubmitMessage ]
                [ textarea
                    [ class "h-24 rounded bg-slate-950 px-3 py-2 text-sm text-slate-100 focus:outline-none focus:ring-2 focus:ring-indigo-500"
                    , placeholder "Send guidance or unblock an agent…"
                    , value model.message
                    , onInput UpdateMessageInput
                    ]
                    []
                , button
                    [ class "self-end rounded bg-indigo-600 px-3 py-1 text-xs font-semibold text-white hover:bg-indigo-500 disabled:opacity-40"
                    , disabled (String.trim model.message == "" || model.messageState == Working)
                    ]
                    [ text
                        (case model.messageState of
                            Working ->
                                "Sending…"

                            Failed _ ->
                                "Retry Send"

                            _ ->
                                "Send Message"
                        )
                    ]
                , case model.messageState of
                    Failed err ->
                        p [ class "text-xs text-rose-300" ] [ text err ]

                    Completed ->
                        p [ class "text-xs text-emerald-300" ] [ text "Message sent." ]

                    _ ->
                        text ""
                ]
            ]
        , div [ class "space-y-4" ]
            [ section [ class "rounded-xl border border-slate-900 bg-slate-900/80 p-6" ]
                [ h3 [ class "text-sm font-semibold uppercase tracking-wide text-slate-400" ] [ text "Task Actions" ]
                , div [ class "mt-3 space-y-2" ]
                    [ button
                        [ class "w-full rounded border border-emerald-500/40 bg-emerald-500/10 px-3 py-2 text-xs font-semibold text-emerald-200 hover:border-emerald-400"
                        , onClick (UpdateTaskStatus TaskStatusCompleted)
                        ]
                        [ text "Mark as Merged" ]
                    , button
                        [ class "w-full rounded border border-rose-500/40 bg-rose-500/10 px-3 py-2 text-xs font-semibold text-rose-200 hover:border-rose-400"
                        , onClick (UpdateTaskStatus TaskStatusDiscarded)
                        ]
                        [ text "Mark as Discarded" ]
                    , button
                        [ class "w-full rounded border border-amber-500/40 bg-amber-500/10 px-3 py-2 text-xs font-semibold text-amber-200 hover:border-amber-400 disabled:opacity-40"
                        , onClick CancelTask
                        , disabled (model.cancelState == Working)
                        ]
                        [ text <|
                            case model.cancelState of
                                Working ->
                                    "Killing…"

                                _ ->
                                    "Kill Task"
                        ]
                    , case model.statusState of
                        Failed err ->
                            p [ class "text-xs text-rose-300" ] [ text err ]

                        Working ->
                            p [ class "text-xs text-slate-400" ] [ text "Updating status…" ]

                        Completed ->
                            p [ class "text-xs text-emerald-300" ] [ text "Status updated." ]

                        Idle ->
                            text ""
                    , case model.cancelState of
                        Failed err ->
                            p [ class "text-xs text-rose-300" ] [ text err ]

                        Completed ->
                            p [ class "text-xs text-emerald-300" ] [ text "Task cancelled." ]

                        Working ->
                            p [ class "text-xs text-slate-400" ] [ text "Cancelling task…" ]

                        Idle ->
                            text ""
                    ]
                ]
            ]
        ]


viewArtifacts : RemoteData TaskDetail -> Html Msg
viewArtifacts data =
    case data of
        Success detail ->
            section [ class "rounded-xl border border-slate-900 bg-slate-900/80 p-6" ]
                [ h3 [ class "text-lg font-semibold mb-4" ] [ text "Artifacts" ]
                , if List.isEmpty detail.artifacts then
                    p [ class "text-sm text-slate-400" ] [ text "No artifacts recorded yet." ]

                  else
                    div [ class "grid gap-4 md:grid-cols-2" ] (List.map viewArtifact detail.artifacts)
                ]

        Loading ->
            div [ class "rounded-xl border border-slate-900 bg-slate-900/80 p-6" ] [ text "Loading artifacts…" ]

        Failure err ->
            div [ class "rounded-xl border border-slate-900 bg-slate-900/80 p-6 text-rose-300" ] [ text err ]

        NotAsked ->
            text ""


viewArtifact : Artifact -> Html Msg
viewArtifact artifact =
    div [ class "rounded border border-slate-900 bg-slate-950/60 p-4" ]
        [ div [ class "flex items-center justify-between" ]
            [ span [ class "text-sm font-semibold text-slate-200" ] [ text artifact.label ]
            , span [ class "text-xs text-slate-500" ] [ text (artifactKindText artifact.kind) ]
            ]
        , span [ class "mt-1 block text-xs text-slate-500" ] [ text artifact.createdAt ]
        , case artifact.body of
            Just value ->
                pre [ class "mt-3 whitespace-pre-wrap text-xs text-slate-300" ] [ text (Encode.encode 2 value) ]

            Nothing ->
                text ""
        , case artifact.path of
            Just pathStr ->
                p [ class "mt-3 text-xs text-indigo-300" ] [ text ("Path: " ++ pathStr) ]

            Nothing ->
                text ""
        ]


artifactKindText : ArtifactKind -> String
artifactKindText kind =
    case kind of
        ArtifactDesign ->
            "Design"

        ArtifactDiff ->
            "Diff"

        ArtifactTestLog ->
            "Tests"

        ArtifactCommitLog ->
            "Commit"

        ArtifactPreviewLog ->
            "Preview Log"

        ArtifactPreviewPing ->
            "Preview Ping"

        ArtifactAgentTranscript ->
            "Transcript"

        ArtifactUnknown raw ->
            raw


-- SETTINGS VIEW --------------------------------------------------------------

viewSettings : SettingsModel -> Html Msg
viewSettings model =
    div [ class "space-y-8 p-8" ]
        [ section [ class "rounded-xl border border-slate-900 bg-slate-900/80 p-6" ]
            [ h2 [ class "text-lg font-semibold" ] [ text "Repository Defaults" ]
            , case model.settings of
                Loading ->
                    p [ class "mt-2 text-sm text-slate-400" ] [ text "Loading settings…" ]

                Failure err ->
                    p [ class "mt-2 text-sm text-rose-300" ] [ text err ]

                _ ->
                    text ""
            , form [ class "mt-4 grid gap-4 md:grid-cols-2", onSubmit SubmitSettings ]
                [ viewInput "Repo Root" model.draft.repoRoot UpdateSettingsRepo True
                , viewInput "Default Branch" model.draft.branch UpdateSettingsBranch True
                , viewInput "Test Command" model.draft.testCommand UpdateSettingsTestCommand True
                , viewInput "Preview Command" model.draft.previewCommand UpdateSettingsPreviewCommand False
                , viewNumberInput "Agent Timeout (minutes)" model.draft.inactivityMinutes UpdateSettingsInactivity
                , div [ class "md:col-span-2 flex justify-end" ]
                    [ button
                        [ class "rounded bg-indigo-600 px-4 py-2 text-sm font-semibold text-white hover:bg-indigo-500 disabled:opacity-40"
                        , disabled (model.saving == Working)
                        ]
                        [ text
                            (case model.saving of
                                Working ->
                                    "Saving…"

                                _ ->
                                    "Save Defaults"
                            )
                        ]
                    ]
                , case model.saving of
                    Failed err ->
                        p [ class "md:col-span-2 text-xs text-rose-300" ] [ text err ]

                    Completed ->
                        p [ class "md:col-span-2 text-xs text-emerald-300" ] [ text "Settings saved." ]

                    _ ->
                        text ""
                ]
            ]
        , section [ class "rounded-xl border border-slate-900 bg-slate-900/80 p-6" ]
            [ h2 [ class "text-lg font-semibold" ] [ text "Agent Prompts" ]
            , case ( model.prompts, model.promptEdits ) of
                ( Loading, _ ) ->
                    p [ class "mt-2 text-sm text-slate-400" ] [ text "Loading prompts…" ]

                ( Failure err, _ ) ->
                    p [ class "mt-2 text-sm text-rose-300" ] [ text err ]

                ( Success prompts, edits ) ->
                    div [ class "mt-4 space-y-4" ]
                        (List.map (viewPromptEditor edits) prompts)

                ( NotAsked, _ ) ->
                    text ""
            ]
        ]


viewPromptEditor : PromptEdits -> PromptTemplate -> Html Msg
viewPromptEditor edits prompt =
    let
        currentEdit =
            List.filter (
                \edit -> edit.key == prompt.key
            ) edits
                |> List.head
    in
    case currentEdit of
        Nothing ->
            div [] [ text "" ]

        Just edit ->
            div [ class "rounded border border-slate-900 bg-slate-950/60 p-4" ]
                [ div [ class "flex items-center justify-between" ]
                    [ div []
                        [ h4 [ class "text-sm font-semibold text-slate-100" ] [ text prompt.key ]
                        , input
                            [ class "mt-1 w-full rounded bg-slate-900/80 px-2 py-1 text-xs text-slate-200"
                            , value edit.description
                            , onInput (UpdatePromptDescription prompt.key)
                            ]
                            []
                        ]
                    , span [ class "text-xs text-slate-500" ] [ text ("v" ++ String.fromInt edit.version) ]
                    ]
                , textarea
                    [ class "mt-3 h-48 w-full rounded bg-slate-900/60 px-3 py-2 text-xs text-slate-200"
                    , value edit.content
                    , onInput (UpdatePromptContent prompt.key)
                    ]
                    []
                , div [ class "mt-3 flex gap-2" ]
                    [ button
                        [ class "rounded border border-emerald-500/40 px-3 py-1 text-xs font-semibold text-emerald-200 hover:border-emerald-400"
                        , onClick (SavePrompt prompt.key)
                        , disabled (edit.status == Working)
                        ]
                        [ text
                            (case edit.status of
                                Working ->
                                    "Saving…"

                                _ ->
                                    "Save"
                            )
                        ]
                    , button
                        [ class "rounded border border-slate-700 px-3 py-1 text-xs text-slate-300 hover:border-slate-500"
                        , onClick (ResetPrompt prompt.key)
                        , disabled (edit.status == Working)
                        ]
                        [ text "Reset" ]
                    , case edit.status of
                        Failed err ->
                            span [ class "text-xs text-rose-300" ] [ text err ]

                        Completed ->
                            span [ class "text-xs text-emerald-300" ] [ text "Updated." ]

                        _ ->
                            text ""
                    ]
                ]


-- SUBSCRIPTIONS & VIEW END --------------------------------------------------

-- Already implemented above


-- COMMAND HELPERS ------------------------------------------------------------

-- END -----------------------------------------------------------------------
