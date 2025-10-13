port module Main exposing (main)

import Browser
import Browser.Navigation as Nav
import Dict exposing (Dict)
import Generated.Api as Api exposing (AgentRole(..), ArtifactKind(..), PreviewStatus(..), TaskStatus(..), WorkerMetricDTO, WorkerStatusStateDTO(..), WorkflowStep(..))
import Html exposing (Html, a, button, div, form, h1, h2, h3, h4, input, label, li, main_, nav, option, p, pre, section, select, span, strong, text, textarea, ul)
import Html.Attributes exposing (checked, class, disabled, href, min, placeholder, rel, target, type_, value)
import Html.Events exposing (onCheck, onClick, onInput, onSubmit)
import Html.Lazy as Lazy
import Http
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode
import Maybe
import Process
import String
import Task
import Time exposing (Month(..), Posix, toDay, toHour, toMinute, toMonth, toSecond, toYear, utc)
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
        trimmed =
            String.trim raw
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
    , notifications : List Notification
    , nextNotificationId : Int
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
    , timelineVisibleCount : Int
    , timelineNextCursor : Maybe Int
    , agentLogs : AgentLogs
    , agentLogVisible : Dict String Int
    , selectedTab : DetailTab
    , testCommandDraft : String
    , testCommandState : RequestState
    , qaSkipState : RequestState
    , previewStartState : RequestState
    , message : String
    , messageState : RequestState
    , messageTargetRole : Maybe AgentRole
    , messageAutoResume : Bool
    , statusState : RequestState
    , pingState : RequestState
    , cancelState : RequestState
    , retryState : RequestState
    , pauseState : RequestState
    , reassignState : RequestState
    , redirectState : RequestState
    , redirectTarget : Maybe Int
    , snoozeMinutesDraft : String
    , snoozeState : RequestState
    , snapshot : RemoteData OrchestratorSnapshot
    , historyState : RequestState
    , historyRequest : Maybe HistoryRequestOrigin
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


type DetailTab
    = TabActivity
    | TabLogs
    | TabArtifacts


type HistoryRequestOrigin
    = HistoryFromTimeline
    | HistoryFromLog AgentRole


type RemoteData a
    = NotAsked
    | Loading
    | Success a
    | Failure String


type alias Notification =
    { id : Int
    , text : String
    }


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


snapshotPollInterval : Float
snapshotPollInterval =
    5000


scheduleSnapshot : Model -> Cmd Msg
scheduleSnapshot model =
    case model.page of
        DashboardPage _ ->
            Process.sleep snapshotPollInterval
                |> Task.perform (\_ -> SnapshotPoll)

        _ ->
            Cmd.none



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



-- DATA TYPES --------------------------------------------------------------


type alias TaskSummary =
    { id : Int
    , title : String
    , status : TaskStatus
    , repoRoot : String
    , branch : String
    , featureBranch : Maybe String
    , updatedAt : Posix
    , previewUrl : Maybe String
    , previewStatus : PreviewStatus
    , isPaused : Bool
    , snoozeUntil : Maybe Posix
    }


type alias TaskRun =
    { ordinal : Int
    , step : WorkflowStep
    , summary : Maybe String
    , createdAt : Posix
    , updatedAt : Posix
    }


type alias StatusEvent =
    { id : Maybe Int
    , step : WorkflowStep
    , message : String
    , createdAt : Posix
    , payload : Maybe Decode.Value
    }


type alias Artifact =
    { kind : ArtifactKind
    , label : String
    , body : Maybe Decode.Value
    , path : Maybe String
    , createdAt : Posix
    }


type alias AgentLogEntry =
    { role : AgentRole
    , step : WorkflowStep
    , stream : String
    , line : String
    , createdAt : Posix
    }


type alias AgentLogs =
    Dict String (List AgentLogEntry)


type alias TaskDetail =
    { summary : TaskSummary
    , runs : List TaskRun
    , events : List StatusEvent
    , artifacts : List Artifact
    , testCommand : String
    , testCommandOverride : Maybe String
    , isPaused : Bool
    , snoozeUntil : Maybe Posix
    , eventNextCursor : Maybe Int
    }


type alias TaskHistoryPage =
    { events : List StatusEvent
    , nextCursor : Maybe Int
    }


type alias OrchestratorSnapshot =
    { activeTasks : List TaskSummary
    , queueDepth : Int
    , workers : List WorkerStatus
    , pausedTasks : List Int
    , workerMetrics : List WorkerMetric
    }


type alias TaskRedirectResponse =
    { requeuedStep : WorkflowStep
    , targetTaskId : Maybe Int
    , targetStep : Maybe WorkflowStep
    , workerId : Maybe Int
    }


defaultTimelineWindow : Int
defaultTimelineWindow =
    50


timelineChunkSize : Int
timelineChunkSize =
    25


defaultLogWindow : Int
defaultLogWindow =
    200


logChunkSize : Int
logChunkSize =
    100


timelineBaselineCount : List StatusEvent -> Int
timelineBaselineCount events =
    let
        total =
            List.length events
    in
    if total <= 0 then
        0

    else
        Basics.min total defaultTimelineWindow


normalizeTimelineCount : Int -> List StatusEvent -> Int
normalizeTimelineCount desired events =
    let
        total =
            List.length events

        baseline =
            timelineBaselineCount events
    in
    if total <= 0 then
        0

    else
        Basics.min total (Basics.max baseline desired)


logBaselineCount : List AgentLogEntry -> Int
logBaselineCount entries =
    let
        total =
            List.length entries
    in
    if total <= 0 then
        0

    else
        Basics.min total defaultLogWindow


normalizeLogCount : Int -> List AgentLogEntry -> Int
normalizeLogCount desired entries =
    let
        total =
            List.length entries

        baseline =
            logBaselineCount entries
    in
    if total <= 0 then
        0

    else
        Basics.min total (Basics.max baseline desired)


takeNewest : Int -> List a -> List a
takeNewest count items =
    if count <= 0 then
        []

    else
        let
            total =
                List.length items

            dropCount =
                Basics.max 0 (total - count)
        in
        List.drop dropCount items


updateLogVisibility : AgentLogs -> Dict String Int -> Dict String Int
updateLogVisibility logs existing =
    Dict.foldl
        (\key entries acc ->
            let
                desired =
                    Dict.get key existing |> Maybe.withDefault (logBaselineCount entries)
            in
            Dict.insert key (normalizeLogCount desired entries) acc
        )
        Dict.empty
        logs


mergeAgentLogs : AgentLogs -> AgentLogs -> AgentLogs
mergeAgentLogs newLogs existing =
    Dict.foldl
        (\key entries acc ->
            let
                current =
                    Dict.get key acc |> Maybe.withDefault []
            in
            Dict.insert key (current ++ entries) acc
        )
        existing
        newLogs


agentLogLimit : Dict String Int -> String -> List AgentLogEntry -> Int
agentLogLimit limits key entries =
    let
        desired =
            Dict.get key limits |> Maybe.withDefault (logBaselineCount entries)
    in
    normalizeLogCount desired entries


hydrateDetailModel : TaskDetailModel -> TaskDetail -> TaskDetailModel
hydrateDetailModel detailModel detail =
    let
        ( timelineEvents, logs ) =
            splitEvents detail.events

        sanitizedDetail =
            { detail | events = timelineEvents }

        normalizedTimeline =
            normalizeTimelineCount detailModel.timelineVisibleCount timelineEvents

        updatedVisibility =
            updateLogVisibility logs detailModel.agentLogVisible
    in
    { detailModel
        | detail = Success sanitizedDetail
        , events = timelineEvents
        , agentLogs = logs
        , testCommandDraft = Maybe.withDefault "" sanitizedDetail.testCommandOverride
        , timelineVisibleCount = normalizedTimeline
        , agentLogVisible = updatedVisibility
        , timelineNextCursor = detail.eventNextCursor
        , historyState = Idle
        , historyRequest = Nothing
    }


type alias WorkerStatus =
    { id : Int
    , state : WorkerState
    }


type WorkerState
    = WorkerIdle Posix
    | WorkerRunning WorkerRunInfo


type alias WorkerMetric =
    { id : Int
    , currentTaskId : Maybe Int
    , currentStep : Maybe WorkflowStep
    , startedAt : Maybe Posix
    , lastTaskId : Maybe Int
    , lastStep : Maybe WorkflowStep
    , lastDurationSeconds : Maybe Float
    , lastSuccess : Maybe Bool
    , lastError : Maybe String
    , totalAssignments : Int
    , totalBusySeconds : Float
    }


type alias WorkerRunInfo =
    { taskId : Int
    , taskTitle : Maybe String
    , step : WorkflowStep
    , startedAt : Posix
    }


type alias Settings =
    { repoRoot : String
    , branch : String
    , testCommand : String
    , previewCommand : Maybe String
    , inactivityMinutes : Int
    , updatedAt : Posix
    }


type alias TaskSnoozeStatus =
    { snoozeUntil : Maybe Posix
    }


type alias PromptTemplate =
    { key : String
    , description : String
    , content : String
    , version : Int
    , updatedAt : Posix
    , isCustom : Bool
    }


type alias PreviewPingResponse =
    { status : PreviewStatus
    }


andMap : Decoder a -> Decoder (a -> b) -> Decoder b
andMap valueDecoder funcDecoder =
    Decode.map2 (\value func -> func value) valueDecoder funcDecoder


fromApiTaskSummary : Api.TaskSummary -> TaskSummary
fromApiTaskSummary summary =
    { id = summary.taskSummaryId
    , title = summary.taskSummaryTitle
    , status = summary.taskSummaryStatus
    , repoRoot = summary.taskSummaryRepoRoot
    , branch = summary.taskSummaryBranch
    , featureBranch = summary.taskSummaryFeatureBranch
    , updatedAt = summary.taskSummaryUpdatedAt
    , previewUrl = summary.taskSummaryPreviewUrl
    , previewStatus = summary.taskSummaryPreviewStatus
    , isPaused = summary.taskSummaryIsPaused
    , snoozeUntil = summary.taskSummarySnoozeUntil
    }


fromApiTaskRun : Api.TaskRunInfo -> TaskRun
fromApiTaskRun info =
    { ordinal = info.taskRunOrdinal
    , step = info.taskRunCurrentStep
    , summary = info.taskRunPmSummary
    , createdAt = info.taskRunCreatedAt
    , updatedAt = info.taskRunUpdatedAt
    }


fromApiStatusEvent : Api.StatusEventDTO -> StatusEvent
fromApiStatusEvent dto =
    { id = dto.statusEventId
    , step = dto.statusEventStep
    , message = dto.statusEventMessage
    , createdAt = dto.statusEventCreatedAt
    , payload = dto.statusEventPayload
    }


fromApiArtifact : Api.ArtifactDTO -> Artifact
fromApiArtifact artifact =
    { kind = artifact.artifactKind
    , label = artifact.artifactLabel
    , body = artifact.artifactBody
    , path = artifact.artifactPath
    , createdAt = artifact.artifactCreatedAt
    }


fromApiTaskDetail : Api.TaskDetail -> TaskDetail
fromApiTaskDetail detail =
    { summary = fromApiTaskSummary detail.taskDetailSummary
    , runs = List.map fromApiTaskRun detail.taskDetailRuns
    , events = List.map fromApiStatusEvent detail.taskDetailEvents
    , artifacts = List.map fromApiArtifact detail.taskDetailArtifacts
    , testCommand = detail.taskDetailTestCommand
    , testCommandOverride = detail.taskDetailTestCommandOverride
    , isPaused = detail.taskDetailIsPaused
    , snoozeUntil = detail.taskDetailSnoozeUntil
    , eventNextCursor = detail.taskDetailEventNextCursor
    }


fromApiWorkerStatus : Api.WorkerStatusDTO -> WorkerStatus
fromApiWorkerStatus dto =
    { id = dto.workerStatusId
    , state =
        case dto.workerStatusState of
            WorkerStatusIdle idle ->
                WorkerIdle idle.workerStatusIdleSince

            WorkerStatusRunning running ->
                WorkerRunning
                    { taskId = running.workerStatusTaskId
                    , taskTitle = running.workerStatusTaskTitle
                    , step = running.workerStatusStep
                    , startedAt = running.workerStatusStartedAt
                    }
    }


fromApiWorkerMetric : WorkerMetricDTO -> WorkerMetric
fromApiWorkerMetric dto =
    { id = dto.workerMetricId
    , currentTaskId = dto.workerMetricCurrentTaskId
    , currentStep = dto.workerMetricCurrentStep
    , startedAt = dto.workerMetricStartedAt
    , lastTaskId = dto.workerMetricLastTaskId
    , lastStep = dto.workerMetricLastStep
    , lastDurationSeconds = dto.workerMetricLastDurationSeconds
    , lastSuccess = dto.workerMetricLastSuccess
    , lastError = dto.workerMetricLastError
    , totalAssignments = dto.workerMetricTotalAssignments
    , totalBusySeconds = dto.workerMetricTotalBusySeconds
    }


fromApiTaskSnoozeStatus : Api.TaskSnoozeStatus -> TaskSnoozeStatus
fromApiTaskSnoozeStatus dto =
    { snoozeUntil = dto.taskSnoozeUntil
    }


fromApiTaskRedirectResponse : Api.TaskRedirectResponse -> TaskRedirectResponse
fromApiTaskRedirectResponse dto =
    { requeuedStep = dto.taskRedirectRequeuedStep
    , targetTaskId = dto.taskRedirectTargetTaskId
    , targetStep = dto.taskRedirectTargetStep
    , workerId = dto.taskRedirectWorkerId
    }


fromApiPromptTemplate : Api.PromptTemplateDTO -> PromptTemplate
fromApiPromptTemplate template =
    { key = template.promptTemplateKey
    , description = template.promptTemplateDescription
    , content = template.promptTemplateContent
    , version = template.promptTemplateVersion
    , updatedAt = template.promptTemplateUpdatedAt
    , isCustom = template.promptTemplateIsCustom
    }


fromApiSnapshot : Api.OrchestratorSnapshot -> OrchestratorSnapshot
fromApiSnapshot snapshot =
    { activeTasks = List.map fromApiTaskSummary snapshot.snapshotActiveTasks
    , queueDepth = snapshot.snapshotQueueDepth
    , workers = List.map fromApiWorkerStatus snapshot.snapshotWorkers
    , pausedTasks = snapshot.snapshotPausedTasks
    , workerMetrics = List.map fromApiWorkerMetric snapshot.snapshotWorkerMetrics
    }


formatTimestamp : Posix -> String
formatTimestamp posix =
    let
        year =
            String.fromInt (toYear utc posix)

        month =
            monthNumber (toMonth utc posix)

        day =
            pad2 (toDay utc posix)

        hour =
            pad2 (toHour utc posix)

        minute =
            pad2 (toMinute utc posix)

        second =
            pad2 (toSecond utc posix)
    in
    year ++ "-" ++ month ++ "-" ++ day ++ " " ++ hour ++ ":" ++ minute ++ ":" ++ second


pad2 : Int -> String
pad2 value =
    let
        raw =
            String.fromInt value
    in
    if String.length raw < 2 then
        "0" ++ raw

    else
        raw


monthNumber : Month -> String
monthNumber month =
    case month of
        Jan ->
            "01"

        Feb ->
            "02"

        Mar ->
            "03"

        Apr ->
            "04"

        May ->
            "05"

        Jun ->
            "06"

        Jul ->
            "07"

        Aug ->
            "08"

        Sep ->
            "09"

        Oct ->
            "10"

        Nov ->
            "11"

        Dec ->
            "12"


formatSeconds : Float -> String
formatSeconds seconds =
    if seconds < 60 then
        formatFloat 1 seconds ++ "s"

    else if seconds < 3600 then
        let
            minutes =
                seconds / 60
        in
        formatFloat 1 minutes ++ "m"

    else
        let
            hours =
                seconds / 3600
        in
        formatFloat 1 hours ++ "h"


formatFloat : Int -> Float -> String
formatFloat decimals value =
    let
        factor =
            toFloat (10 ^ decimals)

        rounded =
            toFloat (round (value * factor)) / factor

        raw =
            String.fromFloat rounded

        trimmed =
            trimTrailingZeros raw
    in
    if String.contains "NaN" trimmed || String.contains "Infinity" trimmed then
        raw

    else
        trimmed


trimTrailingZeros : String -> String
trimTrailingZeros str =
    if String.contains "." str then
        let
            withoutZeros =
                let
                    reversed =
                        String.reverse str
                in
                reversed
                    |> String.toList
                    |> dropWhileLeft ((==) '0')
                    |> String.fromList
                    |> String.reverse

            cleaned =
                if String.endsWith "." withoutZeros then
                    String.dropRight 1 withoutZeros

                else
                    withoutZeros
        in
        if String.isEmpty cleaned then
            "0"

        else
            cleaned

    else
        str


dropWhileLeft : (a -> Bool) -> List a -> List a
dropWhileLeft predicate list =
    case list of
        [] ->
            []

        x :: xs ->
            if predicate x then
                dropWhileLeft predicate xs

            else
                x :: xs


agentRoleLabel : AgentRole -> String
agentRoleLabel role =
    case role of
        AgentRoleProjectManager ->
            "Project Manager"

        AgentRoleImplementer ->
            "Implementation"

        AgentRoleQa ->
            "QA"


roleKey : AgentRole -> String
roleKey role =
    case role of
        AgentRoleProjectManager ->
            "project-manager"

        AgentRoleImplementer ->
            "implementer"

        AgentRoleQa ->
            "qa"


addAgentLog : AgentLogEntry -> AgentLogs -> AgentLogs
addAgentLog entry logs =
    Dict.update (roleKey entry.role)
        (\maybeLogs ->
            case maybeLogs of
                Just existing ->
                    Just (existing ++ [ entry ])

                Nothing ->
                    Just [ entry ]
        )
        logs


splitEvents : List StatusEvent -> ( List StatusEvent, AgentLogs )
splitEvents events =
    let
        reducer event ( timelineAcc, logsAcc ) =
            case agentLogFromEvent event of
                Just logEntry ->
                    ( timelineAcc, addAgentLog logEntry logsAcc )

                Nothing ->
                    ( event :: timelineAcc, logsAcc )

        ( timelineRev, logs ) =
            List.foldl reducer ( [], Dict.empty ) events
    in
    ( List.reverse timelineRev, logs )


agentLogFromEvent : StatusEvent -> Maybe AgentLogEntry
agentLogFromEvent event =
    case event.payload of
        Nothing ->
            Nothing

        Just value ->
            case Decode.decodeValue agentLogPayloadDecoder value of
                Ok payload ->
                    Just
                        { role = payload.role
                        , step = event.step
                        , stream = payload.stream
                        , line = payload.line
                        , createdAt = event.createdAt
                        }

                Err _ ->
                    Nothing


type alias AgentLogPayload =
    { role : AgentRole
    , stream : String
    , line : String
    }


agentLogPayloadDecoder : Decoder AgentLogPayload
agentLogPayloadDecoder =
    Decode.field "kind" Decode.string
        |> Decode.andThen
            (\kind ->
                if kind == "agent-log" then
                    Decode.map3 AgentLogPayload
                        (Decode.field "role" Api.jsonDecAgentRole)
                        (Decode.field "stream" Decode.string)
                        (Decode.field "line" Decode.string)

                else
                    Decode.fail "not an agent log"
            )



-- DECODERS ------------------------------------------------------------------


taskSummaryDecoder : Decoder TaskSummary
taskSummaryDecoder =
    Decode.succeed TaskSummary
        |> andMap (Decode.field "taskSummaryId" Decode.int)
        |> andMap (Decode.field "taskSummaryTitle" Decode.string)
        |> andMap (Decode.field "taskSummaryStatus" Api.jsonDecTaskStatus)
        |> andMap (Decode.field "taskSummaryRepoRoot" Decode.string)
        |> andMap (Decode.field "taskSummaryBranch" Decode.string)
        |> andMap (Decode.field "taskSummaryFeatureBranch" (Decode.nullable Decode.string))
        |> andMap (Decode.field "taskSummaryUpdatedAt" Api.jsonDecPosix)
        |> andMap (Decode.field "taskSummaryPreviewUrl" (Decode.nullable Decode.string))
        |> andMap (Decode.field "taskSummaryPreviewStatus" Api.jsonDecPreviewStatus)
        |> andMap (Decode.field "taskSummaryIsPaused" Decode.bool)
        |> andMap (Decode.field "taskSummarySnoozeUntil" (Decode.nullable Api.jsonDecPosix))


taskRunDecoder : Decoder TaskRun
taskRunDecoder =
    Decode.succeed TaskRun
        |> andMap (Decode.field "taskRunOrdinal" Decode.int)
        |> andMap (Decode.field "taskRunCurrentStep" Api.jsonDecWorkflowStep)
        |> andMap (Decode.field "taskRunPmSummary" (Decode.nullable Decode.string))
        |> andMap (Decode.field "taskRunCreatedAt" Api.jsonDecPosix)
        |> andMap (Decode.field "taskRunUpdatedAt" Api.jsonDecPosix)


statusEventDecoder : Decoder StatusEvent
statusEventDecoder =
    Decode.succeed StatusEvent
        |> andMap (Decode.field "statusEventId" (Decode.nullable Decode.int))
        |> andMap (Decode.field "statusEventStep" Api.jsonDecWorkflowStep)
        |> andMap (Decode.field "statusEventMessage" Decode.string)
        |> andMap (Decode.field "statusEventCreatedAt" Api.jsonDecPosix)
        |> andMap (Decode.field "statusEventPayload" (Decode.nullable Api.jsonDecValue))


artifactDecoder : Decoder Artifact
artifactDecoder =
    Decode.succeed Artifact
        |> andMap (Decode.field "artifactKind" Api.jsonDecArtifactKind)
        |> andMap (Decode.field "artifactLabel" Decode.string)
        |> andMap (Decode.field "artifactBody" (Decode.nullable Api.jsonDecValue))
        |> andMap (Decode.field "artifactPath" (Decode.nullable Decode.string))
        |> andMap (Decode.field "artifactCreatedAt" Api.jsonDecPosix)


taskDetailDecoder : Decoder TaskDetail
taskDetailDecoder =
    Decode.succeed TaskDetail
        |> andMap (Decode.field "taskDetailSummary" taskSummaryDecoder)
        |> andMap (Decode.field "taskDetailRuns" (Decode.list taskRunDecoder))
        |> andMap (Decode.field "taskDetailEvents" (Decode.list statusEventDecoder))
        |> andMap (Decode.field "taskDetailArtifacts" (Decode.list artifactDecoder))
        |> andMap (Decode.field "taskDetailTestCommand" Decode.string)
        |> andMap (Decode.field "taskDetailTestCommandOverride" (Decode.nullable Decode.string))
        |> andMap (Decode.field "taskDetailIsPaused" Decode.bool)
        |> andMap (Decode.field "taskDetailSnoozeUntil" (Decode.nullable Api.jsonDecPosix))
        |> andMap (Decode.field "taskDetailEventNextCursor" (Decode.nullable Decode.int))


taskHistoryDecoder : Decoder TaskHistoryPage
taskHistoryDecoder =
    Decode.succeed TaskHistoryPage
        |> andMap (Decode.field "taskHistoryEvents" (Decode.list statusEventDecoder))
        |> andMap (Decode.field "taskHistoryNextCursor" (Decode.nullable Decode.int))


workerStatusDecoder : Decoder WorkerStatus
workerStatusDecoder =
    Decode.map fromApiWorkerStatus Api.jsonDecWorkerStatusDTO


workerMetricDecoder : Decoder WorkerMetric
workerMetricDecoder =
    Decode.map fromApiWorkerMetric Api.jsonDecWorkerMetricDTO


taskSnoozeStatusDecoder : Decoder TaskSnoozeStatus
taskSnoozeStatusDecoder =
    Decode.map fromApiTaskSnoozeStatus Api.jsonDecTaskSnoozeStatus


taskRedirectResponseDecoder : Decoder TaskRedirectResponse
taskRedirectResponseDecoder =
    Decode.map fromApiTaskRedirectResponse Api.jsonDecTaskRedirectResponse


snapshotDecoder : Decoder OrchestratorSnapshot
snapshotDecoder =
    Decode.succeed OrchestratorSnapshot
        |> andMap (Decode.field "snapshotActiveTasks" (Decode.list taskSummaryDecoder))
        |> andMap (Decode.field "snapshotQueueDepth" Decode.int)
        |> andMap
            (Decode.maybe (Decode.field "snapshotWorkers" (Decode.list workerStatusDecoder))
                |> Decode.map (Maybe.withDefault [])
            )
        |> andMap
            (Decode.maybe (Decode.field "snapshotPausedTasks" (Decode.list Decode.int))
                |> Decode.map (Maybe.withDefault [])
            )
        |> andMap
            (Decode.maybe (Decode.field "snapshotWorkerMetrics" (Decode.list workerMetricDecoder))
                |> Decode.map (Maybe.withDefault [])
            )


settingsDecoder : Decoder Settings
settingsDecoder =
    Decode.map6 Settings
        (Decode.field "settingsDefaultRepoRoot" Decode.string)
        (Decode.field "settingsDefaultBranch" Decode.string)
        (Decode.field "settingsTestCommand" Decode.string)
        (Decode.field "settingsPreviewCommand" (Decode.nullable Decode.string))
        (Decode.field "settingsInactivityMinutes" Decode.int)
        (Decode.field "settingsUpdatedAt" Api.jsonDecPosix)


promptTemplateDecoder : Decoder PromptTemplate
promptTemplateDecoder =
    Decode.map fromApiPromptTemplate Api.jsonDecPromptTemplateDTO


previewPingDecoder : Decoder PreviewPingResponse
previewPingDecoder =
    Decode.map PreviewPingResponse (Decode.field "status" Api.jsonDecPreviewStatus)


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
        base =
            normalizeBase flags.backendBase

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
            , notifications = []
            , nextNotificationId = 0
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
    | InsertMessageTemplate String
    | UpdateMessageRole String
    | ToggleMessageAutoResume Bool
    | SubmitMessage
    | MessageSent (Result Http.Error ())
    | UpdateTaskStatus TaskStatus
    | TaskStatusUpdated (Result Http.Error TaskDetail)
    | TriggerPreviewPing
    | PreviewPinged (Result Http.Error PreviewPingResponse)
    | CancelTask
    | TaskCancelled (Result Http.Error TaskDetail)
    | ForceRetry
    | ForceRetryResult (Result Http.Error WorkflowStep)
    | PauseCurrentTask
    | PauseResult (Result Http.Error Bool)
    | ResumeCurrentTask
    | ResumeResult (Result Http.Error Bool)
    | ReassignWorker
    | ReassignResult (Result Http.Error WorkflowStep)
    | UpdateRedirectTarget String
    | RedirectWorkerToTask
    | RedirectResult (Result Http.Error TaskRedirectResponse)
    | UpdateSnoozeDraft String
    | SubmitSnooze
    | SnoozeResult (Result Http.Error TaskSnoozeStatus)
    | CancelSnooze
    | SnoozeCancelled (Result Http.Error TaskSnoozeStatus)
    | UpdateTestCommandDraft String
    | SaveTestCommand
    | ClearTestCommand
    | TestCommandSaved (Result Http.Error TaskDetail)
    | SkipQa
    | QaSkipped (Result Http.Error TaskDetail)
    | StartPreview
    | PreviewStarted (Result Http.Error TaskDetail)
    | ReceiveStatusEvent Decode.Value
    | ShowMoreTimeline
    | CollapseTimeline
    | ShowMoreAgentLog AgentRole
    | CollapseAgentLog AgentRole
    | HistoryLoaded (Result Http.Error TaskHistoryPage)
    | SelectDetailTab DetailTab
    | SnapshotPoll
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
    | DismissNotification Int



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

        DismissNotification notificationId ->
            ( { model | notifications = List.filter (\n -> n.id /= notificationId) model.notifications }, Cmd.none )

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

                        newModel =
                            { model | page = DashboardPage updated }
                    in
                    ( newModel, scheduleSnapshot newModel )

                TaskDetailPage detailModel ->
                    case result of
                        Ok snapshot ->
                            let
                                candidates =
                                    snapshot.activeTasks
                                        |> List.filter (\summary -> summary.id /= detailModel.id)

                                nextTarget =
                                    case detailModel.redirectTarget of
                                        Just existing ->
                                            Just existing

                                        Nothing ->
                                            candidates
                                                |> List.head
                                                |> Maybe.map .id

                                updatedDetail =
                                    { detailModel
                                        | snapshot = Success snapshot
                                        , redirectTarget = nextTarget
                                    }
                            in
                            ( { model | page = TaskDetailPage updatedDetail }, Cmd.none )

                        Err err ->
                            let
                                updatedDetail =
                                    { detailModel | snapshot = Failure (httpErrorToString err) }
                            in
                            ( { model | page = TaskDetailPage updatedDetail }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        UpdateCreateTitle str ->
            updateDashboardForm model
                (\form dash ->
                    { dash | form = { form | title = str } }
                )

        UpdateCreateDescription str ->
            updateDashboardForm model
                (\form dash ->
                    { dash | form = { form | description = str } }
                )

        UpdateCreateRepo str ->
            updateDashboardForm model
                (\form dash ->
                    { dash | form = { form | repoRoot = str } }
                )

        UpdateCreateBranch str ->
            updateDashboardForm model
                (\form dash ->
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
                                    hydrated =
                                        hydrateDetailModel detailModel detail

                                    newModel =
                                        { hydrated
                                            | testCommandState = Idle
                                            , qaSkipState = Idle
                                            , previewStartState = Idle
                                            , retryState = Idle
                                            , pauseState = Idle
                                            , reassignState = Idle
                                            , snoozeState = Idle
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

        InsertMessageTemplate template ->
            case model.page of
                TaskDetailPage detailModel ->
                    if detailModel.messageState == Working then
                        ( model, Cmd.none )

                    else
                        let
                            appended =
                                if String.trim detailModel.message == "" then
                                    template

                                else
                                    detailModel.message ++ "\n\n" ++ template
                        in
                        ( { model | page = TaskDetailPage { detailModel | message = appended, messageState = Idle } }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        UpdateMessageRole roleStr ->
            case model.page of
                TaskDetailPage detailModel ->
                    let
                        newRole =
                            messageRoleFromString roleStr
                    in
                    ( { model | page = TaskDetailPage { detailModel | messageTargetRole = newRole } }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        ToggleMessageAutoResume flag ->
            case model.page of
                TaskDetailPage detailModel ->
                    ( { model | page = TaskDetailPage { detailModel | messageAutoResume = flag } }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        SubmitMessage ->
            case model.page of
                TaskDetailPage detailModel ->
                    if String.trim detailModel.message == "" || detailModel.messageState == Working then
                        ( model, Cmd.none )

                    else
                        ( { model | page = TaskDetailPage { detailModel | messageState = Working } }
                        , sendTaskMessage model detailModel detailModel.message
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
                            let
                                hydrated =
                                    hydrateDetailModel detailModel detail

                                updatedModel =
                                    { hydrated
                                        | statusState = Completed
                                        , testCommandState = Idle
                                        , qaSkipState = Idle
                                        , previewStartState = Idle
                                    }
                            in
                            ( { model | page = TaskDetailPage updatedModel }, Cmd.none )

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
                                summary =
                                    detail.summary

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
                            let
                                hydrated =
                                    hydrateDetailModel detailModel detail

                                updatedModel =
                                    { hydrated
                                        | cancelState = Completed
                                        , testCommandState = Idle
                                        , qaSkipState = Idle
                                        , previewStartState = Idle
                                        , retryState = Idle
                                        , pauseState = Idle
                                        , reassignState = Idle
                                    }
                            in
                            ( { model | page = TaskDetailPage updatedModel }, Cmd.none )

                        Err err ->
                            ( { model | page = TaskDetailPage { detailModel | cancelState = Failed (httpErrorToString err) } }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        ForceRetry ->
            case model.page of
                TaskDetailPage detailModel ->
                    let
                        maybeInstructions =
                            let
                                trimmed =
                                    String.trim detailModel.message
                            in
                            if trimmed == "" then
                                Nothing

                            else
                                Just trimmed
                    in
                    ( { model | page = TaskDetailPage { detailModel | retryState = Working } }
                    , forceRetryRequest model detailModel.id maybeInstructions
                    )

                _ ->
                    ( model, Cmd.none )

        ForceRetryResult result ->
            case model.page of
                TaskDetailPage detailModel ->
                    case result of
                        Ok _ ->
                            ( { model | page = TaskDetailPage { detailModel | retryState = Completed, messageState = Idle, message = "" } }
                            , fetchTaskDetail model detailModel.id
                            )

                        Err err ->
                            ( { model | page = TaskDetailPage { detailModel | retryState = Failed (httpErrorToString err) } }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        PauseCurrentTask ->
            case model.page of
                TaskDetailPage detailModel ->
                    ( { model | page = TaskDetailPage { detailModel | pauseState = Working } }
                    , pauseTaskRequest model detailModel.id
                    )

                _ ->
                    ( model, Cmd.none )

        PauseResult result ->
            case model.page of
                TaskDetailPage detailModel ->
                    case result of
                        Ok _ ->
                            let
                                updatedDetail =
                                    case detailModel.detail of
                                        Success info ->
                                            let
                                                summary =
                                                    info.summary

                                                updatedSummary =
                                                    { summary | isPaused = True }
                                            in
                                            Success
                                                { info
                                                    | summary = updatedSummary
                                                    , isPaused = True
                                                }

                                        other ->
                                            other
                            in
                            ( { model | page = TaskDetailPage { detailModel | detail = updatedDetail, pauseState = Completed } }
                            , Cmd.none
                            )

                        Err err ->
                            ( { model | page = TaskDetailPage { detailModel | pauseState = Failed (httpErrorToString err) } }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        ResumeCurrentTask ->
            case model.page of
                TaskDetailPage detailModel ->
                    ( { model | page = TaskDetailPage { detailModel | pauseState = Working } }
                    , resumeTaskRequest model detailModel.id
                    )

                _ ->
                    ( model, Cmd.none )

        ResumeResult result ->
            case model.page of
                TaskDetailPage detailModel ->
                    case result of
                        Ok _ ->
                            let
                                updatedDetail =
                                    case detailModel.detail of
                                        Success info ->
                                            let
                                                summary =
                                                    info.summary

                                                updatedSummary =
                                                    { summary | isPaused = False }
                                            in
                                            Success
                                                { info
                                                    | summary = updatedSummary
                                                    , isPaused = False
                                                }

                                        other ->
                                            other
                            in
                            ( { model | page = TaskDetailPage { detailModel | detail = updatedDetail, pauseState = Completed } }
                            , Cmd.none
                            )

                        Err err ->
                            ( { model | page = TaskDetailPage { detailModel | pauseState = Failed (httpErrorToString err) } }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        ReassignWorker ->
            case model.page of
                TaskDetailPage detailModel ->
                    ( { model | page = TaskDetailPage { detailModel | reassignState = Working } }
                    , reassignTaskRequest model detailModel.id
                    )

                _ ->
                    ( model, Cmd.none )

        ReassignResult result ->
            case model.page of
                TaskDetailPage detailModel ->
                    case result of
                        Ok _ ->
                            ( { model | page = TaskDetailPage { detailModel | reassignState = Completed } }
                            , fetchTaskDetail model detailModel.id
                            )

                        Err err ->
                            ( { model | page = TaskDetailPage { detailModel | reassignState = Failed (httpErrorToString err) } }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        UpdateRedirectTarget str ->
            case model.page of
                TaskDetailPage detailModel ->
                    let
                        trimmed =
                            String.trim str

                        parsed =
                            if trimmed == "" then
                                Nothing

                            else
                                String.toInt trimmed
                    in
                    ( { model | page = TaskDetailPage { detailModel | redirectTarget = parsed } }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        RedirectWorkerToTask ->
            case model.page of
                TaskDetailPage detailModel ->
                    case detailModel.redirectTarget of
                        Nothing ->
                            ( model, Cmd.none )

                        Just targetId ->
                            if detailModel.redirectState == Working then
                                ( model, Cmd.none )

                            else
                                ( { model | page = TaskDetailPage { detailModel | redirectState = Working } }
                                , redirectWorkerRequest model detailModel.id targetId
                                )

                _ ->
                    ( model, Cmd.none )

        RedirectResult result ->
            case model.page of
                TaskDetailPage detailModel ->
                    case result of
                        Ok response ->
                            let
                                updatedModel =
                                    { detailModel
                                        | redirectState = Completed
                                        , redirectTarget = response.targetTaskId
                                        , snapshot = Loading
                                    }
                            in
                            ( { model | page = TaskDetailPage updatedModel }
                            , Cmd.batch
                                [ fetchTaskDetail model detailModel.id
                                , fetchSnapshot model
                                ]
                            )

                        Err err ->
                            ( { model | page = TaskDetailPage { detailModel | redirectState = Failed (httpErrorToString err) } }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        UpdateSnoozeDraft str ->
            case model.page of
                TaskDetailPage detailModel ->
                    let
                        nextState =
                            case detailModel.snoozeState of
                                Working ->
                                    Working

                                _ ->
                                    Idle
                    in
                    ( { model | page = TaskDetailPage { detailModel | snoozeMinutesDraft = str, snoozeState = nextState } }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        SubmitSnooze ->
            case model.page of
                TaskDetailPage detailModel ->
                    if detailModel.snoozeState == Working then
                        ( model, Cmd.none )

                    else
                        let
                            trimmed =
                                String.trim detailModel.snoozeMinutesDraft
                        in
                        case String.toInt trimmed of
                            Just minutes ->
                                if minutes <= 0 then
                                    ( { model | page = TaskDetailPage { detailModel | snoozeState = Failed "Enter minutes greater than zero" } }, Cmd.none )

                                else
                                    ( { model | page = TaskDetailPage { detailModel | snoozeState = Working } }
                                    , scheduleSnoozeRequest model detailModel.id minutes
                                    )

                            Nothing ->
                                ( { model | page = TaskDetailPage { detailModel | snoozeState = Failed "Enter a valid number" } }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        SnoozeResult result ->
            case model.page of
                TaskDetailPage detailModel ->
                    case result of
                        Ok status ->
                            let
                                noteMessage =
                                    case status.snoozeUntil of
                                        Just ts ->
                                            "Snooze scheduled until " ++ formatTimestamp ts

                                        Nothing ->
                                            "Snooze scheduled"

                                modelNotified =
                                    addNotification noteMessage model

                                updatedDetail =
                                    { detailModel | snoozeState = Completed, snapshot = Loading }
                            in
                            ( { modelNotified | page = TaskDetailPage updatedDetail }
                            , Cmd.batch
                                [ fetchTaskDetail model detailModel.id
                                , fetchSnapshot model
                                ]
                            )

                        Err err ->
                            ( { model | page = TaskDetailPage { detailModel | snoozeState = Failed (httpErrorToString err) } }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        CancelSnooze ->
            case model.page of
                TaskDetailPage detailModel ->
                    let
                        hasActiveSnooze =
                            case detailModel.detail of
                                Success detail ->
                                    Maybe.withDefault False (Maybe.map (\_ -> True) detail.snoozeUntil)

                                _ ->
                                    False
                    in
                    if detailModel.snoozeState == Working || not hasActiveSnooze then
                        ( model, Cmd.none )

                    else
                        ( { model | page = TaskDetailPage { detailModel | snoozeState = Working } }
                        , cancelSnoozeRequest model detailModel.id
                        )

                _ ->
                    ( model, Cmd.none )

        SnoozeCancelled result ->
            case model.page of
                TaskDetailPage detailModel ->
                    case result of
                        Ok _ ->
                            let
                                modelNotified =
                                    addNotification "Snooze cancelled" model

                                updatedDetail =
                                    { detailModel
                                        | snoozeState = Completed
                                        , snoozeMinutesDraft = "30"
                                        , snapshot = Loading
                                    }
                            in
                            ( { modelNotified | page = TaskDetailPage updatedDetail }
                            , Cmd.batch
                                [ fetchTaskDetail model detailModel.id
                                , fetchSnapshot model
                                ]
                            )

                        Err err ->
                            ( { model | page = TaskDetailPage { detailModel | snoozeState = Failed (httpErrorToString err) } }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        UpdateTestCommandDraft str ->
            case model.page of
                TaskDetailPage detailModel ->
                    let
                        nextState =
                            case detailModel.testCommandState of
                                Working ->
                                    Working

                                _ ->
                                    Idle
                    in
                    ( { model | page = TaskDetailPage { detailModel | testCommandDraft = str, testCommandState = nextState } }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        SaveTestCommand ->
            case model.page of
                TaskDetailPage detailModel ->
                    case detailModel.detail of
                        Success detail ->
                            if detailModel.testCommandState == Working then
                                ( model, Cmd.none )

                            else
                                let
                                    trimmed =
                                        String.trim detailModel.testCommandDraft

                                    desired =
                                        if trimmed == "" then
                                            Nothing

                                        else
                                            Just trimmed
                                in
                                if desired == detail.testCommandOverride then
                                    ( { model | page = TaskDetailPage { detailModel | testCommandState = Idle } }, Cmd.none )

                                else
                                    ( { model | page = TaskDetailPage { detailModel | testCommandState = Working } }
                                    , updateTaskTestCommand model detailModel.id desired
                                    )

                        _ ->
                            ( model, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        ClearTestCommand ->
            case model.page of
                TaskDetailPage detailModel ->
                    case detailModel.detail of
                        Success detail ->
                            if detailModel.testCommandState == Working then
                                ( model, Cmd.none )

                            else if detail.testCommandOverride == Nothing then
                                ( { model | page = TaskDetailPage { detailModel | testCommandDraft = "", testCommandState = Idle } }, Cmd.none )

                            else
                                ( { model
                                    | page =
                                        TaskDetailPage
                                            { detailModel
                                                | testCommandDraft = ""
                                                , testCommandState = Working
                                            }
                                  }
                                , updateTaskTestCommand model detailModel.id Nothing
                                )

                        _ ->
                            ( model, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        TestCommandSaved result ->
            case model.page of
                TaskDetailPage detailModel ->
                    case result of
                        Ok detail ->
                            let
                                hydrated =
                                    hydrateDetailModel detailModel detail

                                updatedModel =
                                    { hydrated
                                        | testCommandState = Completed
                                        , qaSkipState = Idle
                                        , previewStartState = Idle
                                    }
                            in
                            ( { model | page = TaskDetailPage updatedModel }, Cmd.none )

                        Err err ->
                            ( { model | page = TaskDetailPage { detailModel | testCommandState = Failed (httpErrorToString err) } }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        SkipQa ->
            case model.page of
                TaskDetailPage detailModel ->
                    if detailModel.qaSkipState == Working then
                        ( model, Cmd.none )

                    else
                        ( { model | page = TaskDetailPage { detailModel | qaSkipState = Working } }
                        , skipQa model detailModel.id
                        )

                _ ->
                    ( model, Cmd.none )

        QaSkipped result ->
            case model.page of
                TaskDetailPage detailModel ->
                    case result of
                        Ok detail ->
                            let
                                hydrated =
                                    hydrateDetailModel detailModel detail

                                updatedModel =
                                    { hydrated
                                        | testCommandState = Idle
                                        , qaSkipState = Completed
                                        , previewStartState = Idle
                                    }
                            in
                            ( { model | page = TaskDetailPage updatedModel }, Cmd.none )

                        Err err ->
                            ( { model | page = TaskDetailPage { detailModel | qaSkipState = Failed (httpErrorToString err) } }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        StartPreview ->
            case model.page of
                TaskDetailPage detailModel ->
                    if detailModel.previewStartState == Working then
                        ( model, Cmd.none )

                    else
                        ( { model | page = TaskDetailPage { detailModel | previewStartState = Working } }
                        , startPreviewRequest model detailModel.id
                        )

                _ ->
                    ( model, Cmd.none )

        PreviewStarted result ->
            case model.page of
                TaskDetailPage detailModel ->
                    case result of
                        Ok detail ->
                            let
                                hydrated =
                                    hydrateDetailModel detailModel detail

                                updatedModel =
                                    { hydrated
                                        | previewStartState = Completed
                                        , qaSkipState = Idle
                                        , testCommandState = Idle
                                    }
                            in
                            ( { model | page = TaskDetailPage updatedModel }, Cmd.none )

                        Err err ->
                            ( { model | page = TaskDetailPage { detailModel | previewStartState = Failed (httpErrorToString err) } }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        ReceiveStatusEvent value ->
            case Decode.decodeValue ssePayloadDecoder value of
                Ok payload ->
                    let
                        baseModel =
                            maybeAddNotification payload.event model
                    in
                    case baseModel.page of
                        TaskDetailPage detailModel ->
                            if payload.taskId /= detailModel.id then
                                ( baseModel, Cmd.none )

                            else
                                case agentLogFromEvent payload.event of
                                    Just logEntry ->
                                        let
                                            updatedLogs =
                                                addAgentLog logEntry detailModel.agentLogs

                                            updatedVisibility =
                                                updateLogVisibility updatedLogs detailModel.agentLogVisible

                                            updatedDetailModel =
                                                { detailModel
                                                    | agentLogs = updatedLogs
                                                    , agentLogVisible = updatedVisibility
                                                }
                                        in
                                        ( { baseModel | page = TaskDetailPage updatedDetailModel }, Cmd.none )

                                    Nothing ->
                                        let
                                            updatedDetail =
                                                remoteDataMap
                                                    (\d -> { d | events = payload.event :: d.events })
                                                    detailModel.detail

                                            updatedEvents =
                                                payload.event :: detailModel.events

                                            normalizedCount =
                                                normalizeTimelineCount detailModel.timelineVisibleCount updatedEvents

                                            updatedModel =
                                                { detailModel
                                                    | events = updatedEvents
                                                    , detail = updatedDetail
                                                    , timelineVisibleCount = normalizedCount
                                                }
                                        in
                                        ( { baseModel | page = TaskDetailPage updatedModel }, Cmd.none )

                        _ ->
                            ( baseModel, Cmd.none )

                Err _ ->
                    ( model, Cmd.none )

        ShowMoreTimeline ->
            case model.page of
                TaskDetailPage detailModel ->
                    let
                        total =
                            List.length detailModel.events

                        hasHidden =
                            detailModel.timelineVisibleCount < total

                        canFetchMore =
                            detailModel.timelineNextCursor /= Nothing

                        alreadyLoading =
                            detailModel.historyState == Working
                    in
                    if hasHidden then
                        let
                            desired =
                                detailModel.timelineVisibleCount + timelineChunkSize

                            newCount =
                                normalizeTimelineCount desired detailModel.events

                            updated =
                                { detailModel | timelineVisibleCount = newCount }
                        in
                        ( { model | page = TaskDetailPage updated }, Cmd.none )

                    else if canFetchMore && not alreadyLoading then
                        let
                            updated =
                                { detailModel
                                    | historyState = Working
                                    , historyRequest = Just HistoryFromTimeline
                                }
                        in
                        ( { model | page = TaskDetailPage updated }
                        , loadMoreHistory model detailModel
                        )

                    else
                        ( model, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        CollapseTimeline ->
            case model.page of
                TaskDetailPage detailModel ->
                    let
                        baseline =
                            timelineBaselineCount detailModel.events

                        updated =
                            { detailModel | timelineVisibleCount = baseline }
                    in
                    ( { model | page = TaskDetailPage updated }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        ShowMoreAgentLog role ->
            case model.page of
                TaskDetailPage detailModel ->
                    let
                        key =
                            roleKey role

                        entries =
                            Dict.get key detailModel.agentLogs |> Maybe.withDefault []

                        currentLimit =
                            agentLogLimit detailModel.agentLogVisible key entries

                        shownCount =
                            List.length (takeNewest currentLimit entries)

                        total =
                            List.length entries

                        hidden =
                            Basics.max 0 (total - shownCount)

                        alreadyLoading =
                            detailModel.historyState == Working
                    in
                    if hidden > 0 then
                        let
                            desired =
                                currentLimit + logChunkSize

                            newLimit =
                                normalizeLogCount desired entries

                            updatedVisibility =
                                Dict.insert key newLimit detailModel.agentLogVisible

                            updated =
                                { detailModel | agentLogVisible = updatedVisibility }
                        in
                        ( { model | page = TaskDetailPage updated }, Cmd.none )

                    else if detailModel.timelineNextCursor /= Nothing && not alreadyLoading then
                        let
                            updated =
                                { detailModel
                                    | historyState = Working
                                    , historyRequest = Just (HistoryFromLog role)
                                }
                        in
                        ( { model | page = TaskDetailPage updated }
                        , loadMoreHistory model detailModel
                        )

                    else
                        ( model, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        CollapseAgentLog role ->
            case model.page of
                TaskDetailPage detailModel ->
                    let
                        key =
                            roleKey role

                        entries =
                            Dict.get key detailModel.agentLogs |> Maybe.withDefault []

                        baseline =
                            logBaselineCount entries

                        updatedVisibility =
                            Dict.insert key baseline detailModel.agentLogVisible

                        updated =
                            { detailModel | agentLogVisible = updatedVisibility }
                    in
                    ( { model | page = TaskDetailPage updated }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        SelectDetailTab tab ->
            case model.page of
                TaskDetailPage detailModel ->
                    if detailModel.selectedTab == tab then
                        ( model, Cmd.none )

                    else
                        let
                            resetHistoryState =
                                case detailModel.historyState of
                                    Failed _ ->
                                        Idle

                                    otherState ->
                                        otherState

                            updated =
                                { detailModel
                                    | selectedTab = tab
                                    , historyState = resetHistoryState
                                    , historyRequest = Nothing
                                }
                        in
                        ( { model | page = TaskDetailPage updated }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        SnapshotPoll ->
            case model.page of
                DashboardPage _ ->
                    ( model, fetchSnapshot model )

                _ ->
                    ( model, Cmd.none )

        HistoryLoaded result ->
            case model.page of
                TaskDetailPage detailModel ->
                    case result of
                        Ok page ->
                            let
                                ( chunkTimeline, chunkLogs ) =
                                    splitEvents page.events

                                mergedEvents =
                                    detailModel.events ++ chunkTimeline

                                mergedLogs =
                                    mergeAgentLogs chunkLogs detailModel.agentLogs

                                baseVisibility =
                                    updateLogVisibility mergedLogs detailModel.agentLogVisible

                                adjustedVisibility =
                                    case detailModel.historyRequest of
                                        Just (HistoryFromLog role) ->
                                            let
                                                key =
                                                    roleKey role

                                                entries =
                                                    Dict.get key mergedLogs |> Maybe.withDefault []

                                                current =
                                                    Dict.get key baseVisibility |> Maybe.withDefault (logBaselineCount entries)

                                                desired =
                                                    current + logChunkSize

                                                newLimit =
                                                    normalizeLogCount desired entries
                                            in
                                            Dict.insert key newLimit baseVisibility

                                        _ ->
                                            baseVisibility

                                timelineTarget =
                                    case detailModel.historyRequest of
                                        Just HistoryFromTimeline ->
                                            normalizeTimelineCount (detailModel.timelineVisibleCount + List.length chunkTimeline) mergedEvents

                                        _ ->
                                            normalizeTimelineCount detailModel.timelineVisibleCount mergedEvents

                                updatedDetail =
                                    remoteDataMap
                                        (\d ->
                                            { d
                                                | events = d.events ++ chunkTimeline
                                                , eventNextCursor = page.nextCursor
                                            }
                                        )
                                        detailModel.detail

                                updatedModel =
                                    { detailModel
                                        | events = mergedEvents
                                        , agentLogs = mergedLogs
                                        , agentLogVisible = adjustedVisibility
                                        , timelineVisibleCount = timelineTarget
                                        , timelineNextCursor = page.nextCursor
                                        , historyState = Idle
                                        , historyRequest = Nothing
                                        , detail = updatedDetail
                                    }
                            in
                            ( { model | page = TaskDetailPage updatedModel }, Cmd.none )

                        Err err ->
                            let
                                updated =
                                    { detailModel
                                        | historyState = Failed (httpErrorToString err)
                                        , historyRequest = Nothing
                                    }
                            in
                            ( { model | page = TaskDetailPage updated }, Cmd.none )

                _ ->
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
            modifySettingsDraft model
                (\draft settingsModel ->
                    { settingsModel | draft = { draft | repoRoot = str } }
                )

        UpdateSettingsBranch str ->
            modifySettingsDraft model
                (\draft settingsModel ->
                    { settingsModel | draft = { draft | branch = str } }
                )

        UpdateSettingsTestCommand str ->
            modifySettingsDraft model
                (\draft settingsModel ->
                    { settingsModel | draft = { draft | testCommand = str } }
                )

        UpdateSettingsPreviewCommand str ->
            modifySettingsDraft model
                (\draft settingsModel ->
                    { settingsModel | draft = { draft | previewCommand = str } }
                )

        UpdateSettingsInactivity str ->
            modifySettingsDraft model
                (\draft settingsModel ->
                    { settingsModel | draft = { draft | inactivityMinutes = str } }
                )

        SubmitSettings ->
            case model.page of
                SettingsPage settingsModel ->
                    if settingsModel.saving == Working then
                        ( model, Cmd.none )

                    else
                        let
                            trimmed =
                                String.trim settingsModel.draft.inactivityMinutes

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
                                    List.map
                                        (\prompt ->
                                            { key = prompt.key
                                            , content = prompt.content
                                            , description = prompt.description
                                            , version = prompt.version
                                            , status = Idle
                                            }
                                        )
                                        prompts
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
            updatePromptEdit model
                key
                (\edit -> { edit | content = content })

        UpdatePromptDescription key desc ->
            updatePromptEdit model
                key
                (\edit -> { edit | description = desc })

        SavePrompt key ->
            case model.page of
                SettingsPage settingsModel ->
                    let
                        updatedEdits =
                            List.map
                                (\edit ->
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
                                    List.map
                                        (\existing ->
                                            if existing.key == prompt.key then
                                                prompt

                                            else
                                                existing
                                        )
                                        prompts

                                updatedPrompts =
                                    settingsModel.prompts |> remoteDataMap updatePromptList

                                updatedEdits =
                                    List.map
                                        (\edit ->
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
                                        (\edit ->
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
                                (\edit ->
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
                                            (List.map
                                                (\existing ->
                                                    if existing.key == prompt.key then
                                                        prompt

                                                    else
                                                        existing
                                                )
                                            )

                                updatedEdits =
                                    List.map
                                        (\edit ->
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
                                        (\edit ->
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
                        (\edit ->
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


addNotification : String -> Model -> Model
addNotification message model =
    let
        notification =
            { id = model.nextNotificationId
            , text = message
            }

        trimmed =
            notification
                :: model.notifications
                |> List.take 5
    in
    { model
        | notifications = trimmed
        , nextNotificationId = model.nextNotificationId + 1
    }


automationKeywords : List String
automationKeywords =
    [ "automatic"
    , "snooze"
    , "resume"
    , "timeout"
    , "escalat"
    , "redirect"
    ]


eventOrigin : StatusEvent -> Maybe String
eventOrigin event =
    case event.payload of
        Just value ->
            case Decode.decodeValue (Decode.field "origin" Decode.string) value of
                Ok origin ->
                    Just origin

                Err _ ->
                    Nothing

        Nothing ->
            Nothing


isSystemEvent : StatusEvent -> Bool
isSystemEvent event =
    eventOrigin event == Just "system"


automationToastFor : StatusEvent -> Maybe String
automationToastFor event =
    if isSystemEvent event then
        let
            lower =
                String.toLower event.message
        in
        if List.any (\keyword -> String.contains keyword lower) automationKeywords then
            Just event.message

        else
            Nothing

    else
        Nothing


maybeAddNotification : StatusEvent -> Model -> Model
maybeAddNotification event model =
    case automationToastFor event of
        Just text ->
            addNotification text model

        Nothing ->
            model


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


messageRoleFromString : String -> Maybe AgentRole
messageRoleFromString str =
    case String.toLower str of
        "pm" ->
            Just AgentRoleProjectManager

        "implementer" ->
            Just AgentRoleImplementer

        "qa" ->
            Just AgentRoleQa

        _ ->
            Nothing


hasRedirectOptions : OrchestratorSnapshot -> TaskDetailModel -> Bool
hasRedirectOptions snapshot model =
    snapshot.activeTasks
        |> List.any (\summary -> summary.id /= model.id)


viewRedirectControls : TaskDetailModel -> OrchestratorSnapshot -> Html Msg
viewRedirectControls model snapshot =
    let
        available =
            snapshot.activeTasks
                |> List.filter (\summary -> summary.id /= model.id)

        selectValue =
            case model.redirectTarget of
                Just targetId ->
                    String.fromInt targetId

                Nothing ->
                    ""

        optionView summary =
            option [ value (String.fromInt summary.id) ]
                [ text (summary.title ++ " (#" ++ String.fromInt summary.id ++ ")") ]

        disabledButton =
            model.redirectTarget == Nothing || model.redirectState == Working
    in
    div [ class "space-y-2" ]
        [ select
            [ class "w-full rounded border border-slate-700 bg-slate-900 px-3 py-2 text-xs text-slate-200 focus:outline-none focus:ring-2 focus:ring-indigo-500"
            , value selectValue
            , onInput UpdateRedirectTarget
            ]
            (List.concat
                [ [ option [ value "" ] [ text "Select target task" ] ]
                , List.map optionView available
                ]
            )
        , button
            [ class "w-full rounded border border-slate-600 bg-slate-800/80 px-3 py-2 text-xs font-semibold text-slate-200 hover:border-indigo-400 disabled:opacity-40"
            , onClick RedirectWorkerToTask
            , disabled disabledButton
            ]
            [ text "Redirect Worker" ]
        ]


roleSelectValue : Maybe AgentRole -> String
roleSelectValue maybeRole =
    case maybeRole of
        Just AgentRoleProjectManager ->
            "pm"

        Just AgentRoleImplementer ->
            "implementer"

        Just AgentRoleQa ->
            "qa"

        Nothing ->
            ""


messageTemplates : List ( String, String )
messageTemplates =
    [ ( "Request Tests", "Please add regression tests covering the reported issue and rerun the configured test command." )
    , ( "Share Progress", "Give a brief summary of what changed, what remains, and any blockers you are facing." )
    , ( "Switch Focus", "Pause the current direction and follow the updated requirements described above instead." )
    ]


viewMessageTemplates : Html Msg
viewMessageTemplates =
    if List.isEmpty messageTemplates then
        text ""

    else
        div [ class "flex flex-wrap gap-2" ]
            (List.map templateButton messageTemplates)


templateButton : ( String, String ) -> Html Msg
templateButton ( label, template ) =
    button
        [ class "rounded border border-slate-700 px-2 py-1 text-[11px] text-slate-300 hover:border-indigo-400"
        , onClick (InsertMessageTemplate template)
        ]
        [ text label ]



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
                    , timelineVisibleCount = defaultTimelineWindow
                    , timelineNextCursor = Nothing
                    , agentLogs = Dict.empty
                    , agentLogVisible = Dict.empty
                    , selectedTab = TabActivity
                    , testCommandDraft = ""
                    , testCommandState = Idle
                    , qaSkipState = Idle
                    , previewStartState = Idle
                    , message = ""
                    , messageState = Idle
                    , messageTargetRole = Nothing
                    , messageAutoResume = True
                    , statusState = Idle
                    , pingState = Idle
                    , cancelState = Idle
                    , retryState = Idle
                    , pauseState = Idle
                    , reassignState = Idle
                    , redirectState = Idle
                    , redirectTarget = Nothing
                    , snoozeMinutesDraft = "30"
                    , snoozeState = Idle
                    , snapshot = Loading
                    , historyState = Idle
                    , historyRequest = Nothing
                    }

                openCmd =
                    openStatusStream { taskId = taskId, path = statusStreamPath modelCleared taskId }
            in
            ( { modelCleared | page = TaskDetailPage detailModel, activeStream = Just taskId }
            , Cmd.batch
                [ closeCmd
                , fetchTaskDetail modelCleared taskId
                , fetchSnapshot modelCleared
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


plainDelete : String -> (Result Http.Error a -> msg) -> Decoder a -> Cmd msg
plainDelete url toMsg decoder =
    Http.request
        { method = "DELETE"
        , headers = []
        , url = url
        , body = Http.emptyBody
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
                        "" ->
                            Encode.null

                        repo ->
                            Encode.string repo
                  )
                , ( "taskReqBranch"
                  , case String.trim form.branch of
                        "" ->
                            Encode.null

                        b ->
                            Encode.string b
                  )
                ]
    in
    plainPost (apiUrl model "/tasks") payload CreatedTask taskSummaryDecoder


fetchTaskDetail : Model -> Int -> Cmd Msg
fetchTaskDetail model taskId =
    getJson (apiUrl model ("/tasks/" ++ String.fromInt taskId)) (GotTaskDetail taskId) taskDetailDecoder


loadMoreHistory : Model -> TaskDetailModel -> Cmd Msg
loadMoreHistory model detailModel =
    case detailModel.timelineNextCursor of
        Nothing ->
            Cmd.none

        Just cursor ->
            getJson
                (apiUrl model
                    ("/tasks/"
                        ++ String.fromInt detailModel.id
                        ++ "/history?beforeId="
                        ++ String.fromInt cursor
                    )
                )
                HistoryLoaded
                taskHistoryDecoder


sendTaskMessage : Model -> TaskDetailModel -> String -> Cmd Msg
sendTaskMessage model detailModel message =
    let
        payloadFields =
            [ ( "agentMessage", Encode.string message )
            , ( "agentMessageAutoResume", Encode.bool detailModel.messageAutoResume )
            ]
                ++ (case detailModel.messageTargetRole of
                        Just role ->
                            [ ( "agentMessageRole", Api.jsonEncAgentRole role ) ]

                        Nothing ->
                            []
                   )

        payload =
            Encode.object payloadFields
    in
    plainPost (apiUrl model ("/tasks/" ++ String.fromInt detailModel.id ++ "/messages"))
        payload
        (\result ->
            case result of
                Ok _ ->
                    MessageSent (Ok ())

                Err err ->
                    MessageSent (Err err)
        )
        (Decode.field "ok" Decode.bool
            |> Decode.andThen
                (\isOk ->
                    if isOk then
                        Decode.succeed ()

                    else
                        Decode.fail "message send failed"
                )
        )


forceRetryRequest : Model -> Int -> Maybe String -> Cmd Msg
forceRetryRequest model taskId maybeInstructions =
    let
        payloadFields =
            case maybeInstructions of
                Nothing ->
                    []

                Just txt ->
                    [ ( "taskRetryInstructions", Encode.string txt ) ]

        payload =
            Encode.object payloadFields

        decoder =
            Decode.field "requeuedStep" Api.jsonDecWorkflowStep
    in
    plainPost (apiUrl model ("/tasks/" ++ String.fromInt taskId ++ "/retry")) payload ForceRetryResult decoder


pauseTaskRequest : Model -> Int -> Cmd Msg
pauseTaskRequest model taskId =
    plainPost
        (apiUrl model ("/tasks/" ++ String.fromInt taskId ++ "/pause"))
        (Encode.object [])
        PauseResult
        (Decode.field "paused" Decode.bool)


resumeTaskRequest : Model -> Int -> Cmd Msg
resumeTaskRequest model taskId =
    plainPost
        (apiUrl model ("/tasks/" ++ String.fromInt taskId ++ "/resume"))
        (Encode.object [])
        ResumeResult
        (Decode.field "paused" Decode.bool)


reassignTaskRequest : Model -> Int -> Cmd Msg
reassignTaskRequest model taskId =
    plainPost
        (apiUrl model ("/tasks/" ++ String.fromInt taskId ++ "/reassign"))
        (Encode.object [])
        ReassignResult
        (Decode.field "requeuedStep" Api.jsonDecWorkflowStep)


redirectWorkerRequest : Model -> Int -> Int -> Cmd Msg
redirectWorkerRequest model taskId targetId =
    let
        payload =
            Encode.object
                [ ( "taskRedirectTargetTaskId", Encode.int targetId ) ]
    in
    plainPost
        (apiUrl model ("/tasks/" ++ String.fromInt taskId ++ "/redirect"))
        payload
        RedirectResult
        taskRedirectResponseDecoder


scheduleSnoozeRequest : Model -> Int -> Int -> Cmd Msg
scheduleSnoozeRequest model taskId minutes =
    let
        payload =
            Encode.object
                [ ( "taskSnoozeMinutes", Encode.int minutes ) ]
    in
    plainPost
        (apiUrl model ("/tasks/" ++ String.fromInt taskId ++ "/snooze"))
        payload
        SnoozeResult
        taskSnoozeStatusDecoder


cancelSnoozeRequest : Model -> Int -> Cmd Msg
cancelSnoozeRequest model taskId =
    plainDelete
        (apiUrl model ("/tasks/" ++ String.fromInt taskId ++ "/snooze"))
        SnoozeCancelled
        taskSnoozeStatusDecoder


updateTaskStatus : Model -> Int -> TaskStatus -> Cmd Msg
updateTaskStatus model taskId status =
    patchJson
        (apiUrl model ("/tasks/" ++ String.fromInt taskId))
        (Encode.object
            [ ( "taskStatus", Api.jsonEncTaskStatus status ) ]
        )
        TaskStatusUpdated
        taskDetailDecoder


updateTaskTestCommand : Model -> Int -> Maybe String -> Cmd Msg
updateTaskTestCommand model taskId maybeCommand =
    let
        value =
            case maybeCommand of
                Just cmd ->
                    Encode.string cmd

                Nothing ->
                    Encode.null

        payload =
            Encode.object [ ( "taskTestCommand", value ) ]
    in
    putJson
        (apiUrl model ("/tasks/" ++ String.fromInt taskId ++ "/test-command"))
        payload
        TestCommandSaved
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


skipQa : Model -> Int -> Cmd Msg
skipQa model taskId =
    plainPost (apiUrl model ("/tasks/" ++ String.fromInt taskId ++ "/qa/skip"))
        (Encode.object [])
        QaSkipped
        taskDetailDecoder


startPreviewRequest : Model -> Int -> Cmd Msg
startPreviewRequest model taskId =
    plainPost (apiUrl model ("/tasks/" ++ String.fromInt taskId ++ "/preview/start"))
        (Encode.object [])
        PreviewStarted
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
                        "" ->
                            Encode.null

                        value ->
                            Encode.string value
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

        url =
            apiUrl model ("/prompts/" ++ edit.key)
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
            , viewNotifications model.notifications
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


viewNotifications : List Notification -> Html Msg
viewNotifications notifications =
    case notifications of
        [] ->
            text ""

        _ ->
            div [ class "fixed top-16 right-4 z-40 flex w-80 flex-col gap-2" ]
                (List.map viewNotification notifications)


viewNotification : Notification -> Html Msg
viewNotification notification =
    div [ class "rounded border border-indigo-500/40 bg-indigo-500/15 px-4 py-3 text-xs text-indigo-100 shadow-lg backdrop-blur" ]
        [ div [ class "flex items-start justify-between gap-3" ]
            [ span [ class "text-[11px] font-semibold uppercase tracking-wide text-indigo-200" ] [ text "Automation" ]
            , button [ class "text-[11px] text-indigo-200 hover:text-indigo-100", onClick (DismissNotification notification.id) ] [ text "Dismiss" ]
            ]
        , p [ class "mt-2 text-xs text-indigo-100 leading-relaxed" ] [ text notification.text ]
        ]


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
            let
                metricsById =
                    List.foldl
                        (\metric acc -> Dict.insert metric.id metric acc)
                        Dict.empty
                        snapshot.workerMetrics

                stateWorkers =
                    snapshot.workers

                workerCount =
                    if List.isEmpty stateWorkers then
                        List.length snapshot.workerMetrics

                    else
                        List.length stateWorkers

                workerViews =
                    if List.isEmpty stateWorkers then
                        snapshot.workerMetrics
                            |> List.map viewWorkerMetricChip

                    else
                        List.map (\status -> viewWorkerChip status (Dict.get status.id metricsById)) stateWorkers

                pausedSummaries =
                    List.filter .isPaused snapshot.activeTasks

                snoozedSummaries =
                    List.filter
                        (\summary ->
                            case summary.snoozeUntil of
                                Just _ ->
                                    True

                                Nothing ->
                                    False
                        )
                        snapshot.activeTasks

                blockedSummaries =
                    List.filter (\summary -> summary.status == TaskStatusBlocked) snapshot.activeTasks

                totalTasks =
                    List.length snapshot.activeTasks

                pausedCount =
                    List.length pausedSummaries

                snoozedCount =
                    List.length snoozedSummaries

                implementingCount =
                    List.length (List.filter (\summary -> summary.status == TaskStatusImplementing) snapshot.activeTasks)
            in
            div [ class "border-b border-slate-800 px-6 py-6 space-y-6" ]
                [ div [ class "flex flex-col gap-3 md:flex-row md:items-center md:justify-between" ]
                    [ div []
                        [ h3 [ class "text-lg font-semibold" ] [ text "Task Queue" ]
                        , p [ class "text-xs text-slate-400" ] [ text ("Queue depth: " ++ String.fromInt snapshot.queueDepth) ]
                        ]
                    , div [ class "flex flex-wrap gap-2" ]
                        (List.map viewSnapshotTask (List.take 3 snapshot.activeTasks))
                    ]
                , viewSnapshotStats totalTasks pausedCount snoozedCount implementingCount
                , viewTaskHealthPanel blockedSummaries pausedSummaries snoozedSummaries
                , div []
                    [ h4 [ class "text-sm font-semibold text-slate-300" ] [ text ("Workers (" ++ String.fromInt workerCount ++ ")") ]
                    , div [ class "mt-2 grid gap-3 md:grid-cols-2" ]
                        (if List.isEmpty workerViews then
                            [ span [ class "text-xs text-slate-500" ] [ text "Workers warming up…" ] ]

                         else
                            workerViews
                        )
                    ]
                ]

        Loading ->
            div [ class "border-b border-slate-800 px-6 py-4 text-sm text-slate-400" ] [ text "Loading snapshot…" ]

        Failure err ->
            div [ class "border-b border-slate-800 px-6 py-4 text-sm text-rose-300" ] [ text err ]

        NotAsked ->
            text ""


viewSnapshotStats : Int -> Int -> Int -> Int -> Html Msg
viewSnapshotStats total paused snoozed implementing =
    div [ class "grid gap-3 text-[11px] text-slate-300 md:grid-cols-2" ]
        [ viewSnapshotStat "Active" total
        , viewSnapshotStat "Paused" paused
        , viewSnapshotStat "Snoozed" snoozed
        , viewSnapshotStat "Implementing" implementing
        ]


viewSnapshotStat : String -> Int -> Html Msg
viewSnapshotStat label value =
    div [ class "rounded border border-slate-800 bg-slate-950/60 px-3 py-2" ]
        [ span [ class "block text-slate-500" ] [ text label ]
        , span [ class "text-sm font-semibold text-slate-100" ] [ text (String.fromInt value) ]
        ]


viewSnapshotTask summary =
    let
        tags =
            List.filterMap identity
                [ if summary.isPaused then
                    Just "paused"

                  else
                    Nothing
                , case summary.snoozeUntil of
                    Just _ ->
                        Just "snoozed"

                    Nothing ->
                        Nothing
                ]

        suffix =
            case tags of
                [] ->
                    ""

                _ ->
                    " · " ++ String.join ", " tags
    in
    span [ class "rounded border border-slate-800 px-3 py-1 text-xs text-slate-200" ]
        [ text (summary.title ++ suffix) ]


viewTaskHealthPanel : List TaskSummary -> List TaskSummary -> List TaskSummary -> Html Msg
viewTaskHealthPanel blocked paused snoozed =
    let
        sections =
            [ ( "Blocked", blocked, "border-rose-500/40 bg-rose-500/10 text-rose-200" )
            , ( "Paused", paused, "border-amber-500/40 bg-amber-500/10 text-amber-200" )
            , ( "Snoozed", snoozed, "border-sky-500/40 bg-sky-500/10 text-sky-200" )
            ]

        nonEmpty =
            List.filter (\( _, tasks, _ ) -> not (List.isEmpty tasks)) sections
    in
    if List.isEmpty nonEmpty then
        text ""

    else
        section [ class "rounded-xl border border-slate-900 bg-slate-900/80 p-6" ]
            [ h4 [ class "text-sm font-semibold text-slate-300" ] [ text "Attention Needed" ]
            , div [ class "mt-4 grid gap-3 md:grid-cols-3" ] (List.map viewHealthItem nonEmpty)
            ]


viewHealthItem : ( String, List TaskSummary, String ) -> Html Msg
viewHealthItem ( label, tasks, toneClasses ) =
    div [ class ("rounded border px-3 py-3 text-xs " ++ toneClasses) ]
        [ span [ class "text-[10px] font-semibold uppercase tracking-wide" ]
            [ text (label ++ " (" ++ String.fromInt (List.length tasks) ++ ")") ]
        , div [ class "mt-2 flex flex-wrap gap-2" ] (List.map viewAttentionTask tasks)
        ]


viewAttentionTask : TaskSummary -> Html Msg
viewAttentionTask summary =
    let
        label =
            summary.title ++ " (#" ++ String.fromInt summary.id ++ ")"
    in
    span [ class "rounded border border-current/40 bg-black/20 px-2 py-1 text-[11px]" ]
        [ text label ]


viewWorkerChip : WorkerStatus -> Maybe WorkerMetric -> Html Msg
viewWorkerChip worker maybeMetric =
    case worker.state of
        WorkerIdle since ->
            let
                metricLines =
                    maybeMetric
                        |> Maybe.map (workerMetricSummary False)
                        |> Maybe.withDefault []
            in
            div [ class "rounded border border-slate-800 bg-slate-950/60 px-3 py-2 text-[11px] text-slate-300" ]
                ([ span [ class "font-semibold text-slate-100" ] [ text ("Worker " ++ String.fromInt worker.id) ]
                 , span [ class "block text-slate-500" ] [ text ("Idle since " ++ formatTimestamp since) ]
                 ]
                    ++ metricLines
                )

        WorkerRunning info ->
            let
                titleLabel =
                    case info.taskTitle of
                        Just title ->
                            title

                        Nothing ->
                            "Task #" ++ String.fromInt info.taskId

                metricLines =
                    maybeMetric
                        |> Maybe.map (workerMetricSummary True)
                        |> Maybe.withDefault []
            in
            div [ class "rounded border border-indigo-500/50 bg-indigo-500/10 px-3 py-2 text-[11px] text-indigo-200" ]
                ([ span [ class "font-semibold" ]
                    [ text
                        ("Worker "
                            ++ String.fromInt worker.id
                            ++ " • "
                            ++ workflowStepToString info.step
                        )
                    ]
                 , span [ class "block text-indigo-200/80" ] [ text titleLabel ]
                 , span [ class "block text-indigo-200/60" ] [ text ("Since " ++ formatTimestamp info.startedAt) ]
                 ]
                    ++ metricLines
                )


viewWorkerMetricChip : WorkerMetric -> Html Msg
viewWorkerMetricChip metric =
    let
        statusLine =
            case ( metric.currentTaskId, metric.currentStep ) of
                ( Just taskId, Just step ) ->
                    "Running " ++ workflowStepToString step ++ " (task #" ++ String.fromInt taskId ++ ")"

                _ ->
                    "Idle"

        sinceLine =
            metric.startedAt
                |> Maybe.map (\ts -> span [ class "block text-slate-500" ] [ text ("Since " ++ formatTimestamp ts) ])

        lastLine =
            case ( metric.lastTaskId, metric.lastStep, metric.lastSuccess ) of
                ( Just taskId, Just step, Just success ) ->
                    let
                        label =
                            if success then
                                "Last success"

                            else
                                "Last failure"
                    in
                    Just
                        (span [ class "block text-slate-500" ]
                            [ text
                                (label
                                    ++ " on task #"
                                    ++ String.fromInt taskId
                                    ++ " during "
                                    ++ workflowStepToString step
                                )
                            ]
                        )

                _ ->
                    Nothing

        extra =
            List.filterMap identity [ sinceLine, lastLine ]
    in
    div [ class "rounded border border-slate-800 bg-slate-950/60 px-3 py-2 text-[11px] text-slate-300" ]
        (span [ class "font-semibold text-slate-100" ] [ text ("Worker " ++ String.fromInt metric.id) ]
            :: span [ class "block text-slate-400" ] [ text statusLine ]
            :: extra
        )


workerMetricSummary : Bool -> WorkerMetric -> List (Html Msg)
workerMetricSummary isRunning metric =
    let
        baseClass =
            if isRunning then
                "block text-[10px] text-indigo-200/70"

            else
                "block text-[10px] text-slate-500"

        accentClass =
            if isRunning then
                "block text-[10px] text-indigo-200/80"

            else
                "block text-[10px] text-slate-400"

        totals =
            [ span [ class accentClass ] [ text ("Assignments: " ++ String.fromInt metric.totalAssignments) ]
            , span [ class accentClass ] [ text ("Busy time: " ++ formatSeconds metric.totalBusySeconds) ]
            ]

        lastRunLines =
            case metric.lastStep of
                Just step ->
                    let
                        icon =
                            case metric.lastSuccess of
                                Just True ->
                                    "✔"

                                Just False ->
                                    "⚠"

                                Nothing ->
                                    "•"

                        durationText =
                            metric.lastDurationSeconds
                                |> Maybe.map formatSeconds
                                |> Maybe.withDefault "-"

                        headline =
                            icon ++ " Last: " ++ workflowStepToString step ++ " in " ++ durationText

                        errorLines =
                            case ( metric.lastSuccess, metric.lastError ) of
                                ( Just False, Just err ) ->
                                    [ span [ class baseClass ] [ text ("Last error: " ++ err) ] ]

                                _ ->
                                    []
                    in
                    span [ class baseClass ] [ text headline ] :: errorLines

                Nothing ->
                    []
    in
    totals ++ lastRunLines


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
                    , if summary.isPaused then
                        text " · paused"

                      else
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
            , Html.Attributes.min "1"
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

                PreviewFailed ->
                    ( "Preview failed", "border-rose-500/60 text-rose-200" )
    in
    span [ class ("rounded border px-2 py-1 text-xs " ++ classes) ] [ text labelText ]


workflowStepToString : WorkflowStep -> String
workflowStepToString step =
    case step of
        StepIntake ->
            "Intake"

        StepDesign ->
            "Design"

        StepImplementation ->
            "Implementation"

        StepSpecVerification ->
            "Spec Verification"

        StepPmReview ->
            "PM Review"

        StepQaReview ->
            "QA Review"

        StepFixIteration ->
            "Fix Iteration"

        StepCommit ->
            "Commit"

        StepPreview ->
            "Preview"

        StepFinalize ->
            "Finalize"



-- TASK DETAIL VIEW -----------------------------------------------------------


viewTaskDetail : TaskDetailModel -> Html Msg
viewTaskDetail model =
    let
        leftColumn =
            div [ class "space-y-6" ]
                [ viewDetailTabs model
                , viewTabContent model
                ]

        rightColumn =
            viewControlPanel model
    in
    div [ class "space-y-8 p-8" ]
        [ Lazy.lazy viewTaskSummary model.detail
        , div [ class "grid gap-6 lg:grid-cols-[2fr,1fr]" ]
            [ leftColumn
            , rightColumn
            ]
        ]


viewSettings : SettingsModel -> Html Msg
viewSettings model =
    let
        defaultsCard =
            section [ class "rounded-xl border border-slate-900 bg-slate-900/80 p-6" ]
                [ h2 [ class "text-sm font-semibold uppercase tracking-wide text-slate-400" ] [ text "Default Configuration" ]
                , case model.settings of
                    Loading ->
                        p [ class "mt-3 text-sm text-slate-400" ] [ text "Loading defaults…" ]

                    Failure err ->
                        p [ class "mt-3 text-sm text-rose-300" ] [ text err ]

                    NotAsked ->
                        text ""

                    Success _ ->
                        form [ class "mt-4 grid gap-3 md:grid-cols-2", onSubmit SubmitSettings ]
                            [ viewInput "Repository Root" model.draft.repoRoot UpdateSettingsRepo False
                            , viewInput "Default Branch" model.draft.branch UpdateSettingsBranch False
                            , viewInput "Test Command" model.draft.testCommand UpdateSettingsTestCommand True
                            , viewInput "Preview Command" model.draft.previewCommand UpdateSettingsPreviewCommand False
                            , viewNumberInput "Agent Timeout (minutes)" model.draft.inactivityMinutes UpdateSettingsInactivity
                            , div [ class "md:col-span-2 flex items-center justify-end gap-3" ]
                                [ case model.saving of
                                    Failed err ->
                                        span [ class "text-xs text-rose-300" ] [ text err ]

                                    Completed ->
                                        span [ class "text-xs text-emerald-300" ] [ text "Settings saved." ]

                                    _ ->
                                        text ""
                                , button
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
                            ]
                ]

        promptsCard =
            section [ class "rounded-xl border border-slate-900 bg-slate-900/80 p-6" ]
                [ h2 [ class "text-sm font-semibold uppercase tracking-wide text-slate-400" ] [ text "Prompt Templates" ]
                , case ( model.prompts, model.promptEdits ) of
                    ( Loading, _ ) ->
                        p [ class "mt-3 text-sm text-slate-400" ] [ text "Loading prompts…" ]

                    ( Failure err, _ ) ->
                        p [ class "mt-3 text-sm text-rose-300" ] [ text err ]

                    ( NotAsked, _ ) ->
                        text ""

                    ( Success prompts, edits ) ->
                        div [ class "mt-4 space-y-4" ] (List.map (viewPromptEditor edits) prompts)
                ]
    in
    div [ class "space-y-8 p-8" ]
        [ defaultsCard
        , promptsCard
        ]


viewDetailTabs : TaskDetailModel -> Html Msg
viewDetailTabs model =
    let
        eventCount =
            List.length model.events

        logCount =
            model.agentLogs
                |> Dict.values
                |> List.map List.length
                |> List.sum

        artifactCount =
            case model.detail of
                Success detail ->
                    List.length detail.artifacts

                _ ->
                    0

        tabInfo =
            [ ( TabActivity, "Activity", eventCount )
            , ( TabLogs, "Agent Output", logCount )
            , ( TabArtifacts, "Artifacts", artifactCount )
            ]

        tabButton ( tab, label, count ) =
            let
                isActive =
                    model.selectedTab == tab

                baseClasses =
                    "px-3 py-2 rounded-full text-xs font-semibold transition focus:outline-none"

                activeClasses =
                    if isActive then
                        " bg-indigo-500/20 text-indigo-200 border border-indigo-400"

                    else
                        " text-slate-300 border border-slate-800 hover:border-indigo-400 hover:text-indigo-200"

                countBadge =
                    if count > 0 then
                        " (" ++ String.fromInt count ++ ")"

                    else
                        ""
            in
            button
                [ class (baseClasses ++ activeClasses)
                , onClick (SelectDetailTab tab)
                ]
                [ text (label ++ countBadge) ]
    in
    nav [ class "flex flex-wrap gap-2" ] (List.map tabButton tabInfo)


viewTabContent : TaskDetailModel -> Html Msg
viewTabContent model =
    case model.selectedTab of
        TabActivity ->
            Lazy.lazy viewActivityTimeline model

        TabLogs ->
            Lazy.lazy viewAgentLogs model

        TabArtifacts ->
            Lazy.lazy viewArtifacts model.detail


viewTaskSummary : RemoteData TaskDetail -> Html Msg
viewTaskSummary data =
    case data of
        Success detail ->
            let
                summary =
                    detail.summary

                pausedBadgeView =
                    if summary.isPaused then
                        span [ class "rounded-full border border-amber-500/60 bg-amber-500/10 px-2 py-[2px] text-[10px] font-semibold uppercase tracking-wide text-amber-200" ] [ text "Paused" ]

                    else
                        text ""
            in
            section [ class "rounded-xl border border-slate-900 bg-slate-900/80 p-6" ]
                [ div [ class "flex flex-col gap-4 md:flex-row md:items-center md:justify-between" ]
                    [ div []
                        [ div [ class "flex items-center gap-3" ]
                            [ statusBadge summary.status
                            , pausedBadgeView
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


viewTestCommand : TaskDetailModel -> Html Msg
viewTestCommand model =
    case model.detail of
        Success detail ->
            let
                overrideValue =
                    detail.testCommandOverride

                effective =
                    detail.testCommand

                trimmedDraft =
                    String.trim model.testCommandDraft

                desired =
                    if trimmedDraft == "" then
                        Nothing

                    else
                        Just trimmedDraft

                hasChanges =
                    desired /= overrideValue

                isWorking =
                    model.testCommandState == Working

                saveDisabled =
                    isWorking || not hasChanges

                clearDisabled =
                    isWorking || (overrideValue == Nothing && trimmedDraft == "")

                stateNotice =
                    case model.testCommandState of
                        Failed err ->
                            Just (p [ class "text-xs text-rose-300" ] [ text err ])

                        Completed ->
                            Just (p [ class "text-xs text-emerald-300" ] [ text "Override updated." ])

                        _ ->
                            Nothing

                overrideInfo =
                    case overrideValue of
                        Just value ->
                            "Override active: " ++ value

                        Nothing ->
                            "No override (using default)."
            in
            section [ class "rounded-xl border border-slate-900 bg-slate-900/80 p-6" ]
                ([ h3 [ class "text-lg font-semibold" ] [ text "Test Command Override" ]
                 , div [ class "mt-2 text-xs text-slate-400" ]
                    [ span [] [ text "Effective command:" ]
                    , pre [ class "mt-1 whitespace-pre-wrap rounded bg-slate-950/70 px-3 py-2 text-sm text-slate-200" ] [ text effective ]
                    ]
                 , p [ class "mt-2 text-xs text-slate-500" ] [ text overrideInfo ]
                 , textarea
                    [ class "mt-3 min-h-[90px] w-full rounded bg-slate-950 px-3 py-2 text-sm text-slate-100 focus:outline-none focus:ring-2 focus:ring-indigo-500"
                    , value model.testCommandDraft
                    , onInput UpdateTestCommandDraft
                    , placeholder "Enter custom command, leave blank to use default"
                    ]
                    []
                 , div [ class "mt-3 flex gap-2" ]
                    [ button
                        [ class "rounded bg-indigo-600 px-3 py-1 text-xs font-semibold text-white hover:bg-indigo-500 disabled:opacity-40"
                        , disabled saveDisabled
                        , onClick SaveTestCommand
                        ]
                        [ text
                            (if isWorking then
                                "Saving…"

                             else
                                "Save Override"
                            )
                        ]
                    , button
                        [ class "rounded border border-slate-700 px-3 py-1 text-xs text-slate-300 hover:border-slate-500 disabled:opacity-40"
                        , disabled clearDisabled
                        , onClick ClearTestCommand
                        ]
                        [ text "Clear Override" ]
                    ]
                 ]
                    ++ Maybe.withDefault [] (Maybe.map List.singleton stateNotice)
                )

        Loading ->
            section [ class "rounded-xl border border-slate-900 bg-slate-900/80 p-6" ]
                [ h3 [ class "text-lg font-semibold" ] [ text "Test Command Override" ]
                , p [ class "mt-2 text-sm text-slate-400" ] [ text "Loading…" ]
                ]

        Failure err ->
            section [ class "rounded-xl border border-slate-900 bg-slate-900/80 p-6" ]
                [ h3 [ class "text-lg font-semibold" ] [ text "Test Command Override" ]
                , p [ class "mt-2 text-sm text-rose-300" ] [ text err ]
                ]

        NotAsked ->
            text ""


viewArtifacts : RemoteData TaskDetail -> Html Msg
viewArtifacts data =
    section [ class "rounded-xl border border-slate-900 bg-slate-900/80 p-6" ]
        [ h3 [ class "text-sm font-semibold uppercase tracking-wide text-slate-400" ] [ text "Artifacts" ]
        , case data of
            Loading ->
                p [ class "mt-2 text-sm text-slate-400" ] [ text "Loading artifacts…" ]

            Failure err ->
                p [ class "mt-2 text-sm text-rose-300" ] [ text err ]

            NotAsked ->
                text ""

            Success detail ->
                if List.isEmpty detail.artifacts then
                    p [ class "mt-2 text-sm text-slate-400" ] [ text "No artifacts published yet." ]

                else
                    div [ class "mt-4 space-y-3" ] (List.map viewArtifactCard detail.artifacts)
        ]


artifactKindText : ArtifactKind -> String
artifactKindText kind =
    case kind of
        ArtifactDesign ->
            "Design"

        ArtifactDiff ->
            "Diff"

        ArtifactTestLog ->
            "Test Log"

        ArtifactCommitLog ->
            "Commit Log"

        ArtifactPreviewLog ->
            "Preview Log"

        ArtifactPreviewPing ->
            "Preview Ping"

        ArtifactAgentTranscript ->
            "Transcript"


viewArtifactCard : Artifact -> Html Msg
viewArtifactCard artifact =
    div [ class "rounded border border-slate-800 bg-slate-950/60 p-4" ]
        [ div [ class "flex items-center justify-between" ]
            [ span [ class "text-sm font-semibold text-slate-200" ] [ text artifact.label ]
            , span [ class "text-[11px] uppercase tracking-wide text-slate-500" ] [ text (artifactKindText artifact.kind) ]
            ]
        , case artifact.path of
            Just pathStr ->
                span [ class "mt-1 block text-[11px] text-slate-400" ] [ text pathStr ]

            Nothing ->
                text ""
        , span [ class "mt-1 block text-[11px] text-slate-500" ] [ text (formatTimestamp artifact.createdAt) ]
        , case artifact.body of
            Nothing ->
                text ""

            Just value ->
                pre [ class "mt-3 whitespace-pre-wrap text-xs text-slate-300" ] [ text (Encode.encode 2 value) ]
        ]


viewAgentLogs : TaskDetailModel -> Html Msg
viewAgentLogs model =
    let
        logs =
            model.agentLogs

        limits =
            model.agentLogVisible

        roles =
            [ AgentRoleProjectManager, AgentRoleImplementer, AgentRoleQa ]

        loadingMore =
            model.historyState == Working

        hasRemoteMore =
            model.timelineNextCursor /= Nothing

        statusNotice =
            case ( model.historyState, model.historyRequest ) of
                ( Working, Just (HistoryFromLog _) ) ->
                    [ div [ class "mb-4 rounded border border-slate-700 bg-slate-900/80 px-3 py-2 text-xs text-slate-300" ] [ text "Loading older logs…" ] ]

                _ ->
                    []

        sections =
            roles
                |> List.filterMap
                    (\role ->
                        case Dict.get (roleKey role) logs of
                            Nothing ->
                                Nothing

                            Just entries ->
                                if List.isEmpty entries then
                                    Nothing

                                else
                                    let
                                        key =
                                            roleKey role

                                        limit =
                                            agentLogLimit limits key entries

                                        visible =
                                            takeNewest limit entries

                                        total =
                                            List.length entries

                                        shown =
                                            List.length visible

                                        hiddenCount =
                                            Basics.max 0 (total - shown)

                                        baseline =
                                            logBaselineCount entries

                                        canCollapse =
                                            limit > baseline && baseline > 0

                                        expandButton =
                                            if hiddenCount > 0 then
                                                [ button
                                                    [ class "rounded border border-slate-700 px-2 py-1 text-[11px] text-slate-300 hover:border-indigo-400 disabled:opacity-40"
                                                    , disabled loadingMore
                                                    , onClick (ShowMoreAgentLog role)
                                                    ]
                                                    [ text ("Show older (" ++ String.fromInt hiddenCount ++ ")") ]
                                                ]

                                            else if hasRemoteMore then
                                                [ button
                                                    [ class "rounded border border-slate-700 px-2 py-1 text-[11px] text-slate-300 hover:border-indigo-400 disabled:opacity-40"
                                                    , disabled loadingMore
                                                    , onClick (ShowMoreAgentLog role)
                                                    ]
                                                    [ text
                                                        (if loadingMore then
                                                            "Loading…"

                                                         else
                                                            "Load older logs"
                                                        )
                                                    ]
                                                ]

                                            else
                                                []

                                        collapseButton =
                                            if canCollapse then
                                                [ button
                                                    [ class "rounded border border-slate-700 px-2 py-1 text-[11px] text-slate-300 hover:border-indigo-400"
                                                    , onClick (CollapseAgentLog role)
                                                    ]
                                                    [ text "Collapse" ]
                                                ]

                                            else
                                                []
                                    in
                                    Just <|
                                        div [ class "mb-6" ]
                                            [ h4 [ class "text-sm font-semibold text-slate-200" ] [ text (agentRoleLabel role) ]
                                            , div [ class "mt-3" ]
                                                [ div [ class "max-h-60 space-y-1 overflow-y-auto rounded border border-slate-800 bg-slate-950/60 p-3" ]
                                                    (List.map viewAgentLogLine visible)
                                                , div [ class "mt-2 flex items-center justify-between text-[11px] text-slate-500" ]
                                                    (span []
                                                        [ text
                                                            ("Showing "
                                                                ++ String.fromInt shown
                                                                ++ " of "
                                                                ++ String.fromInt total
                                                                ++ " lines"
                                                            )
                                                        ]
                                                        :: (expandButton ++ collapseButton)
                                                    )
                                                ]
                                            ]
                    )
    in
    let
        content =
            if List.isEmpty sections then
                statusNotice
                    ++ [ p [ class "text-sm text-slate-400" ] [ text "No agent output yet." ] ]

            else
                statusNotice ++ sections
    in
    section [ class "rounded-xl border border-slate-900 bg-slate-900/80 p-6" ]
        (h3 [ class "text-lg font-semibold mb-4" ] [ text "Agent Output" ] :: content)


viewAgentLogLine : AgentLogEntry -> Html Msg
viewAgentLogLine entry =
    let
        timeText =
            formatTimestamp entry.createdAt

        streamLabel =
            case entry.stream of
                "stdout" ->
                    "stdout"

                "stderr" ->
                    "stderr"

                "publish" ->
                    "publish"

                "info" ->
                    "info"

                other ->
                    other

        lineClass =
            case entry.stream of
                "stderr" ->
                    "text-rose-300"

                "publish" ->
                    "text-indigo-300"

                "info" ->
                    "text-slate-300"

                _ ->
                    "text-slate-100"
    in
    div [ class "grid grid-cols-[auto,1fr] gap-3" ]
        [ span [ class "text-[10px] font-mono uppercase tracking-wide text-slate-500" ] [ text timeText ]
        , span
            [ class ("font-mono text-[12px] leading-snug whitespace-pre-wrap " ++ lineClass) ]
            [ text ("[" ++ streamLabel ++ "][" ++ workflowStepToString entry.step ++ "] " ++ entry.line) ]
        ]


viewActivityTimeline : TaskDetailModel -> Html Msg
viewActivityTimeline model =
    let
        detail =
            model.detail

        events =
            model.events

        normalizedCount =
            normalizeTimelineCount model.timelineVisibleCount events

        visibleEvents =
            List.take normalizedCount events

        total =
            List.length events

        shown =
            List.length visibleEvents

        hiddenCount =
            Basics.max 0 (total - shown)

        baseline =
            timelineBaselineCount events

        canCollapse =
            normalizedCount > baseline && baseline > 0

        loadingMore =
            model.historyState == Working

        hasRemoteMore =
            model.timelineNextCursor /= Nothing

        statusNotice =
            case model.historyState of
                Failed err ->
                    [ div [ class "mb-4 rounded border border-rose-500/40 bg-rose-500/10 px-3 py-2 text-xs text-rose-200" ] [ text ("Failed to load older history: " ++ err) ] ]

                Working ->
                    case model.historyRequest of
                        Just (HistoryFromLog _) ->
                            []

                        _ ->
                            [ div [ class "mb-4 rounded border border-slate-700 bg-slate-900/80 px-3 py-2 text-xs text-slate-300" ] [ text "Loading older history…" ] ]

                _ ->
                    []

        controls =
            let
                expandButton =
                    if hiddenCount > 0 then
                        [ button
                            [ class "rounded border border-slate-700 px-2 py-1 text-[11px] text-slate-300 hover:border-indigo-400 disabled:opacity-40"
                            , disabled loadingMore
                            , onClick ShowMoreTimeline
                            ]
                            [ text ("Show older (" ++ String.fromInt hiddenCount ++ ")") ]
                        ]

                    else if hasRemoteMore then
                        [ button
                            [ class "rounded border border-slate-700 px-2 py-1 text-[11px] text-slate-300 hover:border-indigo-400 disabled:opacity-40"
                            , disabled loadingMore
                            , onClick ShowMoreTimeline
                            ]
                            [ text
                                (if loadingMore then
                                    "Loading…"

                                 else
                                    "Load older history"
                                )
                            ]
                        ]

                    else
                        []

                collapseButton =
                    if canCollapse then
                        [ button
                            [ class "rounded border border-slate-700 px-2 py-1 text-[11px] text-slate-300 hover:border-indigo-400"
                            , onClick CollapseTimeline
                            ]
                            [ text "Collapse" ]
                        ]

                    else
                        []
            in
            expandButton ++ collapseButton
    in
    section [ class "rounded-xl border border-slate-900 bg-slate-900/80 p-6" ]
        [ h3 [ class "text-lg font-semibold mb-4" ] [ text "Live Timeline" ]
        , case detail of
            Success _ ->
                if total == 0 then
                    p [ class "text-sm text-slate-400" ] [ text "No major updates yet." ]

                else
                    div [ class "space-y-4" ]
                        (statusNotice
                            ++ [ div [ class "flex items-center justify-between text-[11px] text-slate-500" ]
                                    (span []
                                        [ text
                                            ("Showing "
                                                ++ String.fromInt shown
                                                ++ " of "
                                                ++ String.fromInt total
                                                ++ " events"
                                            )
                                        ]
                                        :: controls
                                    )
                               , ul [ class "space-y-4" ]
                                    (List.map viewEvent visibleEvents)
                               ]
                        )

            Loading ->
                p [ class "text-sm text-slate-400" ] [ text "Loading events…" ]

            Failure err ->
                p [ class "text-sm text-rose-300" ] [ text err ]

            NotAsked ->
                text ""
        ]


automationPayloadPreview : Maybe Decode.Value -> List (Html Msg)
automationPayloadPreview maybeValue =
    case maybeValue of
        Nothing ->
            []

        Just value ->
            let
                preview =
                    Encode.encode 2 value
                        |> String.lines
                        |> List.take 4
                        |> String.join "\n"
            in
            [ pre [ class "mt-2 whitespace-pre-wrap text-[10px] text-indigo-200/70" ] [ text preview ] ]


viewEvent : StatusEvent -> Html Msg
viewEvent event =
    let
        isAutomation =
            isSystemEvent event

        automationBadge =
            if isAutomation then
                span [ class "ml-2 rounded-full border border-indigo-500/40 bg-indigo-500/10 px-2 py-[2px] text-[10px] font-semibold uppercase tracking-wide text-indigo-200" ] [ text "Automation" ]

            else
                text ""
    in
    li [ class "rounded border border-slate-900 bg-slate-950/70 p-4" ]
        [ div [ class "flex items-center justify-between" ]
            [ span [ class "text-xs uppercase tracking-wide text-slate-500" ] [ text (workflowStepToString event.step) ]
            , span [ class "text-[11px] text-slate-500" ] [ text (formatTimestamp event.createdAt) ]
            ]
        , div [ class "mt-2 flex items-center text-xs text-slate-400" ]
            [ text
                (if isAutomation then
                    "System generated event"

                 else
                    ""
                )
            , automationBadge
            ]
        , p [ class "mt-2 text-sm text-slate-200" ] [ text event.message ]
        , case event.payload of
            Nothing ->
                text ""

            Just payloadValue ->
                pre [ class "mt-3 whitespace-pre-wrap text-xs text-slate-300" ] [ text (Encode.encode 2 payloadValue) ]
        ]


viewControlPanel : TaskDetailModel -> Html Msg
viewControlPanel model =
    div [ class "space-y-6" ]
        [ viewMessageCard model
        , viewWorkflowControls model
        , viewSnoozeCard model
        , viewTestCommand model
        ]


viewMessageCard : TaskDetailModel -> Html Msg
viewMessageCard model =
    section [ class "rounded-xl border border-slate-900 bg-slate-900/80 p-6" ]
        [ h3 [ class "text-sm font-semibold uppercase tracking-wide text-slate-400" ] [ text "Message Agents" ]
        , form [ class "mt-3 flex flex-col gap-3", onSubmit SubmitMessage ]
            [ textarea
                [ class "h-28 rounded bg-slate-950 px-3 py-2 text-sm text-slate-100 focus:outline-none focus:ring-2 focus:ring-indigo-500"
                , placeholder "Share guidance, unblock a worker, or hand off context…"
                , value model.message
                , onInput UpdateMessageInput
                ]
                []
            , div [ class "flex flex-col gap-2" ]
                [ div [ class "flex flex-wrap gap-3" ]
                    [ label [ class "flex flex-col text-[11px] font-semibold uppercase tracking-wide text-slate-400" ]
                        [ span [] [ text "Route To" ]
                        , select
                            [ class "mt-1 rounded bg-slate-950 px-3 py-2 text-xs text-slate-200 focus:outline-none focus:ring-2 focus:ring-indigo-500"
                            , value (roleSelectValue model.messageTargetRole)
                            , onInput UpdateMessageRole
                            ]
                            [ option [ value "" ] [ text "Auto (next agent)" ]
                            , option [ value "implementer" ] [ text "Implementation" ]
                            , option [ value "pm" ] [ text "Project Manager" ]
                            , option [ value "qa" ] [ text "QA" ]
                            ]
                        ]
                    , label [ class "flex items-center gap-2 text-xs text-slate-300" ]
                        [ input
                            [ type_ "checkbox"
                            , checked model.messageAutoResume
                            , onCheck ToggleMessageAutoResume
                            , class "h-4 w-4 rounded border border-slate-700 bg-slate-900 text-indigo-500 focus:ring-2 focus:ring-indigo-500"
                            ]
                            []
                        , span [] [ text "Auto resume after sending" ]
                        ]
                    ]
                , viewMessageTemplates
                ]
            , div [ class "flex items-center justify-between" ]
                [ case model.messageState of
                    Failed err ->
                        span [ class "text-xs text-rose-300" ] [ text err ]

                    Completed ->
                        span [ class "text-xs text-emerald-300" ] [ text "Message sent." ]

                    _ ->
                        text ""
                , button
                    [ class "rounded bg-indigo-600 px-3 py-1 text-xs font-semibold text-white hover:bg-indigo-500 disabled:opacity-40"
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
                ]
            ]
        ]


viewWorkflowControls : TaskDetailModel -> Html Msg
viewWorkflowControls model =
    let
        isPaused =
            case model.detail of
                Success detail ->
                    detail.isPaused

                _ ->
                    False

        statusWorking =
            model.statusState == Working

        cancelWorking =
            model.cancelState == Working

        retryWorking =
            model.retryState == Working

        pauseWorking =
            model.pauseState == Working

        reassignWorking =
            model.reassignState == Working

        qaWorking =
            model.qaSkipState == Working

        previewWorking =
            model.previewStartState == Working

        previewDisabled =
            case model.detail of
                Success detail ->
                    detail.summary.previewStatus /= PreviewOffline || previewWorking

                _ ->
                    True

        previewLabel =
            case model.previewStartState of
                Working ->
                    "Starting preview…"

                Failed _ ->
                    "Retry Preview"

                Completed ->
                    "Preview requested"

                Idle ->
                    if previewDisabled then
                        "Preview running"

                    else
                        "Start Preview"

        pauseLabel =
            case ( isPaused, model.pauseState ) of
                ( True, Working ) ->
                    "Resuming…"

                ( False, Working ) ->
                    "Pausing…"

                ( True, _ ) ->
                    "Resume Task"

                ( False, _ ) ->
                    "Pause Task"
    in
    section [ class "rounded-xl border border-slate-900 bg-slate-900/80 p-6" ]
        [ h3 [ class "text-sm font-semibold uppercase tracking-wide text-slate-400" ] [ text "Workflow Controls" ]
        , div [ class "mt-4 space-y-5" ]
            [ actionGroup "Status"
                [ actionButton ToneSuccess statusWorking (UpdateTaskStatus TaskStatusCompleted) "Mark as Merged"
                , actionButton ToneWarning statusWorking (UpdateTaskStatus TaskStatusDiscarded) "Mark as Discarded"
                , actionButton ToneDanger
                    cancelWorking
                    CancelTask
                    (if cancelWorking then
                        "Killing…"

                     else
                        "Kill Task"
                    )
                , viewRequestNotice model.statusState "Status updated."
                , viewRequestNotice model.cancelState "Task cancelled."
                ]
            , actionGroup "Execution"
                [ actionButton TonePrimary
                    retryWorking
                    ForceRetry
                    (if retryWorking then
                        "Retrying…"

                     else
                        "Force Retry"
                    )
                , viewRequestNotice model.retryState "Retry scheduled."
                , actionButton TonePrimary
                    qaWorking
                    SkipQa
                    (if qaWorking then
                        "Skipping…"

                     else
                        "Skip QA and Continue"
                    )
                , viewRequestNotice model.qaSkipState "QA skipped."
                , actionButton TonePrimary previewDisabled StartPreview previewLabel
                , viewRequestNotice model.previewStartState "Preview requested."
                ]
            , workerSection model isPaused pauseLabel pauseWorking reassignWorking
            ]
        ]


workerSection : TaskDetailModel -> Bool -> String -> Bool -> Bool -> Html Msg
workerSection model isPaused pauseLabel pauseWorking reassignWorking =
    let
        redirectContent =
            case model.snapshot of
                Success snapshot ->
                    if hasRedirectOptions snapshot model then
                        viewRedirectControls model snapshot

                    else
                        p [ class "text-xs text-slate-400" ] [ text "No other active tasks available for redirect." ]

                Loading ->
                    p [ class "text-xs text-slate-400" ] [ text "Loading worker snapshot…" ]

                Failure err ->
                    p [ class "text-xs text-rose-300" ] [ text err ]

                NotAsked ->
                    text ""
    in
    actionGroup "Worker"
        [ actionButton ToneWarning
            pauseWorking
            (if isPaused then
                ResumeCurrentTask

             else
                PauseCurrentTask
            )
            pauseLabel
        , viewRequestNotice model.pauseState
            (if isPaused then
                "Task resumed."

             else
                "Task paused."
            )
        , actionButton ToneNeutral
            reassignWorking
            ReassignWorker
            (if reassignWorking then
                "Reassigning…"

             else
                "Reassign Worker"
            )
        , viewRequestNotice model.reassignState "Worker reassigned."
        , div [ class "space-y-2" ]
            [ redirectContent
            , viewRequestNotice model.redirectState "Worker redirected."
            ]
        ]


viewSnoozeCard : TaskDetailModel -> Html Msg
viewSnoozeCard model =
    let
        currentSnooze =
            case model.detail of
                Success detail ->
                    detail.snoozeUntil

                _ ->
                    Nothing

        hasActiveSnooze =
            currentSnooze /= Nothing

        scheduleDisabled =
            model.snoozeState == Working

        cancelDisabled =
            scheduleDisabled || not hasActiveSnooze

        snoozeLabel =
            case currentSnooze of
                Just ts ->
                    "Resumes at " ++ formatTimestamp ts

                Nothing ->
                    "No snooze scheduled"

        successMessage =
            if hasActiveSnooze then
                "Snooze scheduled."

            else
                "Snooze cancelled."
    in
    section [ class "rounded-xl border border-slate-900 bg-slate-900/80 p-6" ]
        [ h3 [ class "text-sm font-semibold uppercase tracking-wide text-slate-400" ] [ text "Snooze" ]
        , p [ class "text-xs text-slate-400" ] [ text snoozeLabel ]
        , div [ class "mt-3 flex gap-2" ]
            [ input
                [ class "w-24 rounded border border-slate-700 bg-slate-950 px-2 py-1 text-xs text-slate-200 focus:outline-none focus:ring-2 focus:ring-indigo-500"
                , placeholder "Minutes"
                , value model.snoozeMinutesDraft
                , onInput UpdateSnoozeDraft
                , type_ "number"
                , Html.Attributes.min "1"
                , disabled scheduleDisabled
                ]
                []
            , button
                [ class "rounded border border-indigo-500/40 bg-indigo-500/10 px-3 py-1 text-xs font-semibold text-indigo-200 hover:border-indigo-400 disabled:opacity-40"
                , onClick SubmitSnooze
                , disabled scheduleDisabled
                ]
                [ text
                    (if scheduleDisabled then
                        "Scheduling…"

                     else
                        "Snooze"
                    )
                ]
            , button
                [ class "rounded border border-slate-700 bg-slate-800 px-3 py-1 text-xs font-semibold text-slate-300 hover:border-indigo-400 disabled:opacity-40"
                , onClick CancelSnooze
                , disabled cancelDisabled
                ]
                [ text
                    (if model.snoozeState == Working && hasActiveSnooze then
                        "Cancelling…"

                     else
                        "Cancel Snooze"
                    )
                ]
            ]
        , viewRequestNotice model.snoozeState successMessage
        ]


actionGroup : String -> List (Html Msg) -> Html Msg
actionGroup title rows =
    div [ class "space-y-2" ]
        [ span [ class "text-[11px] font-semibold uppercase tracking-wide text-slate-500" ] [ text title ]
        , div [ class "space-y-2" ] rows
        ]


type ActionTone
    = TonePrimary
    | ToneSuccess
    | ToneWarning
    | ToneDanger
    | ToneNeutral


actionButton : ActionTone -> Bool -> Msg -> String -> Html Msg
actionButton tone disabled msg label =
    button
        [ class (buttonClasses tone ++ " disabled:opacity-40")
        , Html.Attributes.disabled disabled
        , onClick msg
        ]
        [ text label ]


buttonClasses : ActionTone -> String
buttonClasses tone =
    let
        base =
            "w-full rounded px-3 py-2 text-xs font-semibold text-left focus:outline-none transition"
    in
    base
        ++ " "
        ++ (case tone of
                TonePrimary ->
                    "border border-indigo-500/40 bg-indigo-500/10 text-indigo-200 hover:border-indigo-400"

                ToneSuccess ->
                    "border border-emerald-500/40 bg-emerald-500/10 text-emerald-200 hover:border-emerald-400"

                ToneWarning ->
                    "border border-amber-500/50 bg-amber-500/10 text-amber-200 hover:border-amber-400"

                ToneDanger ->
                    "border border-rose-500/50 bg-rose-500/10 text-rose-200 hover:border-rose-400"

                ToneNeutral ->
                    "border border-slate-700 bg-slate-800 text-slate-200 hover:border-indigo-400"
           )


viewRequestNotice : RequestState -> String -> Html Msg
viewRequestNotice state successMsg =
    case state of
        Failed err ->
            p [ class "text-xs text-rose-300" ] [ text err ]

        Completed ->
            p [ class "text-xs text-emerald-300" ] [ text successMsg ]

        _ ->
            text ""


viewPromptEditor : PromptEdits -> PromptTemplate -> Html Msg
viewPromptEditor edits prompt =
    let
        currentEdit =
            List.filter
                (\edit -> edit.key == prompt.key)
                edits
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
