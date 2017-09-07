effect module Throttle where { command = MyCmd } exposing (both, leading, trailing)

import Dict exposing (Dict)
import Platform
import Process
import Set exposing (Set)
import Task exposing (Task)
import Time exposing (Time)


-- COMMANDS


type MyCmd msg
    = Throttle (Request msg)


type alias Request msg =
    { throttleType : ThrottleType, interval : Time, id : Id, msg : msg }


type ThrottleType
    = Leading
    | Trailing
    | Both


cmdMap : (a -> b) -> MyCmd a -> MyCmd b
cmdMap f (Throttle ({ msg } as mdl)) =
    Throttle { mdl | msg = f msg }


leading : Time -> Id -> msg -> Cmd msg
leading =
    throttleCommand Leading


trailing : Time -> Id -> msg -> Cmd msg
trailing =
    throttleCommand Trailing


both : Time -> Id -> msg -> Cmd msg
both =
    throttleCommand Both


throttleCommand : ThrottleType -> Time -> Id -> msg -> Cmd msg
throttleCommand throttleType interval id msg =
    command (Throttle { throttleType = throttleType, interval = interval, id = id, msg = msg })



-- INTERNAL STATE


type alias Id =
    String


type alias State msg =
    { blockedThrottles : BlockedThrottles
    , trailingMessages : TrailingMessages msg
    }


type alias BlockedThrottles =
    Set Id


type alias TrailingMessages msg =
    Dict Id msg


init : Task Never (State msg)
init =
    Task.succeed (State Set.empty Dict.empty)



-- SELF MESSAGES


type SelfMsg
    = HandleThrottle Id Time


onSelfMsg : Platform.Router msg SelfMsg -> SelfMsg -> State msg -> Task Never (State msg)
onSelfMsg router (HandleThrottle id interval) ({ trailingMessages } as state) =
    case Dict.get id trailingMessages of
        Just msg ->
            sendBackToApp router msg state
                |> Task.andThen (updateTrailingMessages (Dict.remove id))
                |> Task.andThen (handleThrottlingAfterDelay router id interval)

        Nothing ->
            updateBlockedThrottles (Set.remove id) state



-- APP MESSAGES


onEffects : Platform.Router msg SelfMsg -> List (MyCmd msg) -> State msg -> Task Never (State msg)
onEffects router cmds state =
    let
        handleCommand (Throttle request) =
            Task.andThen (throttleHelp router request)
    in
    List.foldl handleCommand (Task.succeed state) cmds


throttleHelp : Platform.Router msg SelfMsg -> Request msg -> State msg -> Task Never (State msg)
throttleHelp router { throttleType, interval, id, msg } ({ blockedThrottles } as state) =
    let
        isBlocked =
            Set.member id blockedThrottles

        ( mustHandleLeading, mustAddMsgToTrailing ) =
            ( not isBlocked && (throttleType == Both || throttleType == Leading)
            , throttleType == Trailing || isBlocked && throttleType == Both
            )

        doIf p task state =
            if p then
                task state
            else
                Task.succeed state

        ( maybeHandleLeading, maybeAddToTrailing, maybeStartThrottling ) =
            ( doIf mustHandleLeading <| sendBackToApp router msg
            , doIf mustAddMsgToTrailing <| updateTrailingMessages (Dict.insert id msg)
            , doIf (not isBlocked) <| handleThrottlingAfterDelay router id interval
            )
    in
    maybeHandleLeading state
        |> Task.andThen maybeAddToTrailing
        |> Task.andThen maybeStartThrottling



-- COMMON TASKS


handleThrottlingAfterDelay : Platform.Router msg SelfMsg -> Id -> Time -> State msg -> Task Never (State msg)
handleThrottlingAfterDelay router id interval state =
    let
        spawnTask =
            Process.spawn (Process.sleep interval &> Platform.sendToSelf router (HandleThrottle id interval))
    in
    do spawnTask state
        |> Task.andThen (updateBlockedThrottles (Set.insert id))


updateBlockedThrottles : (BlockedThrottles -> BlockedThrottles) -> State msg -> Task Never (State msg)
updateBlockedThrottles f state =
    Task.succeed { state | blockedThrottles = f state.blockedThrottles }


updateTrailingMessages : (TrailingMessages msg -> TrailingMessages msg) -> State msg -> Task Never (State msg)
updateTrailingMessages f state =
    Task.succeed { state | trailingMessages = f state.trailingMessages }


sendBackToApp : Platform.Router msg SelfMsg -> msg -> State msg -> Task Never (State msg)
sendBackToApp router msg =
    do (Platform.sendToApp router msg)



-- TASK HELPERS


(&>) : Task x a -> Task x b -> Task x b
(&>) task1 task2 =
    Task.andThen (\_ -> task2) task1


do : Task Never b -> a -> Task Never a
do work a =
    work |> Task.map (always a)
