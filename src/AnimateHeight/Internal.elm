module AnimateHeight.Internal exposing
    ( Adjustment(..)
    , AdjustmentAction(..)
    , AnimationState
    , AnimationStrategy(..)
    , Bezier(..)
    , CalculatedHeight(..)
    , Duration(..)
    , InternalMsg
    , InterruptData
    , QueryLifeCycle(..)
    , RecalcData
    , Scene
    , Step(..)
    , TimingFunction(..)
    , UniqueContainerId
    , buildContainerId
    , calculatedHeightWhenHigh
    , containerIdToString
    , deltaAndDuration
    , durationToMillis
    , heightRatio
    , heightToHighWhenAnimationFrame
    , heightToHighWhenTransition
    , heightToLowWhenAnimationFrame
    , internalSubs
    , interruptDataToHigh
    , interruptDataToLow
    , timingToCubicBezierXY
    , transitionInterruptData
    , updateAnimateHeight
    )

import AnimateHeight.Constants as Constants
import Browser.Dom as BrowserDom
import Browser.Events as BrowserEvents
import Ease as Ease
import Task
import Time


type QueryLifeCycle
    = QueryViewport
    | QueryResolving


type Step
    = ToContent
    | ToMin
    | QueryForContent QueryLifeCycle
    | AnimateToContent Scene
    | QueryForMin QueryLifeCycle
    | AnimateToMin Scene
    | QueryForRecalc QueryLifeCycle


type alias AnimationState =
    { step : Step
    , containerId : UniqueContainerId
    , duration : Duration
    , timing : TimingFunction
    , calculatedHeight : CalculatedHeight
    , startTime : Maybe Time.Posix
    , adjustment : Maybe Adjustment
    , snapToContent : Bool
    , warmUpScene : Bool
    , animationStrategy : AnimationStrategy
    }


type UniqueContainerId
    = UniqueContainerId String


type InternalMsg
    = QueryViewportForHigh Time.Posix
    | ViewportForHigh (Result BrowserDom.Error BrowserDom.Viewport)
    | QueryViewportForRecalc Time.Posix
    | ViewportForRecalc (Result BrowserDom.Error BrowserDom.Viewport)
    | QueryViewportForLow Time.Posix
    | ViewportForLow (Result BrowserDom.Error BrowserDom.Viewport)
    | ToHigh Time.Posix
    | ToLow Time.Posix


type Adjustment
    = Interrupt (AdjustmentAction InterruptData)
    | Recalc (AdjustmentAction RecalcData)


type AdjustmentAction data
    = Triggered
    | Resolved data


type alias RecalcData =
    Float


type alias InterruptData =
    { duration : Float
    , interruptedAt : Time.Posix
    , startingHeight : Float
    , targetHeight : Float
    }


type CalculatedHeight
    = Auto
    | Pixel Float


type alias Scene =
    { height : Float }



-- Straight rip off from Css.Transitions for mapping purposes.


type TimingFunction
    = Ease
    | Linear
    | EaseIn
    | EaseOut
    | EaseInOut
    | CubicBezier Bezier


type Bezier
    = Bezier Float Float Float Float


calculateHeightToLow : Int -> Duration -> Scene -> TimingFunction -> AnimationStrategy -> CalculatedHeight -> Maybe Adjustment -> CalculatedHeight
calculateHeightToLow lapsedTime duration_ scene timing strat currentHeight adjustments =
    case strat of
        Transition ->
            heightToLowWhenTransition adjustments scene

        AnimationFrame ->
            case adjustments of
                Just (Interrupt Triggered) ->
                    currentHeight

                _ ->
                    heightToLowWhenAnimationFrame
                        lapsedTime
                        duration_
                        timing
                        scene
                        adjustments


heightToLowWhenTransition : Maybe Adjustment -> Scene -> CalculatedHeight
heightToLowWhenTransition adjustment scene =
    case adjustment of
        Just (Recalc _) ->
            Pixel scene.height

        Just (Interrupt (Resolved data)) ->
            Pixel data.targetHeight

        Just (Interrupt Triggered) ->
            Pixel 0

        _ ->
            Pixel 0


heightToHighWhenTransition : CalculatedHeight -> Scene -> CalculatedHeight
heightToHighWhenTransition currentHeight scene =
    case currentHeight of
        Pixel _ ->
            Pixel scene.height

        Auto ->
            Auto


calculatedHeightWhenHigh : Scene -> Bool -> CalculatedHeight
calculatedHeightWhenHigh scene snap =
    if snap then
        Auto

    else
        Pixel scene.height


calculateHeightToHigh : Int -> Duration -> Scene -> TimingFunction -> AnimationStrategy -> CalculatedHeight -> Maybe Adjustment -> CalculatedHeight
calculateHeightToHigh lapsedTime duration_ scene timing strat currentHeight adjustment =
    case strat of
        Transition ->
            heightToHighWhenTransition currentHeight scene

        AnimationFrame ->
            case adjustment of
                Just (Interrupt Triggered) ->
                    currentHeight

                _ ->
                    heightToHighWhenAnimationFrame
                        lapsedTime
                        duration_
                        timing
                        scene
                        currentHeight
                        adjustment


type Duration
    = Instant
    | Immediate
    | Rapid
    | Fast
    | Custom Float


deltaAndDuration : Maybe Adjustment -> Time.Posix -> Time.Posix -> Duration -> ( Int, Duration )
deltaAndDuration interrupt now start duration_ =
    case interrupt of
        Just (Interrupt (Resolved interr)) ->
            ( Time.posixToMillis now - Time.posixToMillis interr.interruptedAt, Custom interr.duration )

        _ ->
            ( Time.posixToMillis now - Time.posixToMillis start, duration_ )


durationToMillis : Duration -> Float
durationToMillis duration_ =
    case duration_ of
        Instant ->
            0

        Immediate ->
            200

        Rapid ->
            250

        Fast ->
            300

        Custom f ->
            f


heightToHighWhenAnimationFrame :
    Int
    -> Duration
    -> TimingFunction
    -> Scene
    -> CalculatedHeight
    -> Maybe Adjustment
    -> CalculatedHeight
heightToHighWhenAnimationFrame timeLapsed duration_ timing scene currentHeight adjustment =
    let
        ratio =
            heightRatio timeLapsed duration_ timing

        incrementalHeight height =
            ratio * height

        calculatedHeight =
            case currentHeight of
                Pixel h ->
                    case adjustment of
                        Just (Interrupt (Resolved data)) ->
                            Pixel <| (data.startingHeight + incrementalHeight (data.targetHeight - data.startingHeight))

                        Just (Recalc (Resolved startingHeight)) ->
                            Pixel <| startingHeight + incrementalHeight (scene.height - startingHeight)

                        _ ->
                            Pixel <| h + incrementalHeight scene.height - h

                Auto ->
                    Auto
    in
    calculatedHeight


heightRatio : Int -> Duration -> TimingFunction -> Float
heightRatio timeLapsed duration_ timing =
    let
        (Bezier x1 x2 y1 y2) =
            timingToCubicBezierXY timing

        deltaRatio =
            toFloat timeLapsed / durationToMillis duration_
    in
    if deltaRatio > 1 then
        Ease.bezier x1 x2 y1 y2 1

    else
        Ease.bezier x1 x2 y1 y2 deltaRatio


timingToCubicBezierXY : TimingFunction -> Bezier
timingToCubicBezierXY timing =
    case timing of
        Ease ->
            Bezier 0.25 0.1 0.25 1.0

        Linear ->
            Bezier 0.0 0.0 1.0 1.0

        EaseIn ->
            Bezier 0.42 0.0 1.0 1.0

        EaseInOut ->
            Bezier 0.42 0.0 0.58 1.0

        EaseOut ->
            Bezier 0.0 0.0 0.58 1.0

        CubicBezier bezier ->
            bezier


heightToLowWhenAnimationFrame :
    Int
    -> Duration
    -> TimingFunction
    -> Scene
    -> Maybe Adjustment
    -> CalculatedHeight
heightToLowWhenAnimationFrame timeLapsed duration_ timing scene adjustment =
    let
        ratio =
            heightRatio timeLapsed duration_ timing

        incrementalHeight height =
            ratio * height

        calculatedHeight =
            case adjustment of
                Just (Interrupt (Resolved data)) ->
                    Pixel <| data.startingHeight - incrementalHeight (data.startingHeight - data.targetHeight)

                Just (Recalc (Resolved startingHeight)) ->
                    Pixel <| (startingHeight - incrementalHeight (startingHeight - scene.height))

                _ ->
                    Pixel <| scene.height - incrementalHeight scene.height
    in
    calculatedHeight


interruptDataToLow : Time.Posix -> Int -> Float -> CalculatedHeight -> Scene -> Duration -> AnimationStrategy -> InterruptData
interruptDataToLow now dt targetHeight calculatedHeight scene duration_ animationStrategy =
    case animationStrategy of
        AnimationFrame ->
            case calculatedHeight of
                Pixel ch ->
                    if targetHeight == 0 && scene.height == 0 then
                        { startingHeight = ch, duration = durationToMillis duration_, interruptedAt = now, targetHeight = targetHeight }

                    else if targetHeight == 0 then
                        { startingHeight = ch, duration = ch / scene.height * durationToMillis duration_, interruptedAt = now, targetHeight = targetHeight }

                    else
                        { startingHeight = ch, duration = scene.height / ch * durationToMillis duration_, interruptedAt = now, targetHeight = targetHeight }

                -- This should never be possible
                Auto ->
                    { startingHeight = 0, duration = 0, interruptedAt = now, targetHeight = scene.height }

        Transition ->
            transitionInterruptData now dt targetHeight


interruptDataToHigh : Time.Posix -> Int -> CalculatedHeight -> Scene -> Duration -> AnimationStrategy -> InterruptData
interruptDataToHigh now dt calculatedHeight scene duration_ animationStrategy =
    case animationStrategy of
        AnimationFrame ->
            case calculatedHeight of
                Pixel ch ->
                    { startingHeight = ch, duration = ((scene.height - ch) / scene.height) * durationToMillis duration_, interruptedAt = now, targetHeight = scene.height }

                -- this should never be possible
                Auto ->
                    { startingHeight = 0, duration = 0, interruptedAt = now, targetHeight = scene.height }

        Transition ->
            transitionInterruptData now dt scene.height


type AnimationStrategy
    = Transition
    | AnimationFrame


transitionInterruptData : Time.Posix -> Int -> Float -> InterruptData
transitionInterruptData now dt targetHeight =
    { startingHeight = 0, duration = toFloat dt, interruptedAt = now, targetHeight = targetHeight }


containerIdToString : UniqueContainerId -> String
containerIdToString (UniqueContainerId id_) =
    id_


buildContainerId : String -> UniqueContainerId
buildContainerId id =
    UniqueContainerId (id ++ Constants.idSuffix)


internalSubs : AnimationState -> Sub InternalMsg
internalSubs state_ =
    let
        query msg =
            BrowserEvents.onAnimationFrame msg

        animate msg =
            BrowserEvents.onAnimationFrame msg
    in
    case state_.step of
        QueryForRecalc QueryViewport ->
            query QueryViewportForRecalc

        QueryForContent QueryViewport ->
            query QueryViewportForHigh

        AnimateToContent _ ->
            animate ToHigh

        ToContent ->
            case ( state_.snapToContent, state_.warmUpScene ) of
                ( True, False ) ->
                    Sub.none

                ( _, True ) ->
                    query QueryViewportForHigh

                _ ->
                    Sub.none

        QueryForMin QueryViewport ->
            animate QueryViewportForLow

        AnimateToMin _ ->
            animate ToLow

        _ ->
            Sub.none


updateAnimateHeight : InternalMsg -> AnimationState -> ( AnimationState, Cmd InternalMsg )
updateAnimateHeight msg state_ =
    case msg of
        QueryViewportForRecalc _ ->
            ( { state_ | step = QueryForRecalc QueryResolving }
            , Task.attempt ViewportForRecalc <| BrowserDom.getViewportOf (containerIdToString state_.containerId)
            )

        ViewportForRecalc (Ok viewPort) ->
            case state_.calculatedHeight of
                Pixel ch ->
                    let
                        step =
                            if ch < viewPort.scene.height then
                                AnimateToContent { height = viewPort.scene.height }

                            else if ch == viewPort.scene.height then
                                state_.step

                            else
                                AnimateToMin { height = viewPort.scene.height }
                    in
                    ( { state_ | step = step }, Cmd.none )

                Auto ->
                    ( state_, Cmd.none )

        ViewportForRecalc (Err _) ->
            ( state_, Cmd.none )

        QueryViewportForHigh _ ->
            ( { state_
                | step = QueryForContent QueryResolving
              }
            , Task.attempt ViewportForHigh <| BrowserDom.getViewportOf (containerIdToString state_.containerId)
            )

        ViewportForHigh (Ok viewPort) ->
            let
                ( step, ch ) =
                    if state_.warmUpScene && state_.snapToContent then
                        ( ToContent, Auto )

                    else if state_.warmUpScene then
                        ( ToContent, Pixel viewPort.scene.height )

                    else if viewPort.scene.height == 0 then
                        ( ToContent, Pixel viewPort.scene.height )

                    else
                        ( AnimateToContent { height = viewPort.scene.height }, state_.calculatedHeight )
            in
            ( { state_
                | warmUpScene = False
                , step = step
                , calculatedHeight = ch
              }
            , Cmd.none
            )

        -- We didn't find a node to so we don't have any height info.
        -- lets snap high so the content can be seen.
        ViewportForHigh (Err _) ->
            ( { state_
                | step = ToContent
                , calculatedHeight = Auto
                , warmUpScene = False
              }
            , Cmd.none
            )

        QueryViewportForLow _ ->
            ( { state_ | step = QueryForMin QueryResolving }, Task.attempt ViewportForLow <| BrowserDom.getViewportOf (containerIdToString state_.containerId) )

        ViewportForLow (Ok viewPort) ->
            -- We want to set the calculated height from auto to a pixel value here without animating.
            -- The transition style will not automatically animate from auto to a pixel.
            ( { state_
                | calculatedHeight = Pixel viewPort.scene.height
                , step = AnimateToMin { height = viewPort.scene.height }
              }
            , Cmd.none
            )

        ViewportForLow (Err _) ->
            ( { state_
                | step = ToMin
                , calculatedHeight = Pixel 0
              }
            , Cmd.none
            )

        ToHigh now ->
            case state_.step of
                AnimateToContent scene ->
                    case state_.startTime of
                        Just start ->
                            let
                                ( deltaTime, duration_ ) =
                                    deltaAndDuration state_.adjustment now start state_.duration

                                heightToLow =
                                    calculateHeightToLow
                                        deltaTime
                                        duration_
                                        scene
                                        state_.timing
                                        state_.animationStrategy
                                        state_.calculatedHeight
                                        state_.adjustment
                            in
                            case state_.adjustment of
                                Just (Interrupt Triggered) ->
                                    let
                                        newAdjustment =
                                            Just <| Interrupt (Resolved <| interruptDataToLow now deltaTime 0 heightToLow scene duration_ state_.animationStrategy)
                                    in
                                    ( { state_
                                        | step =
                                            AnimateToMin scene
                                        , calculatedHeight =
                                            heightToLow
                                        , adjustment = newAdjustment
                                        , startTime = Just now
                                      }
                                    , Cmd.none
                                    )

                                Just (Recalc Triggered) ->
                                    ( { state_
                                        | calculatedHeight = state_.calculatedHeight
                                        , adjustment = Just <| Interrupt (Resolved <| interruptDataToHigh now deltaTime state_.calculatedHeight scene state_.duration state_.animationStrategy)
                                        , startTime = Just now
                                      }
                                    , Cmd.none
                                    )

                                _ ->
                                    if deltaTime < round (durationToMillis duration_) then
                                        ( { state_
                                            | calculatedHeight = calculateHeightToHigh deltaTime duration_ scene state_.timing state_.animationStrategy state_.calculatedHeight state_.adjustment
                                          }
                                        , Cmd.none
                                        )

                                    else
                                        ( { state_
                                            | step = ToContent
                                            , startTime = Nothing
                                            , adjustment = Nothing
                                            , calculatedHeight = calculatedHeightWhenHigh scene state_.snapToContent
                                          }
                                        , Cmd.none
                                        )

                        Nothing ->
                            ( { state_
                                | startTime = Just now
                                , calculatedHeight =
                                    calculateHeightToHigh
                                        0
                                        state_.duration
                                        scene
                                        state_.timing
                                        state_.animationStrategy
                                        state_.calculatedHeight
                                        state_.adjustment
                              }
                            , Cmd.none
                            )

                _ ->
                    ( state_, Cmd.none )

        ToLow now ->
            case state_.step of
                AnimateToMin scene ->
                    case state_.startTime of
                        Just start ->
                            let
                                ( deltaTime, duration_ ) =
                                    deltaAndDuration state_.adjustment now start state_.duration

                                calculatedHeightToLow =
                                    calculateHeightToLow
                                        deltaTime
                                        duration_
                                        scene
                                        state_.timing
                                        state_.animationStrategy
                                        state_.calculatedHeight
                                        state_.adjustment

                                isLowest =
                                    case calculatedHeightToLow of
                                        Pixel ch ->
                                            ch == 0

                                        _ ->
                                            False

                                heightToHigh =
                                    calculateHeightToHigh
                                        deltaTime
                                        duration_
                                        scene
                                        state_.timing
                                        state_.animationStrategy
                                        state_.calculatedHeight
                                        state_.adjustment
                            in
                            case state_.adjustment of
                                Just (Interrupt Triggered) ->
                                    let
                                        adjustment =
                                            Just <| Interrupt (Resolved <| interruptDataToHigh now deltaTime heightToHigh scene duration_ state_.animationStrategy)
                                    in
                                    ( { state_
                                        | step = AnimateToContent scene
                                        , calculatedHeight = heightToHigh
                                        , adjustment = adjustment
                                        , startTime = Just now
                                      }
                                    , Cmd.none
                                    )

                                Just (Recalc Triggered) ->
                                    ( { state_
                                        | calculatedHeight = state_.calculatedHeight
                                        , adjustment = Just <| Interrupt (Resolved <| interruptDataToLow now deltaTime scene.height state_.calculatedHeight scene duration_ state_.animationStrategy)
                                        , startTime = Just now
                                      }
                                    , Cmd.none
                                    )

                                _ ->
                                    let
                                        step =
                                            if isLowest then
                                                ToMin

                                            else
                                                ToContent
                                    in
                                    if deltaTime < round (durationToMillis duration_) then
                                        ( { state_
                                            | calculatedHeight = calculatedHeightToLow
                                          }
                                        , Cmd.none
                                        )

                                    else
                                        ( { state_
                                            | step = step
                                            , adjustment = Nothing
                                            , startTime = Nothing
                                            , calculatedHeight =
                                                calculatedHeightToLow
                                          }
                                        , Cmd.none
                                        )

                        Nothing ->
                            ( { state_
                                | startTime = Just now
                                , calculatedHeight =
                                    calculateHeightToLow
                                        0
                                        state_.duration
                                        scene
                                        state_.timing
                                        state_.animationStrategy
                                        state_.calculatedHeight
                                        state_.adjustment
                              }
                            , Cmd.none
                            )

                _ ->
                    ( state_, Cmd.none )
