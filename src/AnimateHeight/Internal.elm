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
    , SceneHeight
    , Step(..)
    , TimingFunction(..)
    , UniqueContainerId
    , buildContainerId
    , calculatedHeightWhenAtContentHeight
    , containerIdToString
    , deltaAndDuration
    , durationToMillis
    , heightRatio
    , heightToContentHeightWhenAnimationFrame
    , heightToContentHeightWhenTransition
    , internalSubs
    , interruptDataToContentHeight
    , interruptDataToMinHeight
    , minHeightWhenAnimationFrame
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
    | AnimateToContent SceneHeight
    | QueryForMin QueryLifeCycle
    | AnimateToMin SceneHeight
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
    = QueryViewportForContentHeight Time.Posix
    | ViewportForContentHeight (Result BrowserDom.Error BrowserDom.Viewport)
    | QueryViewportForRecalc Time.Posix
    | ViewportForRecalc (Result BrowserDom.Error BrowserDom.Viewport)
    | QueryViewportForMinHeight Time.Posix
    | ViewportForMinHeight (Result BrowserDom.Error BrowserDom.Viewport)
    | ToContentHeight Time.Posix
    | ToMinHeight Time.Posix


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


type alias SceneHeight =
    Float



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


calculateMinHeight : Int -> Duration -> SceneHeight -> TimingFunction -> AnimationStrategy -> CalculatedHeight -> Maybe Adjustment -> CalculatedHeight
calculateMinHeight lapsedTime duration_ scene timing strat currentHeight adjustments =
    case strat of
        Transition ->
            minHeightWhenTransition adjustments scene

        AnimationFrame ->
            case adjustments of
                Just (Interrupt Triggered) ->
                    currentHeight

                _ ->
                    minHeightWhenAnimationFrame
                        lapsedTime
                        duration_
                        timing
                        scene
                        adjustments


minHeightWhenTransition : Maybe Adjustment -> SceneHeight -> CalculatedHeight
minHeightWhenTransition adjustment sceneHeight =
    case adjustment of
        Just (Recalc _) ->
            Pixel sceneHeight

        Just (Interrupt (Resolved data)) ->
            Pixel data.targetHeight

        Just (Interrupt Triggered) ->
            Pixel 0

        _ ->
            Pixel 0


heightToContentHeightWhenTransition : CalculatedHeight -> SceneHeight -> CalculatedHeight
heightToContentHeightWhenTransition currentHeight sceneHeight =
    case currentHeight of
        Pixel _ ->
            Pixel sceneHeight

        Auto ->
            Auto


calculatedHeightWhenAtContentHeight : SceneHeight -> Bool -> CalculatedHeight
calculatedHeightWhenAtContentHeight sceneHeight snap =
    if snap then
        Auto

    else
        Pixel sceneHeight


calculateHeightToContentHeight : Int -> Duration -> SceneHeight -> TimingFunction -> AnimationStrategy -> CalculatedHeight -> Maybe Adjustment -> CalculatedHeight
calculateHeightToContentHeight lapsedTime duration_ sceneHeight timing strat currentHeight adjustment =
    case strat of
        Transition ->
            heightToContentHeightWhenTransition currentHeight sceneHeight

        AnimationFrame ->
            case adjustment of
                Just (Interrupt Triggered) ->
                    currentHeight

                _ ->
                    heightToContentHeightWhenAnimationFrame
                        lapsedTime
                        duration_
                        timing
                        sceneHeight
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


heightToContentHeightWhenAnimationFrame :
    Int
    -> Duration
    -> TimingFunction
    -> SceneHeight
    -> CalculatedHeight
    -> Maybe Adjustment
    -> CalculatedHeight
heightToContentHeightWhenAnimationFrame timeLapsed duration_ timing sceneHeight currentHeight adjustment =
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
                            Pixel <| startingHeight + incrementalHeight (sceneHeight - startingHeight)

                        _ ->
                            Pixel <| h + incrementalHeight sceneHeight - h

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


minHeightWhenAnimationFrame :
    Int
    -> Duration
    -> TimingFunction
    -> SceneHeight
    -> Maybe Adjustment
    -> CalculatedHeight
minHeightWhenAnimationFrame timeLapsed duration_ timing sceneHeight adjustment =
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
                    Pixel <| (startingHeight - incrementalHeight (startingHeight - sceneHeight))

                _ ->
                    Pixel <| sceneHeight - incrementalHeight sceneHeight
    in
    calculatedHeight


interruptDataToMinHeight : Time.Posix -> Int -> Float -> CalculatedHeight -> SceneHeight -> Duration -> AnimationStrategy -> InterruptData
interruptDataToMinHeight now dt targetHeight calculatedHeight sceneHeight duration_ animationStrategy =
    case animationStrategy of
        AnimationFrame ->
            case calculatedHeight of
                Pixel ch ->
                    if targetHeight == 0 && sceneHeight == 0 then
                        { startingHeight = ch, duration = durationToMillis duration_, interruptedAt = now, targetHeight = targetHeight }

                    else if targetHeight == 0 then
                        { startingHeight = ch, duration = ch / sceneHeight * durationToMillis duration_, interruptedAt = now, targetHeight = targetHeight }

                    else
                        { startingHeight = ch, duration = sceneHeight / ch * durationToMillis duration_, interruptedAt = now, targetHeight = targetHeight }

                -- This should never be possible
                Auto ->
                    { startingHeight = 0, duration = 0, interruptedAt = now, targetHeight = sceneHeight }

        Transition ->
            transitionInterruptData now dt targetHeight


interruptDataToContentHeight : Time.Posix -> Int -> CalculatedHeight -> SceneHeight -> Duration -> AnimationStrategy -> InterruptData
interruptDataToContentHeight now dt calculatedHeight sceneHeight duration_ animationStrategy =
    case animationStrategy of
        AnimationFrame ->
            case calculatedHeight of
                Pixel ch ->
                    { startingHeight = ch, duration = ((sceneHeight - ch) / sceneHeight) * durationToMillis duration_, interruptedAt = now, targetHeight = sceneHeight }

                -- this should never be possible
                Auto ->
                    { startingHeight = 0, duration = 0, interruptedAt = now, targetHeight = sceneHeight }

        Transition ->
            transitionInterruptData now dt sceneHeight


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
            query QueryViewportForContentHeight

        AnimateToContent _ ->
            animate ToContentHeight

        ToContent ->
            case ( state_.snapToContent, state_.warmUpScene ) of
                ( True, False ) ->
                    Sub.none

                ( _, True ) ->
                    query QueryViewportForContentHeight

                _ ->
                    Sub.none

        QueryForMin QueryViewport ->
            animate QueryViewportForMinHeight

        AnimateToMin _ ->
            animate ToMinHeight

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
                                AnimateToContent <| viewPort.scene.height

                            else if ch == viewPort.scene.height then
                                state_.step

                            else
                                AnimateToMin <| viewPort.scene.height
                    in
                    ( { state_ | step = step }, Cmd.none )

                Auto ->
                    ( state_, Cmd.none )

        ViewportForRecalc (Err _) ->
            ( state_, Cmd.none )

        QueryViewportForContentHeight _ ->
            ( { state_
                | step = QueryForContent QueryResolving
              }
            , Task.attempt ViewportForContentHeight <| BrowserDom.getViewportOf (containerIdToString state_.containerId)
            )

        ViewportForContentHeight (Ok viewPort) ->
            let
                ( step, ch ) =
                    if state_.warmUpScene && state_.snapToContent then
                        ( ToContent, Auto )

                    else if state_.warmUpScene then
                        ( ToContent, Pixel viewPort.scene.height )

                    else if viewPort.scene.height == 0 then
                        ( ToContent, Pixel viewPort.scene.height )

                    else
                        ( AnimateToContent <| viewPort.scene.height, state_.calculatedHeight )
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
        ViewportForContentHeight (Err _) ->
            ( { state_
                | step = ToContent
                , calculatedHeight = Auto
                , warmUpScene = False
              }
            , Cmd.none
            )

        QueryViewportForMinHeight _ ->
            ( { state_ | step = QueryForMin QueryResolving }, Task.attempt ViewportForMinHeight <| BrowserDom.getViewportOf (containerIdToString state_.containerId) )

        ViewportForMinHeight (Ok viewPort) ->
            -- We want to set the calculated height from auto to a pixel value here without animating.
            -- The transition style will not automatically animate from auto to a pixel.
            ( { state_
                | calculatedHeight = Pixel viewPort.scene.height
                , step = AnimateToMin <| viewPort.scene.height
              }
            , Cmd.none
            )

        ViewportForMinHeight (Err _) ->
            ( { state_
                | step = ToMin
                , calculatedHeight = Pixel 0
              }
            , Cmd.none
            )

        ToContentHeight now ->
            case state_.step of
                AnimateToContent scene ->
                    case state_.startTime of
                        Just start ->
                            let
                                ( deltaTime, duration_ ) =
                                    deltaAndDuration state_.adjustment now start state_.duration

                                minHeight =
                                    calculateMinHeight
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
                                            Just <| Interrupt (Resolved <| interruptDataToMinHeight now deltaTime 0 minHeight scene duration_ state_.animationStrategy)
                                    in
                                    ( { state_
                                        | step =
                                            AnimateToMin scene
                                        , calculatedHeight =
                                            minHeight
                                        , adjustment = newAdjustment
                                        , startTime = Just now
                                      }
                                    , Cmd.none
                                    )

                                Just (Recalc Triggered) ->
                                    ( { state_
                                        | calculatedHeight = state_.calculatedHeight
                                        , adjustment = Just <| Interrupt (Resolved <| interruptDataToContentHeight now deltaTime state_.calculatedHeight scene state_.duration state_.animationStrategy)
                                        , startTime = Just now
                                      }
                                    , Cmd.none
                                    )

                                _ ->
                                    if deltaTime < round (durationToMillis duration_) then
                                        ( { state_
                                            | calculatedHeight = calculateHeightToContentHeight deltaTime duration_ scene state_.timing state_.animationStrategy state_.calculatedHeight state_.adjustment
                                          }
                                        , Cmd.none
                                        )

                                    else
                                        ( { state_
                                            | step = ToContent
                                            , startTime = Nothing
                                            , adjustment = Nothing
                                            , calculatedHeight = calculatedHeightWhenAtContentHeight scene state_.snapToContent
                                          }
                                        , Cmd.none
                                        )

                        Nothing ->
                            ( { state_
                                | startTime = Just now
                                , calculatedHeight =
                                    calculateHeightToContentHeight
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

        ToMinHeight now ->
            case state_.step of
                AnimateToMin sceneHeight ->
                    case state_.startTime of
                        Just start ->
                            let
                                ( deltaTime, duration_ ) =
                                    deltaAndDuration state_.adjustment now start state_.duration

                                calculatedMinHeight =
                                    calculateMinHeight
                                        deltaTime
                                        duration_
                                        sceneHeight
                                        state_.timing
                                        state_.animationStrategy
                                        state_.calculatedHeight
                                        state_.adjustment

                                isLowest =
                                    case calculatedMinHeight of
                                        Pixel ch ->
                                            ch == 0

                                        _ ->
                                            False

                                heightToContentHeight =
                                    calculateHeightToContentHeight
                                        deltaTime
                                        duration_
                                        sceneHeight
                                        state_.timing
                                        state_.animationStrategy
                                        state_.calculatedHeight
                                        state_.adjustment
                            in
                            case state_.adjustment of
                                Just (Interrupt Triggered) ->
                                    let
                                        adjustment =
                                            Just <| Interrupt (Resolved <| interruptDataToContentHeight now deltaTime heightToContentHeight sceneHeight duration_ state_.animationStrategy)
                                    in
                                    ( { state_
                                        | step = AnimateToContent sceneHeight
                                        , calculatedHeight = heightToContentHeight
                                        , adjustment = adjustment
                                        , startTime = Just now
                                      }
                                    , Cmd.none
                                    )

                                Just (Recalc Triggered) ->
                                    ( { state_
                                        | calculatedHeight = state_.calculatedHeight
                                        , adjustment = Just <| Interrupt (Resolved <| interruptDataToMinHeight now deltaTime sceneHeight state_.calculatedHeight sceneHeight duration_ state_.animationStrategy)
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
                                            | calculatedHeight = calculatedMinHeight
                                          }
                                        , Cmd.none
                                        )

                                    else
                                        ( { state_
                                            | step = step
                                            , adjustment = Nothing
                                            , startTime = Nothing
                                            , calculatedHeight =
                                                calculatedMinHeight
                                          }
                                        , Cmd.none
                                        )

                        Nothing ->
                            ( { state_
                                | startTime = Just now
                                , calculatedHeight =
                                    calculateMinHeight
                                        0
                                        state_.duration
                                        sceneHeight
                                        state_.timing
                                        state_.animationStrategy
                                        state_.calculatedHeight
                                        state_.adjustment
                              }
                            , Cmd.none
                            )

                _ ->
                    ( state_, Cmd.none )
