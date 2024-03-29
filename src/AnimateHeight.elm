module AnimateHeight exposing
    ( Msg, Config, State, Identifier, Transition(..), init, subscriptions, update
    , auto, fixed, fixedAtAuto, cubicBezier, ease, easeIn, easeInOut, easeOut, container, instant, immediate, rapid, fast, height, heightAt
    , getViewport, identifier, linear, make, animateOpacity, customTiming, content, state
    )

{-| Animate the height of your content.


# Set up

@docs Msg, Config, State, Identifier, Transition, init, subscriptions, update
@docs auto, fixed, fixedAtAuto, cubicBezier, ease, easeIn, easeInOut, easeOut, container, instant, immediate, rapid, fast, height, heightAt
@docs getViewport, identifier, linear, make, animateOpacity, customTiming, content, state

-}

import Browser.Dom as Dom
import Browser.Events as DomEvents
import Html exposing (Html, div)
import Html.Attributes exposing (attribute, id, style)
import Html.Events as Events
import Internal.Internal as Internal
import Json.Decode as Decode
import Json.Encode as Encode
import Platform exposing (Task)
import Task


{-| -}
type Msg
    = AnimationStart
    | AnimationEnd
    | GotContainerViewport (Result Dom.Error Dom.Viewport)
    | SetViewportHeightThenTrigger Float (Result Dom.Error Dom.Viewport)
    | HeightMsg
    | NoOp


{-| -}
type State
    = State_ StateConfig


{-| -}
type Config msg
    = Config (Configuration msg)


type alias Configuration msg =
    { content : List (Html msg)
    , inject : Maybe (Msg -> msg)
    , state : State
    , timing : Internal.TimingFunction
    , duration : Internal.Duration
    , animateOpacity : Bool
    }


type alias StateConfig =
    { targetHeight : Internal.HeightVariant
    , calculatedHeight : Internal.HeightVariant
    , id : Identifier
    , progress : Internal.Progress
    , queriedHeight : Float
    , force : Bool
    , animate : Bool
    }


{-| -}
type Identifier
    = Identifier String


{-| Transitions that are dispatched in sync with the animation lifecycle.

> TransitionStart - Dispatched when an animation starts.

> TransitionEnd - Dispatched when an animation ends.

The Float value is the target pixel height at which the animation will end.

> `TransitionStart` 200 -> The animation has started and will end at 200px

> `TransitionEnd` 200 -> The animation has ended at the height of 200px

-}
type Transition
    = TransitionStart Float
    | TransitionEnd Float


{-| A task for the [Viewport](https://package.elm-lang.org/packages/elm/browser/latest/Browser-Dom#getViewport)
information of the [container](#container).

    getViewport state => Task Error Viewport

-}
getViewport : State -> Task Dom.Error Dom.Viewport
getViewport st =
    Dom.getViewportOf (getIdentifierString st)


{-| The unique id of the container

    yourInit : Model
    yourInit =
        init (identifier "some-unique-id")

-}
identifier : String -> Identifier
identifier str =
    Identifier ("elm-animate-height-container__" ++ str)


defaults : Configuration msg
defaults =
    { content = []
    , inject = Nothing
    , state = init (identifier "unsafe-id")
    , duration = Internal.Fast
    , timing = Internal.Ease
    , animateOpacity = False
    }


{-| A default [Config](#Config)

Argument is a msg that handles [Msg's](#Msg)

    yourView : Html Msg
    yourView =
        container (make AnimateMsg)

-}
make : (Msg -> msg) -> Config msg
make inj =
    Config { defaults | inject = Just inj }



-- CONFIG MODIFIERS


{-| If set to `True` content will fade-in/fade-out while height is animated.

    container
        (make AnimateHeight
            |> animateOpacity True
        )

-}
animateOpacity : Bool -> Config msg -> Config msg
animateOpacity pred (Config config) =
    Config { config | animateOpacity = pred }


{-| This is like not having an animation at all. Duration maps to 0

    yourView : Html msg
    yourView =
        container
            (make AnimateHeight
                |> instant
            )

-}
instant : Config msg -> Config msg
instant (Config config) =
    Config { config | duration = Internal.Instant }


{-| Animation duration of 200ms

    container (make AnimateHeight |> immediate)

-}
immediate : Config msg -> Config msg
immediate (Config config) =
    Config { config | duration = Internal.Immediate }


{-| Animation duration of 250ms

    container (make AnimateHeight |> rapid)

-}
rapid : Config msg -> Config msg
rapid (Config config) =
    Config { config | duration = Internal.Rapid }


{-| Animation duration of 300ms

    container (make AnimateHeight |> fast)

-}
fast : Config msg -> Config msg
fast (Config config) =
    Config { config | duration = Internal.Fast }


{-| Set a custom duration.
Negative values will be converted to their positive equivalent.

custom -333 => 333

    container (make |> customTiming 333)

-}
customTiming : Float -> Config msg -> Config msg
customTiming f (Config config) =
    let
        d =
            if f < 0 then
                f * -1

            else
                f
    in
    Config { config | duration = Internal.Custom d }


{-| The content the [container](#container) will animate.

    yourView : Html Msg
    yourView =
        container
            (make AnimateHeight
                |> content [-- your content]
            )

-}
content : List (Html msg) -> Config msg -> Config msg
content c (Config config) =
    Config { config | content = c }


{-| Sets the State value

    type Model
        = State

    yourView : Model -> Html Msg
    yourView model =
        container
            (make AnimateHeight
                |> state model
            )

-}
state : State -> Config msg -> Config msg
state st (Config config) =
    Config { config | state = st }


{-| [Ease timing](https://developer.mozilla.org/en-US/docs/Web/CSS/animation-timing-function)

    yourView : Html Msg
    yourView =
        container
            (make AnimateHeight
                |> ease
            )

-}
ease : Config msg -> Config msg
ease (Config config) =
    Config { config | timing = Internal.Ease }


{-| [Ease-in timing](https://developer.mozilla.org/en-US/docs/Web/CSS/animation-timing-function)

    container
        (make AnimateHeight
            |> easeIn
        )

-}
easeIn : Config msg -> Config msg
easeIn (Config config) =
    Config { config | timing = Internal.EaseIn }


{-| [Ease-out timing](https://developer.mozilla.org/en-US/docs/Web/CSS/animation-timing-function)

    yourView : Html Msg
    yourView =
        container
            (make AnimateHeight
                |> easeOut
            )

-}
easeOut : Config msg -> Config msg
easeOut (Config config) =
    Config { config | timing = Internal.EaseOut }


{-| [Ease-in-out timing](https://developer.mozilla.org/en-US/docs/Web/CSS/animation-timing-function)

    yourView : Html Msg
    yourView =
        container
            (make AnimateHeight
                |> easeInOut
            )

-}
easeInOut : Config msg -> Config msg
easeInOut (Config config) =
    Config { config | timing = Internal.EaseInOut }


{-| [Linear timing](https://developer.mozilla.org/en-US/docs/Web/CSS/animation-timing-function)

    container
        (make AnimateHeight
            |> linear
        )

-}
linear : Config msg -> Config msg
linear (Config config) =
    Config { config | timing = Internal.Linear }


{-| [Cubic bezier timing](https://developer.mozilla.org/en-US/docs/Web/CSS/animation-timing-function)

    yourView : Model -> Html Msg
    yourView model =
        container
            (make AnimateHeight
                |> cubicBezier 0.1 0.7 1 0.1
            )

-}
cubicBezier : Float -> Float -> Float -> Float -> Config msg -> Config msg
cubicBezier f1 f2 f3 f4 (Config config) =
    Config { config | timing = Internal.CubicBezier (Internal.Bezier f1 f2 f3 f4) }



-- STATE MODIFIERS


{-| Set up an initial state in your init function.

    type alias Model =
        State

    yourInit : Model
    yourInit =
        init (identifier "unique-id")

-}
init : Identifier -> State
init i =
    State_
        { targetHeight = Internal.Fixed 0
        , calculatedHeight = Internal.Fixed 0
        , id = i
        , progress = Internal.Idle
        , queriedHeight = 0
        , force = False
        , animate = True
        }


{-| Set the height of the container to animate to.

This will produce animation events. Checkout [Transition](#Transition)
to find out how transition events work.

    yourUpdate : Msg -> Model -> ( Model, Cmd Msg )
    yourUpdate msg model =
        case msg of
            ShowTheContent ->
                let
                    state =
                        height auto model
                in
                ( state, Cmd.none )

-}
height : Internal.HeightVariant -> State -> State
height t (State_ st) =
    State_ { st | targetHeight = t, force = True, animate = True }


{-| Set the height of the container.

This will not animate or produce animation events. Useful for when you want to set the
starting height of the container without animating to it..

    yourInit : Model
    yourInit =
        init (identifier "unique-id")
            |> heightAt auto

-}
heightAt : Internal.HeightVariant -> State -> State
heightAt t (State_ st) =
    let
        isFixedAtAuto tr f =
            if t == Internal.FixedAtAuto then
                tr

            else
                f
    in
    State_
        { st
            | targetHeight = t
            , calculatedHeight =
                isFixedAtAuto
                    st.calculatedHeight
                    t
            , force =
                -- FixedAtAuto requires a query
                isFixedAtAuto
                    True
                    False
            , animate = False
        }


{-| Will transition to the height of the content.

When the container reaches the content height it will set the height to `auto`.

    type Model
        = State

    update : msg -> Model -> ( Model, Cmd msg )
    update msg model =
        case msg of
            SeeContent ->
                let
                    state =
                        height auto model
                in
                ( state
                , Cmd.none
                )

-}
auto : Internal.HeightVariant
auto =
    Internal.Auto


{-| Will transition to the height you set.

Values translate to px values. e.g. 200 -> 200px

    type Model
        = State

    update : msg -> Model -> ( Model, Cmd msg )
    update msg model =
        case msg of
            SeeContent ->
                let
                    state =
                        height (fixed 200)
                in
                ( state
                , Cmd.none
                )

-}
fixed : Float -> Internal.HeightVariant
fixed =
    Internal.Fixed


{-| Will transition to the fixed height of the container.

This is handy if you want to animate the height of a fixed
container when adding or removing content.

    type Model
        = State

    update : msg -> Model -> ( Model, Cmd msg )
    update msg model =
        case msg of
            ContentRemoved ->
                let
                    state =
                        height fixedAtAuto
                in
                ( state
                , Cmd.none
                )

-}
fixedAtAuto : Internal.HeightVariant
fixedAtAuto =
    Internal.FixedAtAuto


{-| The `AnimateHeight` subscriptions.

    type Model
        = State

    type Msg
        = AnimatetMsg AnimateHeight.Msg

    yourSubs : Model -> Sub Msg
    yourSubs model =
        Sub.map AnimateMsg <|
            subscriptions model

-}
subscriptions : State -> Sub Msg
subscriptions (State_ state_) =
    if state_.force then
        DomEvents.onAnimationFrame (\_ -> HeightMsg)

    else
        Sub.none


{-| Add a branch in your update to handle the `AnimateHeight` Msg's.

    type Model
        = State

    yourUpdate : Msg -> Model -> ( Model, Cmd Msg )
    yourUpdate msg model =
        case msg of
            AnimateMsg animMsg ->
                let
                    ( transition, newState, cmds ) =
                        update animMsg model
                in
                ( newState, Cmd.map AnimateMsg cmds )

-}
update : Msg -> State -> ( Maybe Transition, State, Cmd Msg )
update msg ((State_ state_) as st) =
    let
        (Identifier idString) =
            state_.id
    in
    case msg of
        HeightMsg ->
            case state_.targetHeight of
                Internal.Fixed h ->
                    case state_.calculatedHeight of
                        Internal.Auto ->
                            let
                                queryDomCmd =
                                    -- Setting pixel value from a height of auto will not trigger animation.
                                    -- Instead we need to trigger an animation frame and set the height, then transition.
                                    Task.attempt (SetViewportHeightThenTrigger h) <| Dom.getViewportOf idString
                            in
                            ( Nothing
                            , State_
                                { state_
                                    | force = False
                                }
                            , queryDomCmd
                            )

                        Internal.Fixed _ ->
                            ( Nothing
                            , State_
                                { state_
                                    | calculatedHeight = Internal.Fixed h
                                    , queriedHeight = h
                                    , progress = Internal.Running
                                    , force = False
                                }
                            , Cmd.none
                            )

                        Internal.FixedAtAuto ->
                            let
                                queryDomCmd =
                                    -- Content has probably been added or removed, get a new height to animate to.
                                    Task.attempt (SetViewportHeightThenTrigger h) <| Dom.getViewportOf idString
                            in
                            ( Nothing
                            , State_
                                { state_
                                    | force = False
                                }
                            , queryDomCmd
                            )

                Internal.Auto ->
                    let
                        ( queryDomCmd, resolveProgress ) =
                            getContainerViewport state_
                    in
                    ( Nothing
                    , State_
                        { state_
                            | progress = resolveProgress
                            , force = False
                        }
                    , queryDomCmd
                    )

                Internal.FixedAtAuto ->
                    let
                        ( queryDomCmd, resolveProgress ) =
                            getContainerViewport state_
                    in
                    ( Nothing
                    , State_
                        { state_
                            | progress = resolveProgress
                            , force = False
                        }
                    , queryDomCmd
                    )

        GotContainerViewport (Ok vp) ->
            let
                newCalculatedHeight =
                    Internal.Fixed vp.scene.height
            in
            ( Nothing
            , State_
                { state_
                    | calculatedHeight = newCalculatedHeight
                    , targetHeight =
                        if state_.targetHeight == Internal.FixedAtAuto then
                            Internal.Fixed vp.scene.height

                        else
                            state_.targetHeight
                    , queriedHeight = vp.scene.height
                    , progress =
                        case state_.calculatedHeight of
                            Internal.Auto ->
                                -- Going from Auto to Fixed will not produce transitions.
                                -- SetViewportHeightThenTrigger handles that.
                                Internal.Idle

                            _ ->
                                if state_.animate then
                                    state_.progress

                                else
                                    Internal.Idle
                }
            , Cmd.none
            )

        SetViewportHeightThenTrigger h (Ok vp) ->
            ( Nothing
            , State_
                { state_
                    | calculatedHeight = Internal.Fixed vp.scene.height
                    , targetHeight = Internal.Fixed h
                    , queriedHeight = vp.scene.height
                    , force = True
                }
            , Cmd.none
            )

        SetViewportHeightThenTrigger _ (Err _) ->
            ( Nothing, st, Cmd.none )

        GotContainerViewport (Err _) ->
            ( Nothing, st, Cmd.none )

        AnimationStart ->
            ( Just (TransitionStart state_.queriedHeight)
            , State_ { state_ | progress = Internal.Running }
            , Cmd.none
            )

        AnimationEnd ->
            let
                ( resolveCalculatedHeight, queriedHeight ) =
                    case state_.targetHeight of
                        Internal.Auto ->
                            ( Internal.Auto, state_.queriedHeight )

                        Internal.Fixed fh ->
                            ( Internal.Fixed fh, fh )

                        Internal.FixedAtAuto ->
                            ( Internal.Fixed state_.queriedHeight, state_.queriedHeight )
            in
            ( Just (TransitionEnd queriedHeight)
            , State_
                { state_
                    | progress = Internal.Idle
                    , calculatedHeight = resolveCalculatedHeight
                    , queriedHeight = queriedHeight
                }
            , Cmd.none
            )

        NoOp ->
            ( Nothing, st, Cmd.none )


{-| The container that wraps your view

The example shows a container with nothing in it.

    yourView : Html Msg
    yourView =
        container (make AnimateHeight)

-}
container : Config msg -> Html msg
container (Config config) =
    let
        (Identifier id_) =
            state_.id

        (State_ state_) =
            config.state

        resolveHeight =
            case state_.calculatedHeight of
                Internal.Auto ->
                    "auto"

                Internal.Fixed fh ->
                    String.fromFloat fh ++ "px"

                -- FixedAtAuto should never make it to the view,
                -- it is set as Fixed. Fall back to auto.
                Internal.FixedAtAuto ->
                    "auto"

        resolveTransitionMsgs =
            case config.inject of
                Just msg ->
                    [ Events.on "transitionstart" (Decode.succeed (msg AnimationStart))
                    , Events.on "transitionend" (Decode.succeed (msg AnimationEnd))
                    ]

                _ ->
                    []

        resolveAnimationPlayState =
            case state_.progress of
                Internal.Running ->
                    [ style "animation-play-state" "running" ]

                _ ->
                    []

        resolveTransitionDuration =
            [ style "transition-duration"
                (if state_.animate then
                    (Internal.durationToMillis config.duration
                        |> String.fromFloat
                    )
                        ++ "ms"

                 else
                    (Internal.durationToMillis Internal.Instant
                        |> String.fromFloat
                    )
                        ++ "ms"
                )
            ]

        resolveTiming =
            [ style "transition-timing-function" (Internal.timingToCssValue config.timing) ]

        resolveContentOpacity =
            if config.animateOpacity then
                case state_.calculatedHeight of
                    Internal.Fixed fh ->
                        if
                            fh
                                <= 0
                                && (state_.progress
                                        == Internal.Running
                                        || (state_.progress == Internal.Idle)
                                        || (state_.progress == Internal.Preparing)
                                   )
                        then
                            [ style "opacity" "0"
                            ]

                        else
                            [ style "opacity" "1" ]

                    _ ->
                        [ style "opacity" "1" ]

            else
                [ style "opacity" "1" ]

        resolvePropagation =
            case config.inject of
                Just inj ->
                    [ Events.stopPropagationOn "transitionend" (Decode.succeed ( inj NoOp, True ))
                    , Events.stopPropagationOn "transitionstart" (Decode.succeed ( inj NoOp, True ))
                    ]

                _ ->
                    []

        resolveAccessibilityValues =
            if config.animateOpacity then
                case state_.calculatedHeight of
                    Internal.Fixed fh ->
                        if
                            fh
                                <= 0
                                && state_.progress
                                == Internal.Idle
                        then
                            [ attribute "aria-hidden" (Encode.encode 0 <| Encode.bool True)
                            , style "display" "none"
                            ]

                        else
                            []

                    _ ->
                        []

            else
                []

        resolveOverflow =
            case state_.calculatedHeight of
                Internal.Auto ->
                    [ style "overflow" "visible" ]

                _ ->
                    []
    in
    div
        ([ style "height" resolveHeight
         , style "transition-property" "height"
         , style "overflow" "hidden"
         , style "position" "relative"
         , attribute "data-test-id" "animate-height-container"
         ]
            ++ resolveTransitionMsgs
            ++ resolveAnimationPlayState
            ++ resolveTransitionDuration
            ++ resolveTiming
            ++ resolveOverflow
        )
        [ div
            ([ id id_
             , style "transition-property" "opacity"
             , style "transition-timing-function" "ease-in"
             , attribute "data-test-id" "animate-height-content"
             ]
                ++ resolveContentOpacity
                ++ resolvePropagation
                ++ resolveTransitionDuration
                ++ resolveAccessibilityValues
            )
            config.content
        ]



-- UTILS


getIdentifierString : State -> String
getIdentifierString (State_ state_) =
    let
        (Identifier id_) =
            state_.id
    in
    id_


getContainerViewport : StateConfig -> ( Cmd Msg, Internal.Progress )
getContainerViewport state_ =
    let
        (Identifier idString) =
            state_.id

        queryDomCmd =
            Task.attempt GotContainerViewport <| Dom.getViewportOf idString

        resolveProgress =
            case state_.calculatedHeight of
                Internal.Fixed fh ->
                    if fh <= 0 || state_.targetHeight == Internal.FixedAtAuto then
                        Internal.Preparing

                    else
                        Internal.Idle

                Internal.Auto ->
                    if state_.targetHeight == Internal.FixedAtAuto then
                        Internal.Preparing

                    else
                        Internal.Idle

                _ ->
                    Internal.Idle
    in
    ( queryDomCmd, resolveProgress )
