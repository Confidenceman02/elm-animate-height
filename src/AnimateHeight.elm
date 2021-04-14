module AnimateHeight exposing
    ( State, subscription, container, animationFrame, transition, initialState, instant, immediate, rapid, fast, custom, content, default, setAtContentHeight, snapToContent, state, uniqueContainerId
    , toContentHeight, toMinHeight, Msg, recalculate, isContentHeight, update
    )

{-| Animate a containers height.


# Set up

@docs State, subscription, container, animationFrame, transition, initialState, instant, immediate, rapid, fast, custom, content, default, setAtContentHeight, snapToContent, state, uniqueContainerId


# Trigger an animation

We avoid using terms such as 'open' and 'closed' when animating height.

Instead we say that we are animating to a content height or minimum height which is more inline to what you are seeing.

@docs toContentHeight, toMinHeight, Msg, recalculate, isContentHeight, update

-}

import AnimateHeight.Internal
    exposing
        ( Adjustment(..)
        , AdjustmentAction(..)
        , AnimationState
        , AnimationStrategy(..)
        , Bezier(..)
        , CalculatedHeight(..)
        , Duration(..)
        , InternalMsg(..)
        , QueryLifeCycle(..)
        , Step(..)
        , TimingFunction(..)
        , UniqueContainerId
        , buildContainerId
        , containerIdToString
        , durationToMillis
        , internalSubs
        , updateAnimateHeight
        )
import Css as Css
import Css.Transitions as Transitions
import Html exposing (Html, text)
import Html.Lazy exposing (lazy)
import Html.Styled as Styled
import Html.Styled.Attributes as StyledAttribs


type Config msg
    = Config (Configuration msg)


{-| -}
type Msg
    = Internal InternalMsg


{-| -}
type State
    = State AnimationState


{-| The subscriptions `AnimateHeight` uses to function.

        type Msg =
            AnimateHeightMsg AnimateHeight.Msg

        subscriptions =
            Sub.map AnimateHeightMsg <| AnimateHeight.subscription animState

-}
subscription : State -> Sub Msg
subscription (State state_) =
    Sub.map Internal (internalSubs state_)


{-| The container view that wraps your view.

        someCoolView =
            text "This is pretty cool!"

        view =
            container (default
                |> content someCoolView
            )

-}
container : Config msg -> Html msg
container (Config config) =
    lazy viewContainer config


viewContainer : Configuration msg -> Html msg
viewContainer config =
    let
        (State state_) =
            config.state

        transitionAttribs =
            if state_.animationStrategy == Transition then
                setTransitions state_

            else
                []
    in
    Styled.toUnstyled <|
        Styled.div
            [ StyledAttribs.css ([ Css.overflow Css.hidden, calculatedHeightToAttribute state_.calculatedHeight ] ++ transitionAttribs)
            ]
            [ Styled.fromUnstyled config.content, sizerContainer state_.containerId config.content ]


sizerContainer : UniqueContainerId -> Html msg -> Styled.Html msg
sizerContainer id_ c =
    Styled.div
        [ StyledAttribs.id (containerIdToString id_)
        , StyledAttribs.css
            [ Css.height (Css.px 0)
            , Css.overflow Css.hidden
            , Css.visibility Css.hidden
            , Css.top (Css.px 0)
            ]
        ]
        [ Styled.fromUnstyled c ]



-- UPDATE


{-| Update `AnimateHeight` `container`.

        type Msg =
            AnimateHeightMsg AnimateHeight.Msg

        update =
            case msg of
                AnimateHeightMsg animMsg ->
                    AnimateHeight.update animMsg animState

-}
update : Msg -> State -> ( State, Cmd Msg )
update msg (State state_) =
    case msg of
        Internal internalMsg ->
            let
                ( interrState, interrCmd ) =
                    updateAnimateHeight internalMsg state_
            in
            ( State interrState, Cmd.map Internal interrCmd )



-- CALCULATE HEIGHT HELPERS


timingToCssTimingFunction timingFunction =
    case timingFunction of
        Ease ->
            Transitions.ease

        Linear ->
            Transitions.linear

        EaseIn ->
            Transitions.easeIn

        EaseOut ->
            Transitions.easeOut

        EaseInOut ->
            Transitions.easeInOut

        CubicBezier (Bezier float1 float2 float3 float4) ->
            Transitions.cubicBezier float1 float2 float3 float4


calculatedHeightToAttribute : CalculatedHeight -> Css.Style
calculatedHeightToAttribute ch =
    case ch of
        Auto ->
            Css.height Css.auto

        Pixel v ->
            Css.height <| Css.px v



-- HELPERS


{-| AnimateHeight queries this id to find out height information.

If there are multiple containers then it is important that this ID be unique.

        init =
           { container1 = initialState <| uniqueContainerId "111"
           , container2 = initialState <| uniqueContainerId "222"
           }

-}
uniqueContainerId : String -> UniqueContainerId
uniqueContainerId id =
    buildContainerId id


setTransitions : AnimationState -> List Css.Style
setTransitions state_ =
    let
        duration_ =
            case state_.adjustment of
                Just (Interrupt (Resolved data)) ->
                    data.duration

                _ ->
                    durationToMillis state_.duration
    in
    case state_.step of
        AnimateToContent _ ->
            [ Transitions.transition <| [ Transitions.height3 duration_ 0 (timingToCssTimingFunction state_.timing) ]
            ]

        AnimateToMin _ ->
            [ Transitions.transition <| [ Transitions.height3 duration_ 0 (timingToCssTimingFunction state_.timing) ]
            ]

        QueryForRecalc _ ->
            [ Transitions.transition <| [ Transitions.height3 duration_ 0 (timingToCssTimingFunction state_.timing) ]
            ]

        _ ->
            []



-- STATE MODIFIERS


{-| By default the `container` height will be given a fixed value when it is high. In this scenario, content that is
added/removed will not adjust the containers height. See `snapToContent` for another option.

When your view content changes you can recalculate the container height.

This function makes the most sense to use when the container is already in a high position.

    update : msg -> model -> ( model, Cmd msg )
    update msg model =
        case msg of
            ContentAdded ->
                newState =
                    if isContentHeight model.animateHeightState then
                        recalculate model.animateHeightState
                    else
                        model.animateHeightState

-}
recalculate : State -> State
recalculate ((State state_) as s) =
    case state_.calculatedHeight of
        Pixel ch ->
            case state_.animationStrategy of
                AnimationFrame ->
                    case state_.step of
                        -- its more of a QueryForHigh than a Recalc
                        ToMin ->
                            toContentHeight s

                        AnimateToContent _ ->
                            State { state_ | step = QueryForRecalc QueryViewport, adjustment = Just <| Recalc Triggered }

                        AnimateToMin _ ->
                            State { state_ | step = QueryForRecalc QueryViewport, adjustment = Just <| Recalc Triggered }

                        _ ->
                            State { state_ | step = QueryForRecalc QueryViewport, adjustment = Just <| Recalc (Resolved ch) }

                Transition ->
                    case state_.step of
                        ToContent ->
                            State { state_ | step = QueryForRecalc QueryViewport, adjustment = Just <| Recalc (Resolved ch) }

                        ToMin ->
                            State { state_ | step = QueryForRecalc QueryViewport, adjustment = Just <| Recalc (Resolved ch) }

                        _ ->
                            State { state_ | step = QueryForRecalc QueryViewport, adjustment = Just <| Recalc Triggered }

        _ ->
            State state_


{-| The `container` will snap to a height subsequent to its changing content.

All that is happening internally is that the height value of the container is being set
to auto when the `container` is high.

        init =
            initialState (uniqueContainerId "id")
                |> snapToContent True

-}
snapToContent : Bool -> State -> State
snapToContent snap (State state_) =
    State { state_ | snapToContent = snap }


{-| For when you want to start the `container` off at a its content height by default and not
trigger an animation.

One use case could be when a page loads and you want all your `AnimateHeight` `containers`
to show their content by default.

You can use `setAtContentHeight` in init.

    init =
        initialState (uniqueContainerId "id")
            |> setAtContentHeight

-}
setAtContentHeight : State -> State
setAtContentHeight (State animState) =
    State { animState | step = ToContent, calculatedHeight = Auto, warmUpScene = True }


{-| Animate a container to its content height.

        update msg =
            case msg of
                ToggleContainer ->
                    toContentHeight animState

You might want to use `isContentHeight` to toggle between animating toContentHeight and toMinHeight.

        update msg =
            case msg of
                ToggleContainer ->
                    if (isContentHeight animState) then
                        toMinHeight animState
                    else
                        toContentHeight animState

-}
toContentHeight : State -> State
toContentHeight (State state_) =
    case state_.step of
        AnimateToMin _ ->
            State { state_ | adjustment = Just <| Interrupt Triggered }

        AnimateToContent _ ->
            State { state_ | adjustment = Just <| Interrupt Triggered }

        _ ->
            State { state_ | step = QueryForContent QueryViewport }


{-| Animate a container to its minimum height.

        update msg =
            case msg of
                ToggleContainer ->
                    toMinHeight animState

You might want to use `isContentHeight` to toggle between animating toMinHeight and toContentHeight.

        update msg =
            case msg of
                ToggleContainer ->
                    if (isContentHeight animState) then
                        toMinHeight animState
                    else
                        toContentHeight animState

-}
toMinHeight : State -> State
toMinHeight (State state_) =
    case state_.step of
        AnimateToContent _ ->
            State { state_ | adjustment = Just <| Interrupt Triggered }

        AnimateToMin _ ->
            State { state_ | adjustment = Just <| Interrupt Triggered }

        _ ->
            State { state_ | step = QueryForMin QueryViewport }


{-| Determine if a container is high.

Useful for when you need to toggle a `container` high and low.

        update msg =
            case msg of
                ToggleContainer ->
                    if (isContentHight animState) then
                        toMinHeight animState
                    else
                        toContentHeight animState

-}
isContentHeight : State -> Bool
isContentHeight (State state_) =
    case state_.step of
        ToContent ->
            True

        _ ->
            False


{-| `AnimationFrame` calculates the height of the container per animation frame. By doing
this we are ensuring that all calculations are resolved before the browser refresh
operations. The animation if fully managed by us, no magic!

        init =
            initialState (containerUniqueId "123") default
                |> animationFrame

-}
animationFrame : State -> State
animationFrame (State state_) =
    State { state_ | animationStrategy = AnimationFrame }


{-| `Transition` avoids view updates and leverages css transitions to manage the animation.

        init =
            initialState (containerUniqueId "123") default
                |> transition

-}
transition : State -> State
transition (State state_) =
    State { state_ | animationStrategy = Transition }


{-| This is like not having an animation at all. Duration maps to 0

        init =
            initialState (containerUniqueId "123")
                |> instant

-}
instant : State -> State
instant (State state_) =
    State { state_ | duration = Instant }


{-| Animation duration of 200ms

        init =
            initialState (containerUniqueId "123")
                |> immediate

-}
immediate : State -> State
immediate (State state_) =
    State { state_ | duration = Immediate }


{-| Animation duration of 250ms

        init =
            initialState (containerUniqueId "123")
                |> rapid

-}
rapid : State -> State
rapid (State state_) =
    State { state_ | duration = Rapid }


{-| Animation duration of 300ms

        init =
            initialState (containerUniqueId "123")
                |> fast

-}
fast : State -> State
fast (State state_) =
    State { state_ | duration = Fast }


{-| Set a custom duration.

Negative values will be converted to their positive equivalent.

        custom -200 => 200

        init =
            initialState (containerUniqueId "123")
                |> custom 1000

-}
custom : Float -> State -> State
custom f (State state_) =
    let
        d =
            if f < 0 then
                f * -1

            else
                f
    in
    State { state_ | duration = Custom d }


{-| Use as an initial state for your `AnimateHeight` `container`.

It's important to ensure the unique id argument is in fact unique
to avoid weird behaviour. Internally we add some extra characters to your id to help you out.

        init =
            initialState (containerUniqueId "123")

-}
initialState : UniqueContainerId -> State
initialState containerId =
    State
        { step = ToMin
        , containerId = containerId
        , duration = Fast
        , timing = Ease
        , calculatedHeight = Pixel 0
        , startTime = Nothing
        , adjustment = Nothing
        , snapToContent = False
        , warmUpScene = False
        , animationStrategy = Transition
        }



-- CONFIGURATION TYPES


type alias Configuration msg =
    { content : Html msg
    , state : State
    }



-- CONFIG MODIFIERS


{-| A starting configuration.

        view =
            container default

-}
default : Config msg
default =
    Config
        { content = text ""
        , state = initialState (uniqueContainerId "animate-height-container")
        }


{-| Render your view.

        coolView =
            text "A cool view!"

        view =
           container (default
                |> content coolView
           )

-}
content : Html msg -> Config msg -> Config msg
content content_ (Config config) =
    Config { config | content = content_ }


{-| The working state for your container.

        view model =
            container (default
                |> state model.containerState
            )

-}
state : State -> Config msg -> Config msg
state state_ (Config config) =
    Config { config | state = state_ }
