module AnimateHeight.Variant.Switch exposing
    ( Msg, Config, State, Identifier, Transition(..), init, subscriptions, update, make
    , cubicBezier, ease, easeIn, easeInOut, easeOut, linear, fixView, toView, view
    , identifier
    )

{-| A slick variant that animates height when switching between your views.


# Set up

@docs Msg, Config, State, Identifier, Transition, init, subscriptions, update, make
@docs cubicBezier, ease, easeIn, easeInOut, easeOut, linear, fixView, toView, view
@docs identifier

-}

import AnimateHeight
import Browser.Dom as Dom
import Browser.Events as DomEvents
import Html exposing (Attribute, Html, div, text)
import Html.Attributes exposing (attribute, style)
import Html.Events as HtmlEvents
import Internal.Internal as Internal
import Json.Decode as Decode
import Task


{-| -}
type Msg
    = GhostNoOp AnimateHeight.Msg
    | AnimateHeightMsg AnimateHeight.Msg
    | PrepareGhostMsg
    | SwitchView
    | AnimationEnd
    | GotQueries (Result Dom.Error ( Dom.Viewport, Dom.Viewport ))
    | AnimateTo Float


{-| -}
type State view
    = State (StateConfiguration view)


type alias StateConfiguration view =
    { currentView : Maybe view
    , incomingView : Maybe (IncomingView view)
    , ahState : AnimateHeight.State
    , ghostState : GhostState
    , id : Identifier
    }


{-| -}
type Identifier
    = Identifier String


type GhostState
    = GhostState AnimateHeight.State


type IncomingView view
    = Ghost view
    | PreparingGhost view
    | Entering view
    | Entered view
    | PrepareExit
    | Exiting


{-| -}
type Config msg
    = Config (Configuration msg)


type alias Configuration msg =
    { inject : Msg -> msg
    , timing : Internal.TimingFunction
    , duration : Internal.Duration
    }


{-| Transitions that are dispatched in sync with the animation lifecycle.

`TransitionStart` - Dispatched when an animation starts.

`TransitionEnd` - Dispatched when an animation ends.

The parameter on each type represents the target view.

`TransitionStart view1` -> The transition has started and will switch from the current view to the `view1` view.

`TransitionEnd view1` -> The transition has ended and the `view1` view is rendered.

    type YourViews
      = View1
      | View2

    yourUpdate msg yourModel =
      case msg of
        SwitchMsg msg ->
          let
              (maybeTransition, updatedState, cmds) =
                  update msg yourModel.switchState

              handleTransition =
                case maybeTransition of
                  Just ( TransitionStart View1 ) ->
                    -- Do something

                  Just (TransitionEnd View2) ->
                    -- Do something
          in
          ({model | switchState = updatedState}, Cmd.map SwitchMsg cmds)

-}
type Transition view
    = TransitionStart view
    | TransitionEnd view


{-| A unique indentifier that Switch uses internally.

NOTE: The String passed must be unique.

-}
identifier : String -> Identifier
identifier =
    String.split " "
        >> String.concat
        >> (++) "animate-height-variant-switch"
        >> Identifier


{-| Initialize a Switch [State](#State)

NOTE: Ensure you store the [State](#State) structure in your model.

    type YourView
        = View1
        | View2

    type alias Model =
        { switchState : State YourView
        }

    yourInit =
        { switchState = init (identifier "some-unique-string") }

-}
init : Identifier -> State view
init ((Identifier i) as switchId) =
    State
        { currentView = Nothing
        , incomingView = Nothing
        , ahState = AnimateHeight.init (AnimateHeight.identifier i)
        , ghostState = GhostState <| AnimateHeight.init (AnimateHeight.identifier (i ++ "switch-ghost"))
        , id = switchId
        }


{-| A default [Config](#Config)

Argument is a msg that handles [Msg's](#Msg)

    switchConfig : Config YourViews
    switchConfig =
        make SwitchMsg

-}
make : (Msg -> msg) -> Config msg
make inject =
    Config
        { inject = inject
        , duration = Internal.Fast
        , timing = Internal.Ease
        }



-- STATE MODIFIERS


{-| Set the view without transitions.

Useful for when you want to set an initial view without animating to it.

    type YourView
        = View1
        | View2

    yourInit : Model
    yourInit =
        init (identifier "unique-id")
            |> fixView View1

-}
fixView : view -> State view -> State view
fixView v (State state_) =
    State
        { state_
            | currentView = Just v
            , incomingView = Nothing
            , ahState =
                AnimateHeight.heightAt
                    AnimateHeight.auto
                    state_.ahState
        }


{-| Switch from a current view to a new view.

If no current view exists the incoming view will still transition in.

    type alias Model =
        { switchState : State }

    type YourMsg
        = ChangeView

    yourUpdate model msg =
        case msg of
            ChangeView ->
                let
                    newState =
                        toView (Just View2) model.switchState
                in
                ( { model | switchState = newState }, Cmd.none )

-}
toView : Maybe view -> State view -> State view
toView vw (State state_) =
    case state_.incomingView of
        Just (Entering _) ->
            State state_

        _ ->
            case ( vw, state_.currentView ) of
                ( Just v, Just _ ) ->
                    State { state_ | incomingView = Just (Ghost v) }

                ( Nothing, Just _ ) ->
                    State
                        { state_
                            | incomingView = Just PrepareExit
                        }

                ( Just v, Nothing ) ->
                    State { state_ | incomingView = Just (Ghost v) }

                ( Nothing, Nothing ) ->
                    State state_



-- MODIFIERS


{-| [Ease timing](https://developer.mozilla.org/en-US/docs/Web/CSS/animation-timing-function)

     make SwitchView
          |> ease

-}
ease : Config msg -> Config msg
ease (Config config) =
    Config { config | timing = Internal.Ease }


{-| [Ease-in timing](https://developer.mozilla.org/en-US/docs/Web/CSS/animation-timing-function)

    make SwitchView
        |> easeIn

-}
easeIn : Config msg -> Config msg
easeIn (Config config) =
    Config { config | timing = Internal.EaseIn }


{-| [Ease-out timing](https://developer.mozilla.org/en-US/docs/Web/CSS/animation-timing-function)

      make SwitchView
          |> easeOut

-}
easeOut : Config msg -> Config msg
easeOut (Config config) =
    Config { config | timing = Internal.EaseOut }


{-| [Ease-in-out timing](https://developer.mozilla.org/en-US/docs/Web/CSS/animation-timing-function)

    make SwitchView
        |> easeInOut

-}
easeInOut : Config msg -> Config msg
easeInOut (Config config) =
    Config { config | timing = Internal.EaseInOut }


{-| [Linear timing](https://developer.mozilla.org/en-US/docs/Web/CSS/animation-timing-function)

    make SwitchView
        |> linear

-}
linear : Config msg -> Config msg
linear (Config config) =
    Config { config | timing = Internal.Linear }


{-| [Cubic bezier timing](https://developer.mozilla.org/en-US/docs/Web/CSS/animation-timing-function)

    make SwitchView
        |> cubicBezier 0.1 0.7 1 0.1

-}
cubicBezier : Float -> Float -> Float -> Float -> Config msg -> Config msg
cubicBezier f1 f2 f3 f4 (Config config) =
    Config { config | timing = Internal.CubicBezier (Internal.Bezier f1 f2 f3 f4) }


{-| Subcriptions for the switch.

    type alias Model =
        { switchState : State }

    type Msg
        = SwitchMsg Switch.Msg

    yourSubs : Model -> Sub Msg
    yourSubs model =
        Sub.map SwitchMsg <|
            subscriptions model.switchState

-}
subscriptions : State view -> Sub Msg
subscriptions (State state_) =
    Sub.batch
        [ Sub.map AnimateHeightMsg (AnimateHeight.subscriptions state_.ahState)
        , case state_.incomingView of
            Just (Ghost _) ->
                DomEvents.onAnimationFrame (\_ -> PrepareGhostMsg)

            Just (Entered _) ->
                DomEvents.onAnimationFrame (\_ -> SwitchView)

            Just PrepareExit ->
                DomEvents.onAnimationFrame (\_ -> AnimateTo 0)

            _ ->
                Sub.none
        ]


{-| Add a branch in your update to handle the `Switch` Msg's.

    type alias Model =
        { switchState : State }

    type Msg
        = SwitchMsg

    yourUpdate : Msg -> Model -> ( Model, Cmd Msg )
    yourUpdate msg model =
        case msg of
            SwitchMsg switchMsg ->
                let
                    ( transition, newState, cmds ) =
                        update switchMsg model.switchState
                in
                ( { model | switchState = newState }
                , Cmd.map SwitchMsg cmds
                )

-}
update : Msg -> State view -> ( Maybe (Transition view), State view, Cmd Msg )
update msg1 ((State state_) as st) =
    case msg1 of
        SwitchView ->
            case state_.incomingView of
                Just (Entered v) ->
                    ( Nothing
                    , State
                        { state_
                            | incomingView = Nothing
                            , currentView = Just v
                            , ahState =
                                AnimateHeight.heightAt
                                    AnimateHeight.auto
                                    state_.ahState
                        }
                    , Cmd.none
                    )

                _ ->
                    ( Nothing, State { state_ | incomingView = Nothing }, Cmd.none )

        AnimationEnd ->
            case state_.incomingView of
                Just (Entering v) ->
                    ( Nothing, State { state_ | incomingView = Just (Entered v) }, Cmd.none )

                _ ->
                    ( Nothing, st, Cmd.none )

        AnimateHeightMsg msg2 ->
            let
                ( trans, updatedState, cmds ) =
                    AnimateHeight.update msg2 state_.ahState
            in
            case trans of
                Just (AnimateHeight.TransitionEnd _) ->
                    case state_.incomingView of
                        Just Exiting ->
                            ( Nothing
                            , State
                                { state_
                                    | incomingView = Nothing
                                    , currentView = Nothing
                                    , ahState = updatedState
                                }
                            , Cmd.map AnimateHeightMsg cmds
                            )

                        _ ->
                            ( Nothing
                            , State
                                { state_
                                    | ahState =
                                        updatedState
                                }
                            , Cmd.map AnimateHeightMsg cmds
                            )

                _ ->
                    ( Nothing
                    , State
                        { state_
                            | ahState =
                                updatedState
                        }
                    , Cmd.map AnimateHeightMsg cmds
                    )

        PrepareGhostMsg ->
            let
                resolveIncoming =
                    case state_.incomingView of
                        Just (Ghost v) ->
                            Just v

                        _ ->
                            Nothing

                (GhostState gs) =
                    state_.ghostState

                ghostContainer =
                    AnimateHeight.getViewport gs
            in
            case ( resolveIncoming, state_.currentView ) of
                ( Just v, Nothing ) ->
                    ( Nothing
                    , State
                        { state_
                            | incomingView = Nothing
                            , currentView = Just v
                            , ahState =
                                AnimateHeight.height
                                    AnimateHeight.fixedAtAuto
                                    state_.ahState
                        }
                    , Cmd.none
                    )

                ( Just v1, Just v2 ) ->
                    -- Don't bother doing any work if the incoming view is the same as the current view.
                    if v1 == v2 then
                        ( Nothing, State { state_ | incomingView = Nothing }, Cmd.none )

                    else
                        let
                            queryAhContainer =
                                AnimateHeight.getViewport state_.ahState
                        in
                        ( Nothing
                        , State
                            { state_
                                | incomingView = Just (PreparingGhost v1)
                            }
                        , Cmd.batch
                            [ Task.attempt GotQueries
                                (Task.map2 (\a b -> ( a, b )) queryAhContainer ghostContainer)
                            ]
                        )

                ( Nothing, _ ) ->
                    ( Nothing, State { state_ | incomingView = Nothing }, Cmd.none )

        GotQueries (Ok ( containerElem, ghost )) ->
            ( Nothing
            , State
                { state_
                    | ahState =
                        -- Set the height to a fixed value before animating to incoming height.
                        -- Animating from auto doesn't work.
                        AnimateHeight.heightAt
                            (AnimateHeight.fixed containerElem.viewport.height)
                            state_.ahState
                }
            , Task.perform AnimateTo (Task.succeed ghost.scene.height)
            )

        GotQueries (Err _) ->
            ( Nothing, State { state_ | incomingView = Nothing }, Cmd.none )

        AnimateTo h ->
            case state_.incomingView of
                Just (PreparingGhost v) ->
                    ( Nothing
                    , State
                        { state_
                            | incomingView =
                                Just (Entering v)
                            , ahState =
                                AnimateHeight.height
                                    (AnimateHeight.fixed h)
                                    state_.ahState
                        }
                    , Cmd.none
                    )

                Just PrepareExit ->
                    ( Nothing
                    , State
                        { state_
                            | ahState =
                                AnimateHeight.height
                                    (AnimateHeight.fixed h)
                                    state_.ahState
                            , incomingView = Just Exiting
                        }
                    , Cmd.none
                    )

                _ ->
                    ( Nothing, State state_, Cmd.none )

        _ ->
            ( Nothing, st, Cmd.none )


{-| Render the switch view

    type YourView
        = View1
        | View2
        | View3

    viewResolver : YourView -> List (Html msg)
    viewResolver v =
        case v of
            View1 ->
                [ text "View 1" ]

            View2 ->
                [ text "View 2" ]

            View3 ->
                [ text "View 3" ]

    yourView : Model -> Html Msg
    yourView model =
        view viewResolver (make SwitchView) model.switchView

-}
view : (view -> List (Html msg)) -> Config msg -> State view -> Html msg
view fn (Config config) (State state_) =
    div []
        [ AnimateHeight.container
            (AnimateHeight.make (AnimateHeightMsg >> config.inject)
                |> AnimateHeight.state state_.ahState
                |> AnimateHeight.content
                    (case state_.currentView of
                        Just v ->
                            let
                                resolveTiming =
                                    Internal.timingToCssValue config.timing

                                ( incomingStyles, currentStyles ) =
                                    case state_.incomingView of
                                        Just (PreparingGhost _) ->
                                            ( [ style "opacity" "0" ], [ style "opacity" "1" ] )

                                        Just (Entering _) ->
                                            ( [ style "opacity" "1" ], [ style "opacity" "0" ] )

                                        Just (Entered _) ->
                                            ( [], [ style "display" "none" ] )

                                        Just Exiting ->
                                            ( [], [ style "opacity" "0" ] )

                                        Nothing ->
                                            ( [], [] )

                                        _ ->
                                            ( [], [] )
                            in
                            [ div [ style "position" "relative" ]
                                [ case state_.incomingView of
                                    Just (PreparingGhost v1) ->
                                        viewIncoming incomingStyles
                                            config.inject
                                            config.timing
                                            config.duration
                                            (fn v1)

                                    Just (Entering v1) ->
                                        viewIncoming incomingStyles
                                            config.inject
                                            config.timing
                                            config.duration
                                            (fn v1)

                                    Just (Entered v1) ->
                                        div
                                            [ style "position" "absolute"
                                            ]
                                            (fn v1)

                                    _ ->
                                        text ""
                                , div
                                    ([ style "transition-property" "opacity"
                                     , style "transition-timing-function" resolveTiming
                                     , style "transition-duration" "200ms"
                                     ]
                                        ++ currentStyles
                                    )
                                    (fn v)
                                ]
                            ]

                        _ ->
                            []
                    )
            )
        , case state_.incomingView of
            Just (Ghost v) ->
                viewGhost fn v config.inject state_.ghostState

            Just (PreparingGhost v) ->
                viewGhost fn v config.inject state_.ghostState

            _ ->
                text ""
        ]


viewIncoming : List (Attribute msg) -> (Msg -> msg) -> Internal.TimingFunction -> Internal.Duration -> List (Html msg) -> Html msg
viewIncoming incomingOpac inject timing dur content =
    div
        ([ style "position" "absolute"
         , style "transition-property" "opacity"
         , style "transition-timing-function" (Internal.timingToCssValue timing)
         , style "transition-duration" (toMillis dur)
         , attribute "aria-hidden" "true"
         , HtmlEvents.on "transitionend"
            (Decode.succeed (inject AnimationEnd))
         ]
            ++ incomingOpac
        )
        content



-- The only job the ghost has here is for data harvesting.


viewGhost : (view -> List (Html msg)) -> view -> (Msg -> msg) -> GhostState -> Html msg
viewGhost fn v inject (GhostState gs) =
    let
        ghostStyles =
            [ style "position" "absolute"
            , style "top" "0px"
            , style "left" "0px"
            , style "visibility" "hidden"
            , style "height" "0px"
            , attribute "aria-hidden" "true"
            ]
    in
    div ghostStyles
        [ AnimateHeight.container
            (AnimateHeight.make (GhostNoOp >> inject)
                |> AnimateHeight.content [ div [ style "height" "0px" ] (fn v) ]
                |> AnimateHeight.state gs
            )
        ]



-- UTILS


toMillis : Internal.Duration -> String
toMillis d =
    (Internal.durationToMillis d
        |> String.fromFloat
    )
        ++ "ms"
