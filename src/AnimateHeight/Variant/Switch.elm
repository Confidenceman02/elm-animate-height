module AnimateHeight.Variant.Switch exposing
    ( Config
    , Msg
    , State
    , cubicBezier
    , ease
    , easeIn
    , easeInOut
    , easeOut
    , fixView
    , identifier
    , init
    , linear
    , make
    , subscriptions
    , toView
    , update
    , view
    )

import AnimateHeight
import Browser.Dom as Dom
import Browser.Events as DomEvents
import Html exposing (Attribute, Html, div, text)
import Html.Attributes exposing (attribute, style)
import Html.Events as HtmlEvents
import Internal.Internal as Internal
import Json.Decode as Decode
import Task


type Msg
    = GhostNoOp AnimateHeight.Msg
    | AnimateHeightMsg AnimateHeight.Msg
    | PrepareGhostMsg
    | SwitchView
    | AnimationEnd
    | GotQueries (Result Dom.Error ( Dom.Viewport, Dom.Viewport ))
    | AnimateTo Float


type State view
    = State (StateConfiguration view)


type alias StateConfiguration view =
    { currentView : Maybe view
    , incomingView : Maybe (IncomingView view)
    , ahState : AnimateHeight.State
    , ghostState : GhostState
    , id : Identifier
    }


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


type Config msg
    = Config (Configuration msg)


type alias Configuration msg =
    { inject : Msg -> msg
    , timing : Internal.TimingFunction
    , duration : Internal.Duration
    }


identifier : String -> Identifier
identifier =
    String.split " "
        >> String.concat
        >> (++) "animate-height-variant-switch"
        >> Identifier


init : Identifier -> State view
init ((Identifier i) as switchId) =
    State
        { currentView = Nothing
        , incomingView = Nothing
        , ahState = AnimateHeight.init (AnimateHeight.identifier i)
        , ghostState = GhostState <| AnimateHeight.init (AnimateHeight.identifier "ghost")
        , id = switchId
        }


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

    yourInit : Model
    yourInit =
        init (identifier "unique-id")
            |> fixView someCoolView

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



{- |
   Transition from a current view to a new view.

   If no current view exists the new view will still transition in.

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



-- _ ->
--     State
--         { state_
--             | incomingView = Just Exiting
--         }
-- MODIFIERS


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


update : Msg -> State view -> ( State view, Cmd Msg )
update msg1 ((State state_) as st) =
    case msg1 of
        SwitchView ->
            case state_.incomingView of
                Just (Entered v) ->
                    ( State
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
                    ( State { state_ | incomingView = Nothing }, Cmd.none )

        AnimationEnd ->
            case state_.incomingView of
                Just (Entering v) ->
                    ( State { state_ | incomingView = Just (Entered v) }, Cmd.none )

                _ ->
                    ( st, Cmd.none )

        AnimateHeightMsg msg2 ->
            let
                ( trans, updatedState, cmds ) =
                    AnimateHeight.update msg2 state_.ahState
            in
            case trans of
                Just (AnimateHeight.TransitionEnd _) ->
                    ( case state_.incomingView of
                        Just Exiting ->
                            State
                                { state_
                                    | incomingView = Nothing
                                    , currentView = Nothing
                                    , ahState = updatedState
                                }

                        _ ->
                            State
                                { state_
                                    | ahState =
                                        updatedState
                                }
                    , Cmd.map AnimateHeightMsg cmds
                    )

                _ ->
                    ( State
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
                    ( State
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
                        ( State { state_ | incomingView = Nothing }, Cmd.none )

                    else
                        let
                            queryAhContainer =
                                AnimateHeight.getViewport state_.ahState
                        in
                        ( State
                            { state_
                                | incomingView = Just (PreparingGhost v1)
                            }
                        , Cmd.batch [ Task.attempt GotQueries (Task.map2 (\a b -> ( a, b )) queryAhContainer ghostContainer) ]
                        )

                ( Nothing, _ ) ->
                    ( State { state_ | incomingView = Nothing }, Cmd.none )

        GotQueries (Ok ( containerElem, ghost )) ->
            ( State
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
            ( State { state_ | incomingView = Nothing }, Cmd.none )

        AnimateTo h ->
            case state_.incomingView of
                Just (PreparingGhost v) ->
                    ( State
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
                    ( State
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
                    ( State state_, Cmd.none )

        _ ->
            ( st, Cmd.none )


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
