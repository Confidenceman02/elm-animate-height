module AnimateHeight.Variant.Switch exposing
    ( Config
    , Msg
    , State
    , identifier
    , init
    , make
    , setView
    , subscriptions
    , update
    , view
    )

import AnimateHeight
import Browser.Dom as Dom
import Browser.Events as DomEvents
import Html exposing (Html, div, text)
import Html.Attributes exposing (attribute, style)
import Html.Events as HtmlEvents
import Json.Decode as Decode
import Task


type Msg
    = GhostNoOp AnimateHeight.Msg
    | AnimateHeightMsg AnimateHeight.Msg
    | QueryGhostMsg
    | SwitchView
    | AnimationStart
    | AnimationEnd
    | GotGhostElement (Result Dom.Error Dom.Viewport)


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
    | QueryingGhost view
    | Entering view
    | Entered view


type Config msg
    = Config (Configuration msg)


type alias Configuration msg =
    { inject : Msg -> msg
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
    Config { inject = inject }



-- STATE MODIFIERS


setView : view -> State view -> State view
setView v (State state_) =
    State { state_ | incomingView = Just (Ghost v) }


subscriptions : State view -> Sub Msg
subscriptions (State state_) =
    Sub.batch
        [ Sub.map AnimateHeightMsg (AnimateHeight.subscriptions state_.ahState)
        , case state_.incomingView of
            Just (Ghost v) ->
                DomEvents.onAnimationFrame (\_ -> QueryGhostMsg)

            Just (Entered _) ->
                DomEvents.onAnimationFrame (\_ -> SwitchView)

            _ ->
                Sub.none
        ]


update : Msg -> State view -> ( State view, Cmd Msg )
update msg1 ((State state_) as st) =
    case msg1 of
        SwitchView ->
            case state_.incomingView of
                Just (Entered v) ->
                    ( State { state_ | incomingView = Nothing, currentView = Just v }, Cmd.none )

                _ ->
                    ( State { state_ | incomingView = Nothing }, Cmd.none )

        AnimationStart ->
            ( st, Cmd.none )

        AnimationEnd ->
            case state_.incomingView of
                Just (Entering v) ->
                    ( State { state_ | incomingView = Just (Entered v) }, Cmd.none )

                _ ->
                    ( st, Cmd.none )

        AnimateHeightMsg msg2 ->
            let
                ( _, updatedState, cmds ) =
                    AnimateHeight.update msg2 state_.ahState
            in
            ( State { state_ | ahState = updatedState }, Cmd.map AnimateHeightMsg cmds )

        QueryGhostMsg ->
            let
                resolveIncoming =
                    case state_.incomingView of
                        Just (Ghost v) ->
                            Just v

                        _ ->
                            Nothing

                (GhostState gs) =
                    state_.ghostState

                ghostIDString =
                    AnimateHeight.getIdentifierString gs

                queryCmd =
                    -- Setting pixel value from a height of auto will not trigger animation.
                    -- Instead we need to trigger an animation frame and set the height, then transition.
                    Task.attempt GotGhostElement (Dom.getViewportOf ghostIDString)
            in
            case ( resolveIncoming, state_.currentView ) of
                ( Just v, Nothing ) ->
                    ( State
                        { state_
                            | incomingView = Nothing
                            , currentView = Just v
                            , ahState = AnimateHeight.height AnimateHeight.fixedAtAuto state_.ahState
                        }
                    , Cmd.none
                    )

                ( Just v1, Just v2 ) ->
                    if v1 == v2 then
                        ( State { state_ | incomingView = Nothing }, Cmd.none )

                    else
                        ( State { state_ | incomingView = Just (QueryingGhost v1) }, queryCmd )

                ( Nothing, _ ) ->
                    ( State { state_ | incomingView = Nothing }, Cmd.none )

        GotGhostElement (Ok elem) ->
            case state_.incomingView of
                Just (QueryingGhost v) ->
                    ( State
                        { state_
                            | incomingView =
                                Just (Entering v)
                            , ahState =
                                AnimateHeight.height
                                    (AnimateHeight.fixed elem.scene.height)
                                    state_.ahState
                        }
                    , Cmd.none
                    )

                _ ->
                    ( State { state_ | incomingView = Nothing }, Cmd.none )

        GotGhostElement (Err _) ->
            ( State { state_ | incomingView = Nothing }, Cmd.none )

        _ ->
            ( st, Cmd.none )


view : (view -> List (Html msg)) -> Config msg -> State view -> Html msg
view toView (Config config) (State state_) =
    div []
        [ AnimateHeight.container
            (AnimateHeight.make (AnimateHeightMsg >> config.inject)
                |> AnimateHeight.state state_.ahState
                |> AnimateHeight.content
                    (case state_.currentView of
                        Just v ->
                            let
                                ( incomingOpac, currentOpac ) =
                                    case state_.incomingView of
                                        Just (QueryingGhost _) ->
                                            ( [ style "opacity" "0" ], [ style "opacity" "1" ] )

                                        Just (Entering _) ->
                                            ( [ style "opacity" "1" ], [ style "opacity" "0" ] )

                                        Just (Entered _) ->
                                            ( [], [ style "display" "none" ] )

                                        _ ->
                                            ( [], [] )
                            in
                            [ div [ style "position" "relative" ]
                                [ case state_.incomingView of
                                    Just (QueryingGhost v1) ->
                                        div
                                            ([ style "position" "absolute"
                                             , style "transition-property" "opacity"
                                             , style "transition-timing-function" "ease-in"
                                             , style "transition-duration" "200ms"
                                             , HtmlEvents.on "transitionend" (Decode.succeed (config.inject AnimationEnd))
                                             ]
                                                ++ incomingOpac
                                            )
                                            (toView v1)

                                    Just (Entering v1) ->
                                        div
                                            ([ style "position" "absolute"
                                             , style "transition-property" "opacity"
                                             , style "transition-timing-function" "ease-in"
                                             , style "transition-duration" "200ms"
                                             , HtmlEvents.on "transitionend" (Decode.succeed (config.inject AnimationEnd))
                                             ]
                                                ++ incomingOpac
                                            )
                                            (toView v1)

                                    Just (Entered v1) ->
                                        div
                                            [ style "position" "absolute"
                                            ]
                                            (toView v1)

                                    _ ->
                                        text ""
                                , div
                                    ([ style "position" "absolute"
                                     , style "transition-property" "opacity"
                                     , style "transition-timing-function" "ease-in"
                                     , style "transition-duration" "200ms"
                                     ]
                                        ++ currentOpac
                                    )
                                    (toView v)
                                ]
                            ]

                        _ ->
                            []
                    )
            )
        , case state_.incomingView of
            Just (Ghost v) ->
                viewGhost toView v config.inject state_.ghostState

            Just (QueryingGhost v) ->
                viewGhost toView v config.inject state_.ghostState

            _ ->
                text ""
        ]



-- The only job the ghost has here is for query data.


viewGhost : (view -> List (Html msg)) -> view -> (Msg -> msg) -> GhostState -> Html msg
viewGhost toView v inject (GhostState gs) =
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
                |> AnimateHeight.content [ div [ style "height" "0px" ] (toView v) ]
                |> AnimateHeight.state gs
            )
        ]
