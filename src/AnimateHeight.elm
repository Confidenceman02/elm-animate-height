module AnimateHeight exposing
    ( Config
    , Identifier
    , Msg
    , State
    , Transition(..)
    , auto
    , container
    , content
    , cubicBezier
    , customTiming
    , ease
    , easeIn
    , easeInOut
    , easeOut
    , fast
    , fixed
    , height
    , identifier
    , immediate
    , init
    , inject
    , instant
    , linear
    , make
    , rapid
    , state
    , update
    )

import AnimateHeight.Internal as Internal
import Browser.Dom as Dom
import Html exposing (Html, div)
import Html.Attributes exposing (id, style)
import Html.Events as Events
import Json.Decode as Decode
import Task


{-| -}
type Msg
    = AnimationStart
    | AnimationEnd
    | GotContainerViewport (Result Dom.Error Dom.Viewport)
    | HeightMsg


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
    }


type alias StateConfig =
    { targetHeight : Internal.TargetHeight
    , calculatedHeight : Float
    , id : Identifier
    , running : Bool
    }


{-| -}
type Identifier
    = Identifier String


{-| -}
type Transition
    = TransitionStart Float
    | TransitionEnd Float


{-| -}
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
    }


{-| -}
make : Config msg
make =
    Config defaults



-- CONFIG MODIFIERS


{-| This is like not having an animation at all. Duration maps to 0

        AnimateHeight.view
          (AnimateHeight.make
              |> AnimateHeight.instant
          )

-}
instant : Config msg -> Config msg
instant (Config config) =
    Config { config | duration = Internal.Instant }


{-| Animation duration of 200ms

        AnimateHeight.view
          (AnimateHeight.make
              |> AnimateHeight.immediate
          )

-}
immediate : Config msg -> Config msg
immediate (Config config) =
    Config { config | duration = Internal.Immediate }


{-| Animation duration of 250ms

        AnimateHeight.view
          (AnimateHeight.make
              |> AnimateHeight.rapid
          )

-}
rapid : Config msg -> Config msg
rapid (Config config) =
    Config { config | duration = Internal.Rapid }


{-| Animation duration of 300ms

        AnimateHeight.view
          (AnimateHeight.make
              |> AnimateHeight.fast
          )

-}
fast : Config msg -> Config msg
fast (Config config) =
    Config { config | duration = Internal.Fast }


{-| Set a custom duration.
Negative values will be converted to their positive equivalent.

custom -333 => 333

        AnimateHeight.view
          (AnimateHeight.make
              |> AnimateHeight.customTiming 333
          )

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


{-| -}
inject : (Msg -> msg) -> Config msg -> Config msg
inject msg (Config config) =
    Config { config | inject = Just msg }


{-| -}
content : List (Html msg) -> Config msg -> Config msg
content c (Config config) =
    Config { config | content = c }


{-| -}
state : State -> Config msg -> Config msg
state st (Config config) =
    Config { config | state = st }


{-| [Ease timing](https://developer.mozilla.org/en-US/docs/Web/CSS/animation-timing-function)

        AnimateHeight.view
          (AnimateHeight.make
              |> AnimateHeight.ease
          )

-}
ease : Config msg -> Config msg
ease (Config config) =
    Config { config | timing = Internal.Ease }


{-| [Ease-in timing](https://developer.mozilla.org/en-US/docs/Web/CSS/animation-timing-function)

        AnimateHeight.view
          (AnimateHeight.make
              |> AnimateHeight.easeIn
          )

-}
easeIn : Config msg -> Config msg
easeIn (Config config) =
    Config { config | timing = Internal.EaseIn }


{-| [Ease-out timing](https://developer.mozilla.org/en-US/docs/Web/CSS/animation-timing-function)

        AnimateHeight.view
          (AnimateHeight.make
              |> AnimateHeight.easeOut
          )

-}
easeOut : Config msg -> Config msg
easeOut (Config config) =
    Config { config | timing = Internal.EaseOut }


{-| [Ease-in-out timing](https://developer.mozilla.org/en-US/docs/Web/CSS/animation-timing-function)

        AnimateHeight.view
          (AnimateHeight.make
              |> AnimateHeight.easeInOut
          )

-}
easeInOut : Config msg -> Config msg
easeInOut (Config config) =
    Config { config | timing = Internal.EaseInOut }


{-| [Linear timing](https://developer.mozilla.org/en-US/docs/Web/CSS/animation-timing-function)

        AnimateHeight.view
          (AnimateHeight.make
              |> AnimateHeight.linear
          )

-}
linear : Config msg -> Config msg
linear (Config config) =
    Config { config | timing = Internal.Linear }


{-| [Cubic bezier timing](https://developer.mozilla.org/en-US/docs/Web/CSS/animation-timing-function)

        AnimateHeight.view
          (AnimateHeight.make
              |> AnimateHeight.cubicBezier 0.1 0.7 1 0.1
          )

-}
cubicBezier : Float -> Float -> Float -> Float -> Config msg -> Config msg
cubicBezier f1 f2 f3 f4 (Config config) =
    Config { config | timing = Internal.CubicBezier (Internal.Bezier f1 f2 f3 f4) }



-- STATE MODIFIERS


{-| -}
init : Identifier -> State
init i =
    State_
        { targetHeight = Internal.Fixed 0
        , calculatedHeight = 0
        , id = i
        , running = False
        }


{-| -}
height : Internal.TargetHeight -> State -> ( State, Cmd Msg )
height t (State_ st) =
    let
        ( _, s, c ) =
            update HeightMsg (State_ { st | targetHeight = t })
    in
    ( s, c )


{-| Will transition to the height of the content.

        update : msg -> model -> (model, Cmd msg)
        update msg model =
            case msg of
                SeeContent ->
                  let
                      (state, cmds) =
                      height auto model.heightState
                  in
                  ({ model | heightState = state }
                   , Cmd.map AnimateHeightMsg cmds
                  )

-}
auto : Internal.TargetHeight
auto =
    Internal.Auto


{-| Will transition to the height you set.

Values translate to px values. e.g. 200 -> 200px

        update : msg -> model -> (model, Cmd msg)
        update msg model =
            case msg of
                SeeContent ->
                  let
                      (state, cmds) =
                      height (fixed 200) -- Translates to 200px
                  in
                  ({ model | animateState = state }
                   , Cmd.map AnimateHeightMsg cmds
                  )

-}
fixed : Float -> Internal.TargetHeight
fixed =
    Internal.Fixed


{-| -}
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
                    ( Nothing
                    , State_ { state_ | calculatedHeight = h }
                    , Cmd.none
                    )

                _ ->
                    let
                        queryDomCmd =
                            Task.attempt GotContainerViewport <| Dom.getViewportOf idString
                    in
                    ( Nothing
                    , st
                    , queryDomCmd
                    )

        GotContainerViewport (Ok vp) ->
            ( Nothing
            , State_
                { state_
                    | targetHeight = Internal.Fixed vp.scene.height
                    , calculatedHeight = vp.scene.height
                    , running = True
                }
            , Cmd.none
            )

        GotContainerViewport (Err _) ->
            ( Nothing, st, Cmd.none )

        AnimationStart ->
            ( Just (TransitionStart state_.calculatedHeight), State_ { state_ | running = True }, Cmd.none )

        AnimationEnd ->
            ( Just (TransitionEnd state_.calculatedHeight)
            , State_ { state_ | running = False }
            , Cmd.none
            )


{-| -}
container : Config msg -> Html msg
container (Config config) =
    let
        (Identifier id_) =
            state_.id

        (State_ state_) =
            config.state

        resolveHeight =
            String.fromFloat state_.calculatedHeight ++ "px"

        resolveTransitionMsgs =
            case config.inject of
                Just msg ->
                    [ Events.on "transitionstart" (Decode.succeed (msg AnimationStart))
                    , Events.on "transitionend" (Decode.succeed (msg AnimationEnd))
                    ]

                _ ->
                    []

        resolveAnimationPlayState =
            if state_.running then
                [ style "animation-play-state" "running" ]

            else
                []

        resolveTransitionDuration =
            [ style "transition-duration"
                ((Internal.durationToMillis config.duration
                    |> String.fromFloat
                 )
                    ++ "ms"
                )
            ]

        resolveTiming =
            [ style "transition-timing-function" (Internal.timingToCssValue config.timing) ]
    in
    div
        ([ id id_
         , style "height" resolveHeight
         , style "overflow" "hidden"
         , style "position" "relative"
         , style "transition-property" "height"
         ]
            ++ resolveTransitionMsgs
            ++ resolveAnimationPlayState
            ++ resolveTransitionDuration
            ++ resolveTiming
        )
        config.content
