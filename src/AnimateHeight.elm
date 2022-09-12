module AnimateHeight exposing
    ( Msg
    , State
    , close
    , content
    , identifier
    , init
    , inject
    , make
    , open
    , state
    , update
    , view
    )

import AnimateHeight.Internal as Internal
import Browser.Dom as Dom
import Html exposing (Html, div)
import Html.Attributes exposing (id, style)
import Html.Events as Events
import Json.Decode as Decode
import Task


type Msg
    = AnimationStart
    | AnimationEnd
    | GotContainerViewport (Result Dom.Error Dom.Viewport)
    | Open
    | Close


type State
    = State_ StateConfig


type Config msg
    = Config (Configuration msg)


type alias Configuration msg =
    { content : List (Html msg)
    , inject : Maybe (Msg -> msg)
    , state : State
    }


type alias StateConfig =
    { transition : Internal.Transition
    , id : Identifier
    , targetHeight : Internal.TargetHeight
    }


type Identifier
    = Identifier String


identifier : String -> Identifier
identifier str =
    Identifier ("elm-animate-height-container__" ++ str)


defaults : Configuration msg
defaults =
    { content = []
    , inject = Nothing
    , state = init (identifier "unsafe-id")
    }


make : Config msg
make =
    Config defaults



-- CONFIG MODIFIERS


inject : (Msg -> msg) -> Config msg -> Config msg
inject msg (Config config) =
    Config { config | inject = Just msg }


content : List (Html msg) -> Config msg -> Config msg
content c (Config config) =
    Config { config | content = c }


state : State -> Config msg -> Config msg
state st (Config config) =
    Config { config | state = st }



-- STATE MODIFIERS


init : Identifier -> State
init i =
    State_
        { transition = Internal.Transition Internal.Closed
        , id = i
        , targetHeight = Internal.Fixed 0
        }


open : Msg
open =
    Open


close : Msg
close =
    Close


update : Msg -> State -> ( State, Cmd Msg )
update msg ((State_ state_) as st) =
    let
        (Identifier idString) =
            state_.id
    in
    case msg of
        Open ->
            let
                queryDomCmd =
                    Task.attempt GotContainerViewport <| Dom.getViewportOf idString
            in
            ( State_ { state_ | transition = Internal.Prepare }, queryDomCmd )

        Close ->
            ( st, Cmd.none )

        GotContainerViewport (Ok vp) ->
            ( State_
                { state_
                    | transition =
                        Internal.Transition Internal.Opening
                    , targetHeight = Internal.Fixed vp.scene.height
                }
            , Cmd.none
            )

        GotContainerViewport (Err _) ->
            ( st, Cmd.none )

        AnimationStart ->
            ( st, Cmd.none )

        AnimationEnd ->
            ( st, Cmd.none )


view : Config msg -> Html msg
view (Config config) =
    let
        (Identifier id_) =
            state_.id

        (State_ state_) =
            config.state

        resolveHeight =
            case state_.targetHeight of
                Internal.Fixed h ->
                    String.fromFloat h ++ "px"

                Internal.Auto ->
                    "auto"

        resolveTransitionMsgs =
            case config.inject of
                Just msg ->
                    [ Events.on "transitionstart" (Decode.succeed (msg AnimationStart))
                    , Events.on "transitionend" (Decode.succeed (msg AnimationEnd))
                    ]

                _ ->
                    []
    in
    div
        ([ id id_
         , style "height" resolveHeight
         , style "overflow" "hidden"
         , style "position" "relative"
         , style "transition" "height 0.3s ease-out"
         ]
            ++ resolveTransitionMsgs
        )
        (case state_.transition of
            Internal.Transition Internal.Closed ->
                []

            _ ->
                config.content
        )
