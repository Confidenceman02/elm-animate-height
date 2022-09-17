module AnimateHeight exposing
    ( Action(..)
    , Msg
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
    | OpenMsg
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
    { transition : ( Internal.TargetHeight, Internal.Transition )
    , id : Identifier
    }


type Identifier
    = Identifier String


type Action
    = Open
    | Opening
    | Closed
    | Closing


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
        { transition = ( Internal.Fixed 0, Internal.Closed )
        , id = i
        }


open : Msg
open =
    OpenMsg


close : Msg
close =
    Close


update : Msg -> State -> ( Maybe Action, State, Cmd Msg )
update msg ((State_ state_) as st) =
    let
        (Identifier idString) =
            state_.id
    in
    case msg of
        OpenMsg ->
            let
                queryDomCmd =
                    Task.attempt GotContainerViewport <| Dom.getViewportOf idString
            in
            ( Nothing
            , State_
                { state_
                    | transition =
                        Tuple.mapSecond (\_ -> Internal.PrepareOpening) state_.transition
                }
            , queryDomCmd
            )

        Close ->
            ( Just Closing
            , State_
                { state_
                    | transition = ( Internal.Fixed 0, Internal.Closing )
                }
            , Cmd.none
            )

        GotContainerViewport (Ok vp) ->
            ( Nothing
            , State_
                { state_
                    | transition = ( Internal.Fixed vp.scene.height, Internal.Opening )
                }
            , Cmd.none
            )

        GotContainerViewport (Err _) ->
            ( Nothing, st, Cmd.none )

        AnimationStart ->
            case state_.transition of
                ( _, Internal.Opening ) ->
                    ( Just Opening, st, Cmd.none )

                ( _, Internal.Closing ) ->
                    ( Just Closing, st, Cmd.none )

                _ ->
                    ( Nothing, st, Cmd.none )

        AnimationEnd ->
            case state_.transition of
                ( _, Internal.Closing ) ->
                    ( Just Closed
                    , State_
                        { state_
                            | transition =
                                Tuple.mapSecond
                                    (\_ -> Internal.Closed)
                                    state_.transition
                        }
                    , Cmd.none
                    )

                ( _, Internal.Opening ) ->
                    ( Just Open
                    , State_
                        { state_
                            | transition =
                                Tuple.mapSecond
                                    (\_ -> Internal.Open)
                                    state_.transition
                        }
                    , Cmd.none
                    )

                _ ->
                    ( Nothing, st, Cmd.none )


view : Config msg -> Html msg
view (Config config) =
    let
        (Identifier id_) =
            state_.id

        (State_ state_) =
            config.state

        resolveHeight =
            case state_.transition of
                ( Internal.Fixed h, _ ) ->
                    String.fromFloat h ++ "px"

                ( Internal.Auto, _ ) ->
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
            ( _, Internal.Closed ) ->
                []

            _ ->
                config.content
        )
