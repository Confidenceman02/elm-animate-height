module Transition exposing (..)

import AnimateHeight
import Browser
import Html exposing (Html, button, div, label, span, text)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)


type Msg
    = AnimateHeight AnimateHeight.Msg
    | Toggle
    | Add


type ContainerState
    = Open
    | Closed


type alias Model =
    ( AnimateHeight.State, List (Html Msg), ContainerState )


loremIpsum : String
loremIpsum =
    """Lorem Ipsum is simply dummy text of the printing and typesetting industry. Lorem Ipsum has been the industry's standard dummy text ever since the 1500s,
    when an unknown printer took a galley of type and scrambled it to make a type specimen book. It has survived not only five centuries, but also the leap into
    electronic typesetting, remaining essentially unchanged. It was popularised in the 1960s with the release of Letraset sheets containing Lorem Ipsum passages,
    and more recently with desktop publishing software like Aldus PageMaker including versions of Lorem Ipsum.
    There are many variations of passages of Lorem Ipsum available, but the majority have suffered alteration in some form, by injected humour, or randomised
    words which don't look even slightly believable. If you are going to use a passage of Lorem Ipsum, you need to be sure there isn't anything embarrassing hidden
    in the middle of text. All the Lorem Ipsum generators on the Internet tend to repeat predefined chunks as necessary, making this the first true generator on the Internet.
    It uses a dictionary of over 200 Latin words, combined with a handful of model sentence structures, to generate Lorem Ipsum which looks reasonable.
    The generated Lorem Ipsum is therefore always free from repetition, injected humour, or non-characteristic words etc.
    """


view : Model -> Html Msg
view ( state, content, cntState ) =
    div []
        [ div [ style "border" "solid" ]
            [ button [ onClick Toggle ] [ text "Toggle" ]
            , button [ onClick Add ] [ text "Add" ]
            , AnimateHeight.container
                (AnimateHeight.make
                    |> AnimateHeight.content [ span [] content ]
                    |> AnimateHeight.state state
                    |> AnimateHeight.inject AnimateHeight
                )
            ]
        ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg ( state, content, cntState ) =
    case msg of
        Toggle ->
            let
                ( newCntSt, cmds ) =
                    case cntState of
                        Open ->
                            AnimateHeight.height (AnimateHeight.fixed 0) state

                        Closed ->
                            AnimateHeight.height AnimateHeight.auto state
            in
            ( ( newCntSt, content, cntState ), Cmd.map AnimateHeight cmds )

        Add ->
            ( ( state, content ++ [ text loremIpsum ], cntState ), Cmd.none )

        AnimateHeight animMsg ->
            let
                ( maybeTransition, animState, animCmd ) =
                    AnimateHeight.update animMsg state

                resolveContainerState =
                    case maybeTransition of
                        Just (AnimateHeight.TransitionEnd arrivedAtHeight) ->
                            if arrivedAtHeight <= 0 then
                                Closed

                            else
                                Open

                        _ ->
                            cntState
            in
            ( ( animState, content, resolveContainerState ), Cmd.map AnimateHeight animCmd )


init : ( Model, Cmd Msg )
init =
    ( ( AnimateHeight.init (AnimateHeight.identifier "paragraph")
      , [ text loremIpsum ]
      , Closed
      )
    , Cmd.none
    )


main : Program () Model Msg
main =
    Browser.element
        { init = always init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }
