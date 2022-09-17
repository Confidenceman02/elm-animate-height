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
    ( AnimateHeight.State, List (Html Msg), AnimateHeight.Action )


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
view ( state, content, cntSt ) =
    div []
        [ div [ style "border" "solid" ]
            [ button [ onClick Toggle ] [ text "Toggle" ]
            , button [ onClick Add ] [ text "Add" ]
            , AnimateHeight.view
                (AnimateHeight.make
                    |> AnimateHeight.content [ span [] content ]
                    |> AnimateHeight.state state
                    |> AnimateHeight.inject AnimateHeight
                )
            ]
        ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg ( state, content, trans ) =
    case msg of
        Toggle ->
            case trans of
                AnimateHeight.Closed ->
                    let
                        ( newCntSt, cmds ) =
                            AnimateHeight.open state
                    in
                    ( ( newCntSt, content, trans ), Cmd.map AnimateHeight cmds )

                AnimateHeight.Open ->
                    let
                        ( newCntSt, cmds ) =
                            AnimateHeight.close state
                    in
                    ( ( newCntSt, content, trans ), Cmd.map AnimateHeight cmds )

                _ ->
                    ( ( state, content, trans ), Cmd.none )

        Add ->
            ( ( state, content ++ [ text loremIpsum ], trans ), Cmd.none )

        AnimateHeight animMsg ->
            let
                ( maybeAction, animState, animCmd ) =
                    AnimateHeight.update animMsg state

                newContainerSt =
                    case maybeAction of
                        Just action ->
                            action

                        _ ->
                            trans
            in
            ( ( animState, content, newContainerSt ), Cmd.map AnimateHeight animCmd )


init : ( Model, Cmd Msg )
init =
    ( ( AnimateHeight.init (AnimateHeight.identifier "paragraph")
      , [ text loremIpsum ]
      , AnimateHeight.Closed
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
