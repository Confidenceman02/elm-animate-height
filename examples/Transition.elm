module Transition exposing (..)

import AnimateHeight
import Browser
import Html exposing (Html, button, div, label, p, span, text)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)
import List.Extra as ListExtra


type Msg
    = AnimateHeight AnimateHeight.Msg
    | Toggle
    | AddTo
    | Remove


type alias Model =
    ( AnimateHeight.State, List (Html Msg) )


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
view ( state, content ) =
    div []
        [ div [ style "border" "solid" ]
            [ label [] [ text "Auto" ]
            , button [ onClick Toggle ] [ text "Toggle" ]
            , button [ onClick AddTo ] [ text "Add" ]
            , button [ onClick Remove ] [ text "Remove" ]
            , AnimateHeight.container
                (AnimateHeight.default
                    |> AnimateHeight.content (span [] content)
                    |> AnimateHeight.state state
                    |> AnimateHeight.overflow (AnimateHeight.VisibleWhenAtContentHeight)
                )
            ]
        ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg (( state, content ) as model) =
    case msg of
        Toggle ->
            if AnimateHeight.isContentHeight state then
                ( ( AnimateHeight.toMinHeight state, content ), Cmd.none )

            else
                ( ( AnimateHeight.toContentHeight state, content ), Cmd.none )

        AddTo ->
            ( ( AnimateHeight.recalculate state, content ++ [ p [] [ text loremIpsum ] ] ), Cmd.none )

        Remove ->
            let
                length =
                    List.length content

                removeLast idx el acc =
                    if idx == length - 1 then
                        acc

                    else
                        el :: acc
            in
            ( ( AnimateHeight.recalculate state, ListExtra.indexedFoldr removeLast [] content ), Cmd.none )

        AnimateHeight animMsg ->
            let
                ( animState, animCmd ) =
                    AnimateHeight.update animMsg state
            in
            ( ( animState, content ), Cmd.map AnimateHeight animCmd )


subscriptions : Model -> Sub Msg
subscriptions ( state, _ ) =
    Sub.batch
        [ Sub.map AnimateHeight (AnimateHeight.subscription state)
        ]


init : ( Model, Cmd Msg )
init =
    ( ( AnimateHeight.initialState (AnimateHeight.uniqueContainerId "paragraph")
            |> AnimateHeight.setAtContentHeight
            |> AnimateHeight.rapid
      , [ text loremIpsum ]
      )
    , Cmd.none
    )


main : Program () Model Msg
main =
    Browser.element
        { init = always init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
