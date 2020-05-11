module Basic exposing (..)

import AnimateHeight
import Browser
import Html exposing (Html, button, div, label, p, span, text)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)
import List.Extra as ListExtra


type alias Model =
    { auto : ( AnimateHeight.State, List (Html Msg) )
    , grow : ( AnimateHeight.State, List (Html Msg) )
    }


type Msg
    = AnimateHeightAutoMsg AnimateHeight.Msg
    | AnimateHeightGrowMsg AnimateHeight.Msg
    | ToggleAuto
    | ToggleGrow
    | AddToAuto
    | AddToGrow
    | RemoveGrow
    | RemoveAuto


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


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        ( autoS, autoC ) =
            model.auto

        ( growS, growC ) =
            model.grow
    in
    case msg of
        ToggleAuto ->
            if AnimateHeight.isHigh autoS then
                ( { model | auto = ( AnimateHeight.toLow autoS, autoC ) }, Cmd.none )

            else
                ( { model | auto = ( AnimateHeight.toHigh autoS, autoC ) }, Cmd.none )

        ToggleGrow ->
            if AnimateHeight.isHigh growS then
                ( { model | grow = ( AnimateHeight.toLow growS, growC ) }, Cmd.none )

            else
                ( { model | grow = ( AnimateHeight.toHigh growS, growC ) }, Cmd.none )

        AddToAuto ->
            ( { model | auto = ( AnimateHeight.recalculate autoS, autoC ++ [ p [] [ text loremIpsum ] ] ) }, Cmd.none )

        AddToGrow ->
            ( { model | grow = ( AnimateHeight.recalculate growS, growC ++ [ p [] [ text loremIpsum ] ] ) }, Cmd.none )

        RemoveGrow ->
            let
                length =
                    List.length growC

                removeLast idx el acc =
                    if idx == length - 1 then
                        acc

                    else
                        el :: acc
            in
            ( { model | grow = ( AnimateHeight.recalculate growS, ListExtra.indexedFoldr removeLast [] growC ) }, Cmd.none )

        RemoveAuto ->
            let
                length =
                    List.length autoC

                removeLast idx el acc =
                    if idx == length - 1 then
                        acc

                    else
                        el :: acc
            in
            ( { model | auto = ( AnimateHeight.recalculate autoS, ListExtra.indexedFoldr removeLast [] autoC ) }, Cmd.none )

        AnimateHeightAutoMsg animMsg ->
            let
                ( animState, animCmd ) =
                    AnimateHeight.update animMsg autoS
            in
            ( { model | auto = ( animState, autoC ) }, Cmd.map AnimateHeightAutoMsg animCmd )

        AnimateHeightGrowMsg animMsg ->
            let
                ( animState, animCmd ) =
                    AnimateHeight.update animMsg growS
            in
            ( { model | grow = ( animState, growC ) }, Cmd.map AnimateHeightGrowMsg animCmd )


view : Model -> Html Msg
view model =
    let
        ( autoS, autoC ) =
            model.auto

        ( growS, growC ) =
            model.grow
    in
    div []
        [ div [ style "border" "solid" ]
            [ label [] [ text "Auto" ]
            , button [ onClick ToggleAuto ] [ text "Toggle" ]
            , button [ onClick AddToAuto ] [ text "Add" ]
            , button [ onClick RemoveAuto ] [ text "Remove" ]
            , AnimateHeight.container
                (AnimateHeight.default
                    |> AnimateHeight.content (span [] autoC)
                    |> AnimateHeight.state autoS
                )
            ]
        , div [ style "border" "solid" ]
            [ label [] [ text "Grow" ]
            , button [ onClick ToggleGrow ] [ text "Toggle" ]
            , button [ onClick AddToGrow ] [ text "Add" ]
            , button [ onClick RemoveGrow ] [ text "Remove" ]
            , AnimateHeight.container
                (AnimateHeight.default
                    |> AnimateHeight.content (span [] growC)
                    |> AnimateHeight.state growS
                )
            ]
        ]


subscriptions : Model -> Sub Msg
subscriptions model =
    let
        ( autoS, _ ) =
            model.auto

        ( growS, _ ) =
            model.grow
    in
    Sub.batch
        [ Sub.map AnimateHeightAutoMsg (AnimateHeight.subscription autoS)
        , Sub.map AnimateHeightGrowMsg (AnimateHeight.subscription growS)
        ]


init : ( Model, Cmd Msg )
init =
    ( { auto =
            ( AnimateHeight.initialState (AnimateHeight.uniqueContainerId "paragraph")
                |> AnimateHeight.setHigh
                --|> AnimateHeight.snapToContent True
                |> AnimateHeight.rapid
            , [ text loremIpsum ]
            )
      , grow =
            ( AnimateHeight.initialState (AnimateHeight.uniqueContainerId "grow")
                --|> AnimateHeight.setHigh
                |> AnimateHeight.rapid
                |> AnimateHeight.animationFrame
            , [ text loremIpsum ]
            )
      }
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
