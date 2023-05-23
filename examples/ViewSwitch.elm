module ViewSwitch exposing (..)

import AnimateHeight.Variant.Switch as Switch
import Browser
import Html exposing (Html, button, div, text)
import Html.Events exposing (onClick)


type Msg
    = ViewSwitchMsg Switch.Msg
    | Toggle View


type View
    = View1
    | View2


toView : View -> List (Html Msg)
toView v =
    case v of
        View1 ->
            [ div []
                [ text "Lorem Ipsum is simply dummy text of the printing and typesetting industry. Lorem Ipsum has been the industry's standard dummy text ever since the 1500s, when an unknown printer took a galley of type and scrambled it to make a type specimen book."
                ]
            ]

        View2 ->
            [ div [] [ text "It is a long established fact that a reader will be distracted by the readable content of a page when looking at its layout. The point of using Lorem Ipsum is that it has a more-or-less normal distribution of letters, as opposed to using 'Content here, content here', making it look like readable English. Many desktop publishing packages and web page editors now use Lorem Ipsum as their default model text, and a search for 'lorem ipsum' will uncover many web sites still in their infancy. Various versions have evolved over the years, sometimes by accident, sometimes on purpose (injected humour and the like)." ] ]


subscriptions : Switch.State View -> Sub Msg
subscriptions st =
    Sub.map ViewSwitchMsg (Switch.subscriptions st)


update : Msg -> Switch.State View -> ( Switch.State View, Cmd Msg )
update msg st =
    case msg of
        Toggle v ->
            let
                _ =
                    Debug.log "Toggle" v
            in
            ( Switch.setView v st, Cmd.none )

        ViewSwitchMsg msg1 ->
            let
                ( updatedSt, cmds ) =
                    Switch.update msg1 st
            in
            ( updatedSt, Cmd.map ViewSwitchMsg cmds )


init : ( Switch.State View, Cmd Msg )
init =
    ( Switch.init (Switch.identifier "all-views")
    , Cmd.none
    )


main : Program () (Switch.State View) Msg
main =
    Browser.element
        { init = always init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


view : Switch.State View -> Html Msg
view st =
    div []
        [ Switch.view toView (Switch.make ViewSwitchMsg) st
        , button [ onClick (Toggle View1) ] [ text "View 1" ]
        , button [ onClick (Toggle View2) ] [ text "View 2" ]
        , text "Hello AnimateHeight"
        ]
