module ViewSwitch exposing (..)

import AnimateHeight.Variant.Switch as Switch
import Browser
import Html exposing (Html, div)
import Html.Attributes exposing (style)


type Msg
    = ViewSwitchMsg Switch.Msg
    | Toggle
    | Add
    | Fix
    | Remove


view : () -> Html Msg
view _ =
    div []
        []


update : Msg -> () -> ( (), Cmd Msg )
update msg m =
    ( m, Cmd.none )


init : ( (), Cmd Msg )
init =
    ( ()
    , Cmd.none
    )


main : Program () () Msg
main =
    Browser.element
        { init = always init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }
