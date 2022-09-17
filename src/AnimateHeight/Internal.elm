module AnimateHeight.Internal exposing (TargetHeight(..), Transition(..), canClose, canOpen)


type TargetHeight
    = Auto
    | Fixed Float


type Transition
    = Open
    | PrepareOpening
    | Opening
    | Closing
    | Closed


canOpen : Transition -> Bool
canOpen t =
    case t of
        Closed ->
            True

        _ ->
            False


canClose : Transition -> Bool
canClose t =
    case t of
        Open ->
            True

        _ ->
            False
