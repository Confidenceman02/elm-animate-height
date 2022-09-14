module AnimateHeight.Internal exposing (TargetHeight(..), Transition(..))


type TargetHeight
    = Auto
    | Fixed Float


type Transition
    = Open
    | PrepareOpening
    | Opening
    | Closing
    | Closed
