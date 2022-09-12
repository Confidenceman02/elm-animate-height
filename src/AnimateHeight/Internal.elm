module AnimateHeight.Internal exposing (TargetHeight(..), Transition(..), TransitionMsg(..))


type TargetHeight
    = Auto
    | Fixed Float


type Transition
    = Prepare
    | Transition TransitionMsg


type TransitionMsg
    = Open
    | Opening
    | Closing
    | Closed
