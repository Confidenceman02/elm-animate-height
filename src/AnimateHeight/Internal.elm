module AnimateHeight.Internal exposing
    ( Bezier(..)
    , Duration(..)
    , TargetHeight(..)
    , TimingFunction(..)
    , Transition(..)
    , canClose
    , canOpen
    , durationToMillis
    , timingToCssValue
    )


type TargetHeight
    = Auto
    | Fixed Float


type Transition
    = Open
    | PrepareSlidingDown
    | SlidingDown Float
    | Closing
    | Closed


type Duration
    = Instant
    | Immediate
    | Rapid
    | Fast
    | Custom Float


type TimingFunction
    = Ease
    | Linear
    | EaseIn
    | EaseOut
    | EaseInOut
    | CubicBezier Bezier


type Bezier
    = Bezier Float Float Float Float


timingToCssValue : TimingFunction -> String
timingToCssValue timingFunction =
    case timingFunction of
        Ease ->
            "ease"

        Linear ->
            "linear"

        EaseIn ->
            "ease-in"

        EaseOut ->
            "ease-out"

        EaseInOut ->
            "ease-in-out"

        CubicBezier (Bezier float1 float2 float3 float4) ->
            "cubic-bezier("
                ++ String.fromFloat float1
                ++ " , "
                ++ String.fromFloat float2
                ++ " , "
                ++ String.fromFloat float3
                ++ " , "
                ++ String.fromFloat float4
                ++ ")"


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


durationToMillis : Duration -> Float
durationToMillis duration_ =
    case duration_ of
        Instant ->
            0

        Immediate ->
            200

        Rapid ->
            250

        Fast ->
            300

        Custom f ->
            f
