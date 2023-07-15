module Internal.Internal exposing
    ( Bezier(..)
    , Duration(..)
    , HeightVariant(..)
    , Progress(..)
    , TimingFunction(..)
    , durationToMillis
    , timingToCssValue
    )


type Progress
    = Preparing
    | Running
    | Idle


type HeightVariant
    = Auto
    | Fixed Float
    | FixedAtAuto


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
