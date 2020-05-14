module AnimateHeightTest exposing (..)

import AnimateHeight exposing (initialState, isHigh, setHigh, uniqueContainerId)
import AnimateHeight.Internal exposing (..)
import Expect
import Test exposing (Test, describe, test)
import Time


now : Time.Posix
now =
    Time.millisToPosix 100


start : Time.Posix
start =
    Time.millisToPosix 0


dt : Int
dt =
    Time.posixToMillis now - Time.posixToMillis start


duration : Duration
duration =
    Rapid


interruptData : Float -> InterruptData
interruptData targetHeight =
    { duration = 280
    , interruptedAt = Time.millisToPosix 280
    , startingHeight = 300
    , targetHeight = targetHeight
    }


scene : Scene
scene =
    { height = 300 }


timing : TimingFunction
timing =
    Ease


recalcData : Adjustment
recalcData =
    Recalc (Resolved 100)


internal : Test
internal =
    describe "internal tests"
        [ describe "isHigh"
            [ test "returns true when step is High" <|
                \() ->
                    Expect.true "Step must be High to be true" (isHigh (initialState (uniqueContainerId "123") |> setHigh))
            , test "is not true when step is Low" <|
                \() ->
                    Expect.false "Step must be High to be true" (isHigh (initialState (uniqueContainerId "123")))
            ]
        , describe "deltaAndDuration"
            [ describe "with no interrupt"
                [ test "should return the difference of now and start as the delta" <|
                    \() -> Expect.equal (deltaAndDuration Nothing now start duration) ( 100, duration )
                ]
            , describe "with Resolved Interrupt data"
                [ describe "when delta is below duration"
                    [ test "returns the lapsed time and duration from interrupt data" <|
                        \() -> Expect.equal (deltaAndDuration (Just <| Interrupt (Resolved (interruptData scene.height))) (Time.millisToPosix 283) start duration) ( 3, Custom 280 )
                    ]
                , describe "when delta == duration"
                    [ test "returns the fully lapsed time and duration" <|
                        \() -> Expect.equal (deltaAndDuration (Just <| Interrupt (Resolved (interruptData scene.height))) (Time.millisToPosix 560) start duration) ( 280, Custom 280 )
                    ]
                ]
            , describe "with Recalc Triggered"
                [ test "should set the delta to time lapsed" <|
                    \() ->
                        Expect.equal ( 100, duration ) (deltaAndDuration (Just <| Recalc Triggered) now start duration)
                ]
            ]
        , describe "calculatedHeightWhenHigh"
            [ describe "when snapToContent is True"
                [ test "should set the calculated height to Auto" <|
                    \() -> Expect.equal (calculatedHeightWhenHigh scene True) Auto
                ]
            , describe "when snapToContent is False"
                [ test "should set the calculated height to a pixel value" <|
                    \() -> Expect.equal (calculatedHeightWhenHigh scene False) (Pixel scene.height)
                ]
            ]
        , describe "heightToLowWhenAnimationFrame"
            [ test "returns 0 pixel CalculatedHeight " <|
                \() ->
                    Expect.equal (Pixel 150) (heightToLowWhenAnimationFrame 150 Fast Linear scene Nothing)
            , describe "when the strategy is AnimationFrame"
                [ describe "when the lapsed time is greater than duration"
                    [ test "should return the lowest height value" <|
                        let
                            delta =
                                301 - Time.posixToMillis start
                        in
                        \() -> Expect.equal (heightToLowWhenAnimationFrame delta duration Linear scene Nothing) (Pixel 0)
                    ]
                ]
            ]
        , describe "heightToHighWhenAF"
            [ describe "when current height is Auto"
                [ test "does not change calculated height " <|
                    let
                        delta =
                            Time.posixToMillis now - Time.posixToMillis start
                    in
                    \() ->
                        Expect.equal Auto (heightToHighWhenAnimationFrame delta duration timing scene Auto Nothing)
                , describe "when there are adjustments"
                    [ describe "when there is a Resolved Interrupt adjustment"
                        [ describe "when the time lapsed is 0"
                            [ test "should return the starting height in the interrupt data" <|
                                \() -> Expect.equal (Pixel 300) (heightToHighWhenAnimationFrame 0 duration timing scene (Pixel 280) (Just <| Interrupt (Resolved <| interruptData 0)))
                            ]
                        ]
                    , describe "when there is a Recalc Resolved adjustment"
                        [ describe "when the time lapsed is 50%"
                            [ test "it should return 50% the starting height to viewport height" <|
                                \() -> Expect.equal (Pixel 200) (heightToHighWhenAnimationFrame 150 Fast Linear scene (Pixel 100) (Just recalcData))
                            ]
                        ]
                    ]
                , describe "when the time lapsed is 50%"
                    [ describe "when the timing is Linear"
                        [ test "should return 50% of the remaining height" <|
                            \() -> Expect.equal (Pixel 150) (heightToHighWhenAnimationFrame 150 Fast Linear scene (Pixel 0) Nothing)
                        ]
                    ]
                ]
            ]
        , describe "interruptDataToLow"
            [ describe "when strategy is AnimationFrame"
                [ describe "when the step is Interrupt Triggered"
                    [ describe "when the target height and the scene height are both 0"
                        [ test "should return the original duration" <|
                            let
                                interrupt =
                                    interruptDataToLow now dt 0 (Pixel 200) { height = 0 } Fast AnimationFrame
                            in
                            \() ->
                                Expect.equal (durationToMillis Fast) (interrupt |> .duration)
                        ]
                    ]
                ]
            , describe "when the strategy is Transition"
                [ describe "when the target height is 0"
                    [ test "it sets the targetHeight field to 0" <|
                        let
                            interrupt =
                                interruptDataToLow now dt 0 (Pixel 200) { height = 0 } Fast Transition
                        in
                        \() ->
                            Expect.equal 0 (interrupt |> .targetHeight)
                    ]
                , describe "when the target height is the scene height"
                    [ test " it sets the targetHeight field to the scene height value" <|
                        let
                            interrupt =
                                interruptDataToLow now dt scene.height (Pixel 200) { height = 0 } Fast Transition
                        in
                        \() -> Expect.equal scene.height (interrupt |> .targetHeight)
                    ]
                ]
            ]
        , describe "transitionInterruptData"
            [ describe "when the strategy is Transition"
                [ test "should set duration to the delta time" <|
                    \() -> Expect.equal (toFloat dt) (transitionInterruptData now dt scene.height |> .duration)
                ]
            ]
        , describe "durationToMillis"
            [ describe "when duration is Instant"
                [ test "it returns 0ms" <|
                    \() ->
                        Expect.equal 0 (durationToMillis Instant)
                ]
            , describe "when duration is Immediate"
                [ test "it returns 200ms" <|
                    \() ->
                        Expect.equal 200 (durationToMillis Immediate)
                ]
            , describe "when duration is Rapid"
                [ test "it returns 250ms" <|
                    \() ->
                        Expect.equal 250 (durationToMillis Rapid)
                ]
            , describe "when duration is Fast"
                [ test "it returns 300" <|
                    \() ->
                        Expect.equal 300 (durationToMillis Fast)
                ]
            ]
        ]
