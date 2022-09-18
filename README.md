# Elm Animate Height
Lightweight Elm component for animating height using CSS transitions. Slide up/down the element to any specific height.

- Hook in to the `animationstart`/`animationend` event lifecycles.
- Animate the opacity of your content for slick transitions.

## Usage
__Set an initial state__ in your model.

> By default, the height of the container is `0px` and all content inside of it is hidden.

```elm
type alias Model = AnimateHeight.State

init : Model
init = init (identifier "my-unique-id")
```


__Subscribe to AnimateHeight subscriptions.__

```elm
type alias Model = AnimateHeight.State

subscriptions : Model -> Sub Msg
subscriptions model = 
    Sub.map AnimateMsg (AnimateHeight.subscriptions model)
```

__Set up an update `Msg`__ in your update function.

> Ignoring the `lifeCycle` for now, the update below shows how to persist the `State` and map the `AnimateHeight.Msg` to your programs `Cmd Msg`.

```elm
type alias Model = AnimateHeight.State

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    AnimateMsg animateMsg ->
        let
            (lifeCycle, newState, cmds) = update animateMsg model
        in
        (newState, Cmd.map AnimateHeight cmds)

```
__Respond to the `Transition` events__ in your update function.

> The `Transition` events let you know the progress of the animation.
The `Float` values represent the final height of the container.

```
TransitionStart 200 -- Animation has started and it's final height will be 200px
```

```
TransitionEnd 200 -- Animation has ended and it's final height is 200px
```

These `Transition` events are handy to find out if your content is 
visible or hidden.


```elm
type alias Model = AnimateHeight.State

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    AnimateMsg animateMsg ->
        let
            (transition, newState, cmds) = update animateMsg model

            isContentVisible =
              case transition of 
                Just (TransitionEnd pxValue) ->
                  if pxValue == 0 then
                    -- Your content is hidden
                  else 
                    -- Your content is visible
              _ ->
                -- Nothing going on

        in
        (newState, Cmd.map AnimateHeight cmds)

```

__Render your view__ in the `AnimateHeight` container.

> When the container is at `0px` `AnimateHeight` hides your content and sets the relevant `Aria` values for accessibility.

```elm
type alias Model = AnimateHeight.State

yourCoolView : Html Msg
yourCoolView :
    -- cool view stuff --
    
view : Model -> Msg
view model =
  container
    (make AnimateMsg
      |> content yourCoolView
      |> state model
     )
```

__Set the height of the container__ in your update function.

> We are using `auto` here which will animate to the height of the content within the container.

> If the content is 300px `auto` will animate the container to 300px.

> Check out [fixed](#fixed) to animate to a specific height.

```elm
update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        ShowContainerStuff ->
            let
              newState = height auto model
            in
            (newState, Cmd.none)

        HideContainerStuff ->
            let
              newState = height (fixed 0) model
            in
            (newState, Cmd.none)
```

## Performance considerations
Animating the height of a container does mean paying a performance cost depending on how much work
the browser needs to perform on layout and paint operations.

`elm-animate-height` uses Css transitions only, which minimises scripting time and ensures the browser is doing the animation calculations.
