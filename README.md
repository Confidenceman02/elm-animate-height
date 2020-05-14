# elm-animate-height
Animate the height of a container to its contents

## Key features

### Simple api
Simply get any view and `elm-animate-height`
will take care of animating.. well.. the height!

### Hides complexity
Animating the height of a container in a resilient way can get really tricky really fast! There's 
performance, dom queries, height information to manage, timing calculations, interruptions and a whole 
lot of gnarly edge cases. 
From the outset `elm-animate-height`'s number one goal was to make set up as frictionless as possible.

Here are the steps,

__Set an initial state__ in your model.

```elm
init : Model
init = initialState (uniqueContainerId "123")
```


__Subscribe to AnimateHeight subscription.__

```elm
subscriptions : Model -> Sub Msg
subscriptions model = 
    Sub.map AnimateMsg (AnimateHeight.subscription model)
```

__Set up an update `Msg`__ in your update function.

```elm
        AnimateMsg animMsg ->
            let
                (newState, cmds) = update model.animState
            in
            ({ model | animState = newState }, Cmd.map AnimateHeight cmds)

```
__Render your view__ in our container.

```elm
someCoolView : Html Msg
someCoolView : 
    -- cool view stuff --
    
view : Model -> Msg
view model =
  container 
    (default 
      |> content someCoolView
      |> state model.animHeightState
     )
```

__Trigger an Animation__ in your update statement.

```elm
    case msg of
        ShowContainerStuff ->
            (toHigh animHeightState, Cmd.none)
            
        HideContainerStuff ->
            (toLow animHeightState, Cmd.none)
```

### Focus on performance
Animating the height of a container does mean paying a performance cost. 
The browser needs to perform layout and paint operations and these 
can be expensive. 

`elm-animate-height` gives you two strategies in which to perform 
the animation, `Transition` and `AnimationFrame`.

`Transition` leverages css transitions to perform the animation which 
means no view updates are triggered on the container.

`AnimationFrame` will trigger view updates but manually calculates the height
of the container to the browsers refresh cadence.

Which one performs the best in your situation is just 
a matter of experimenting.

__Setting the strategy__ can be done on the initial state

```elm
init : Model
init = 
    initialState (uniqueContainerId "123")
        |> animationFrame 
```

### Adjusts to dynamic content
Sometimes the content you give `elm-animate-height` can change. For example
you might have a list of old wooden ship names that you can add and remove,
making the list longer or shorter. To animate the `container` height to the 
changing content you can ask `elm-animate-height` to perform a __recalculation__.


```elm
    case msg of
        ContentChanged ->
            let
                newstate =
                    recalculation model.animHeightState
            in    
            ({ model | animHeightState = newState }, Cmd.none)
```



