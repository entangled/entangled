---
title: Slasher game in Elm
author: Johan Hidding
---

What better waste of time than writing a mind numbing game in a language you don't know? Let's dive into [Elm](https://elm-lang.org/). Elm is a strongly typed functional language that compiles to JavaScript. Its design is centred around the Model/View/Controller concept.

This document may teach you:

* Basics of Elm
* Writing a small browser based game
* Random numbers in Elm (through Cmd/Msg loop)
* Generate SVG

Our game is simple. The original *slasher* game is one I remember from way back when I was playing with [GW Basic](https://en.wikipedia.org/wiki/GW-BASIC). Your hero is zipping around the screen at break neck speeds and the only way to control him is by placing slash `/` and backslash `\` characters from which it bounces off. The goal is to catch the golden snitch which only stays in one place for so long.

# Main

Elm has a set of different application formats, depending on the level of interactivity that is needed:

* **sandbox**: has no interaction with outside world, except through buttons, text fields and forms.
* **element**: can talk to HTTP, use random numbers, and capture browser events.
* **document**: similar to element, but generates the entire document.
* **application**: also handles URL requests and URL changes.

The Elm guide has a section describing the difference between `sandbox` and `element`. The event loop of `element` looks like this:

![`Browser.element` event loop](https://guide.elm-lang.org/effects/diagrams/element.svg)

We will be creating an `element` program, compile it to a JavaScript file, and create a HTML and CSS around it by hand. The lay-out of such a program in Elm looks as follows:

``` {.elm file=src/Main.elm}
<<imports>>
<<main>>

<<model>>
<<update>>
<<subscriptions>>
<<view>>
```

The `main` function tells Elm that we create an `element`

``` {.elm #main}
main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view }
```

The rest of the program has:

* **model**: Data model of the game. In strongly typed functional languages the design of a program always starts with thinking about type definitions.
* **update**: Defines how to propagate the model from one state to the next.
* **subscriptions**: Subscriptions to external events, that is, events that cannot be bound to an action on a HTML element.
* **view**: Describes how to translate the model state into an HTML element.

## Imports


``` {.elm #imports}
import Browser
import Array exposing (Array, repeat, indexedMap, toList, set, get)
import List exposing (concat)
import Browser.Events exposing (onAnimationFrameDelta, onKeyDown)
import Html exposing (Html, button, div, text, p, input, main_)
import Html.Events exposing (onClick, preventDefaultOn)
import Svg exposing (svg, circle, line, rect, g, polygon, text_)
import Svg.Attributes exposing (..)
import Random

import Json.Decode as Decode
```

# The Model

The *model* will be a grid of cells and two actors: our hero and the snitch (if you like to play around with the code, try to add a list of enemy actors that can hurt the hero).

``` {.elm #model}
type alias Model =
    { actors : { player : Actor, snitch : Actor }
    , grid : Grid
    , snitchTime : Float
    , state : GameState
    , lastPressed : String
    }
```

In addition to the *actors* and the *grid*, we have to keep time, and a game state flag. The game state tells us if the game is running etc.

``` {.elm #model}
type GameState = Start | Running | Pause | Won
```

We define the *grid* as nested arrays of cells, where each cell can have the value `Empty`, `Slash` or `BackSlash`:

``` {.elm #model}
type alias Grid = Array (Array Cell)

type Cell
    = Empty
    | Slash
    | BackSlash
```

Lastly, an `Actor` is something that has a location and a velocity, each a 2-tuple of floats.

``` {.elm #model}
type alias Actor =
    { location : (Float, Float)
    , velocity : (Float, Float)
    }
```

## Config

``` {.elm #model}
type alias Config =
    { gridSize : (Int, Int)
    , playerSpeed : Float
    , scale : Int
    , snitchTime : Float
    }

config : Config
config =
    { gridSize = (80, 50)
    , playerSpeed = 0.03
    , scale = 15
    , snitchTime = 10000
    }
```

## Init

The model is initialised by the `init` function. This function has the funny signature `() -> (Model, Cmd Msg)`. We'll return to the type definition of `Msg` later, but what this means is that the `init` function generates an initial state and a command (or list of commands) that tells the Elm run-time to perform an action upon initialisation. Such an action can be anything that requires some form of external state: HTTP requests, location/time information, and in our case: generating random numbers.

``` {.elm #model}
init : () -> (Model, Cmd Msg)
init _ = 
    let (width, height) = config.gridSize
        playerLoc       = ((toFloat width)/2 + 0.5, 0.5)
    in (
    { actors =
        { player = { location = playerLoc
                   , velocity = (0.0, config.playerSpeed) }
        , snitch = { location = (0.0, 0.0)
                   , velocity = (0.0, 0.0) }
        }
    , grid = makeGrid config.gridSize
    , snitchTime = 0.0
    , state = Start
    , lastPressed = ""
    }, Cmd.batch
    [ Random.generate PlaceSnitch (Random.pair (Random.int 0 79)
                                               (Random.int 0 49))
    ])

makeGrid : (Int, Int) -> Array (Array Cell)
makeGrid (width, height) =
    Array.repeat height
                 (Array.repeat width Empty)
```

# Subscriptions

``` {.elm #subscriptions}
keyDecoder : Decode.Decoder Msg
keyDecoder =
  Decode.map KeyPress (Decode.field "key" Decode.string)

subscriptions : Model -> Sub Msg
subscriptions model = Sub.batch
    [ onAnimationFrameDelta TimeStep
    , onKeyDown keyDecoder
    ]
```

# Update

The `update` function has the following signature:

``` {.elm #update-function}
update : Msg -> Model -> (Model, Cmd Msg)
```

This means that, given a *message* and a *model* state, we can generate a new model state and a list of requests to the Elm run-time. This list of requests has the type `Cmd Msg`, telling Elm that, whatever the request, it has to respond with a `Msg`.

``` {.elm #msg-type}
type Msg
    = KeyPress String
    | TimeStep Float
    | PlaceSnitch (Int, Int)
```

In our case, a message is either a `KeyPress` indicating a key being pressed, a `TimeStep` when a new animation frame is triggered or a `PlaceSnitch` when the random number generator returns a fresh pair of coordinates. We'll defer the handling of these messages to helper functions.

``` {.elm #update-function}
update msg model =
    case msg of
        TimeStep dt   -> let next = timeStep dt model
                         in (next, checkSnitchTime next)
        KeyPress k    -> (keyMap k { model | lastPressed = k }, Cmd.none)
        PlaceSnitch l -> (placeSnitch l model, Cmd.none)
```

``` {.elm #update}
<<msg-type>>
<<update-function>>
```

``` {.elm #update}
activeGridLoc : Actor -> (Int, Int)
activeGridLoc actor =
    let (x, y) = actor.location
    in case (actorDirection actor) of
        East  -> (round x, floor y)
        West  -> (round (x - 1.0), floor y)
        North -> (floor x, round (y - 1.0))
        South -> (floor x, round y)            

place : Cell -> Model -> Model
place cell ({actors, grid} as model) =
    let (i, j) = activeGridLoc actors.player
        row = get j grid
    in case row of
        Just r -> { model
                  | grid = set j (set i cell r) grid
                  }
        Nothing -> model

moveActor : Float -> Actor -> Actor
moveActor dt actor =
    let (x, y) = actor.location
        (vx, vy) = actor.velocity
    in { actor | location = (x + dt * vx, y + dt * vy) }

gridRef : (Int, Int) -> Grid -> Cell
gridRef (i, j) grid = 
    Maybe.withDefault Empty 
    <| Maybe.andThen (\ row -> get i row) (get j grid)

type Direction = North | East | South | West

actorDirection : Actor -> Direction
actorDirection actor =
    let (vx, vy) = actor.velocity
    in if (abs vx) > (abs vy) then
        if vx > 0 then East else West
    else
        if vy > 0 then South else North

inRange : (Int, Int) -> Bool
inRange (i, j) = i >= 0 && i < 80 && j >= 0 && j < 50

bounceActor : Float -> Cell -> Actor -> Actor
bounceActor dt cell actor =
    let (i, j)   = activeGridLoc actor
        (vx, vy) = actor.velocity
        newloc   = ((toFloat i) + 0.5, (toFloat j) + 0.5)
    in case cell of
        Slash     -> moveActor dt { location = newloc, velocity = (-vy, -vx) }
        BackSlash -> moveActor dt { location = newloc, velocity = (vy, vx) }
        Empty     -> actor

bounceOffWall : Float -> Actor -> Actor
bounceOffWall dt actor =
    let (x, y)   = actor.location
        (vx, vy) = actor.velocity
        newloc   = case actorDirection actor of
            North -> (x, 0.0)
            South -> (x, 50.0)
            East  -> (80.0, y)
            West  -> (0.0, y)
    in moveActor dt { location = newloc, velocity = (-vx, -vy) }

updateActor : Grid -> Float -> Actor -> Actor
updateActor grid dt actor =
    let a = activeGridLoc actor
        b = activeGridLoc <| moveActor dt actor 
    in if a /= b then case (gridRef a grid) of
        Slash -> bounceActor dt Slash actor
        BackSlash -> bounceActor dt BackSlash actor
        Empty -> if inRange b 
            then moveActor dt actor
            else bounceOffWall dt actor
    else
        moveActor dt actor

timeStep : Float -> Model -> Model
timeStep dt ({actors, grid, snitchTime, state} as model) =
    case state of
        Running -> if didWeWin model
                   then { model
                        | state = Won }
                   else { model
                        | snitchTime = snitchTime - dt
                        , actors =
                            { actors
                            | player = updateActor grid dt actors.player } }
        _       -> model

keyMap : String -> Model -> Model
keyMap k ({state} as model) =
    case k of
        " " -> let newState = case state of
                    Running -> Pause
                    Start   -> Running
                    Pause   -> Running
                    Won     -> Won
               in { model | state = newState }
        "ArrowLeft"  -> if state == Running
                        then place BackSlash model
                        else model
        "ArrowRight" -> if state == Running
                        then place Slash model
                        else model
        _   -> model

placeSnitch : (Int, Int) -> Model -> Model
placeSnitch (x, y) ({actors} as model) = 
    { model
    | snitchTime = config.snitchTime
    , actors = 
        { actors 
        | snitch =
            { location = ((toFloat x) + 0.5, (toFloat y) + 0.5)
            , velocity = (0.0, 0.0) } } }

checkSnitchTime : Model -> Cmd Msg
checkSnitchTime {snitchTime} =
    let (w, h) = config.gridSize
    in if snitchTime < 0.0 
    then Random.generate
            PlaceSnitch
            (Random.pair (Random.int 0 (w - 1))
                         (Random.int 0 (h - 1)))
    else Cmd.none

didWeWin : Model -> Bool
didWeWin ({actors} as model) =
    activeGridLoc actors.snitch == activeGridLoc actors.player
```

# View

``` {.elm #view}
scale : Int
scale = 15

fScale : Float
fScale = toFloat scale

viewCell : (Int, Int) -> Cell -> List (Html Msg)
viewCell (i, j) c =
    case c of
        Slash ->     [ line [ x1 (String.fromInt (scale * i + scale))
                            , y1 (String.fromInt (scale * j))
                            , x2 (String.fromInt (scale * i))
                            , y2 (String.fromInt (scale * j + scale))
                            , class "slash"
                            , style "stroke: blue; stroke-width: 2pt;" ] [] ]
        BackSlash -> [ line [ x1 (String.fromInt (scale * i))
                            , y1 (String.fromInt (scale * j))
                            , x2 (String.fromInt (scale * i + scale))
                            , y2 (String.fromInt (scale * j + scale))
                            , class "slash"
                            , style "stroke: blue; stroke-width: 2pt;" ] [] ]
        Empty ->     []

formatPath : List (Float, Float) -> String
formatPath pts = case pts of
    []           -> ""
    (x, y)::rest -> (String.fromFloat <| x * fScale) ++ "," ++
                    (String.fromFloat <| y * fScale) ++ " " ++ formatPath rest

viewHero : Actor -> Html Msg
viewHero actor =
    let (x, y) = actor.location
        path   = case actorDirection actor of
            South -> [ (x, y)
                     , (x + 0.3, y - 1)
                     , (x - 0.3, y - 1) ]
            North -> [ (x, y)
                     , (x + 0.3, y + 1)
                     , (x - 0.3, y + 1) ]
            West  -> [ (x, y)
                     , (x + 1, y + 0.3)
                     , (x + 1, y - 0.3) ]
            East  -> [ (x, y)
                     , (x - 1, y + 0.3)
                     , (x - 1, y - 0.3) ]
    in polygon [ points <| formatPath path, style "fill: red; stroke: black" ] []

viewSnitch : Actor -> Html Msg
viewSnitch actor =
    let (x, y) = actor.location
    in circle [ cx (String.fromFloat <| x * fScale)
              , cy (String.fromFloat <| y * fScale)
              , r (String.fromInt <| scale // 2)
              , style "fill: gold; stroke: black;"] []

viewOverlay : GameState -> Html Msg
viewOverlay state =
    let (w, h)  = config.gridSize
        sWidth  = (String.fromInt <| config.scale * w)
        sHeight = (String.fromInt <| config.scale * h)
        middleX = (String.fromInt <| config.scale * w // 2)
        middleY = (String.fromInt <| config.scale * h // 2)
        rectA   = rect [ x "0", y "0", width sWidth
                       , height sHeight
                       , rx (String.fromInt <| scale)
                       , ry (String.fromInt <| scale)
                       , style "fill: black" ] []
        textA s = text_ [ x middleX, y middleY, textAnchor "middle"
                        , style "fill: white; font-size: 30pt;" ]
                        [ text s ]
    in case state of
        Running -> g [] []
        Won     -> g [ opacity "0.4" ] [ rectA, textA "YOU WIN!" ] 
        Start   -> g [ opacity "0.4" ]
                     [ rectA, textA "press space to start"]
        Pause   -> g [ opacity "0.4" ]
                     [ rectA, textA "PAUSED" ]

viewSnitchBar : Float -> Html Msg
viewSnitchBar t =
    let u       = t / config.snitchTime
        (w, h)  = config.gridSize
        sWidth  = String.fromFloat <| (toFloat w) * u * (toFloat config.scale)
    in rect [ x "0", y (String.fromInt <| h * config.scale + 10)
            , width sWidth, height (String.fromInt <| config.scale // 2)
            , style "fill: gold; stroke: black" ] []

viewArena : Model -> Html Msg
viewArena ({actors, grid, state, snitchTime} as model) =
    svg [ width "100%"
        , viewBox ("-3 -3 " ++ (String.fromInt <| scale * 80 + 4) ++ " " ++ (String.fromInt <| scale * 50 + 24))]
        [ g [] (concat (toList
            (indexedMap 
                (\ y rows -> (concat (toList (indexedMap 
                            (\ x cell -> viewCell (x, y) cell)
                            rows))))
                grid)))
        , g [] [ viewHero actors.player
               , viewSnitch actors.snitch ]
        , g [] [rect [ x "0", y "0"
                     , width (String.fromInt (scale * 80))
                     , height (String.fromInt (scale * 50))
                     , rx (String.fromInt <| scale)
                     , ry (String.fromInt <| scale)
                     , style "fill: none; stroke: black; stroke-width: 2pt;" ] []]
        , (viewOverlay state)
        , (viewSnitchBar snitchTime)
        ]

view : Model -> Html Msg
view ({snitchTime, lastPressed} as model) =
    main_ []
        [ div [ id "header" ] [ text "⟍ ⟍ S L A S H E R ⟋ ⟋" ]
        , div [ id "arena" ] [ viewArena model ]
        , div [ id "help" ] [ text "keys: Left ⟍ | Right ⟋ | Space pause" ]
        ]
```

# Completing the game

From the code we can either build a HTML or a JavaScript file for embedding. We will manually create the HTML so that we can also create a stylesheet.

```
$ elm make ./src --output=slasher.js
```

The HTML can be very short now:

``` {.html file=index.html}
<!DOCTYPE html>
<html>
  <head>
    <title>Slasher</title>
    <link rel="stylesheet" href="style.css">
  </head>

  <body>
    <div id="slasher"></div>

    <script src="slasher.js"></script>
    <script>
      Elm.Main.embed(document.getElementById("slasher"));
    </script>
  </body>
</html>
```
