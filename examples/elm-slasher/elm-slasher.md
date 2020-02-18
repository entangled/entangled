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

Our game is simple. The original *slasher* game is one I remember from way back when I was playing with [GW Basic](https://en.wikipedia.org/wiki/GW-BASIC). Your hero is zipping around the screen at break neck speeds and the only way to control him is by placing slash `/` and backslash <pre>\</pre> characters from which it bounces off. The goal is to catch the golden snitch which only stays in one place for so long.

![Screenshot of "Slasher"](screenshot.png)

[You can play this game here.](https://jhidding.github.io/enTangleD/slasher.html)

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
module Main exposing (..)

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
import Html exposing (Html, button, div, text, p, input, main_, a)
import Html.Attributes exposing (href)
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

gridRef : (Int, Int) -> Grid -> Cell
gridRef (i, j) grid = 
    Maybe.withDefault Empty 
    <| Maybe.andThen (\ row -> get i row) (get j grid)

gridSet : (Int, Int) -> Cell -> Grid -> Grid
gridSet (i, j) cell grid =
    let row_ = get j grid
    in case row_ of
        Nothing  -> grid
        Just row -> set j (set i cell row) grid
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

inRange : (Int, Int) -> Bool
inRange (i, j) = 
    let (w, h) = config.gridSize
    in i >= 0 && i < w && j >= 0 && j < h
```

## Init

The model is initialised by the `init` function. This function has the funny signature `() -> (Model, Cmd Msg)`. We'll return to the type definition of `Msg` later, but what this means is that the `init` function generates an initial state and a command (or list of commands) that tells the Elm run-time to perform an action upon initialisation. Such an action can be anything that requires some form of external state: HTTP requests, location/time information, and in our case: generating random numbers.

``` {.elm #model}
init : () -> (Model, Cmd Msg)
init _ = 
    let (width, height) = config.gridSize
        playerLoc       = ((toFloat width)/2 + 0.5, 2.5)
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
    }, Random.generate
            PlaceSnitch
            (Random.pair (Random.int 0 (width - 1))
                         (Random.int 0 (height - 1))))

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
        KeyPress k    -> (keyMap k model, Cmd.none)
        PlaceSnitch l -> (placeSnitch l model, Cmd.none)
```

``` {.elm #update}
<<msg-type>>
<<update-function>>
```

## Time step

Each time step, if the game in in `Running` state, we need to check if the game is won, and otherwise move all actors that need moving.

``` {.elm #update}
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
```

### Did we win?

``` {.elm #update}
didWeWin : Model -> Bool
didWeWin ({actors} as model) =
    activeCell actors.snitch == activeCell actors.player
```

### Update actors

Updating an actor is a bit of boring bookkeeping. We check whether the actor's active grid cell changes. This happens when the actor moves across the center of a cell. If the active cell changed, potentially the actor bounced off a (back)slash or a wall.

``` {.elm #update}
updateActor : Grid -> Float -> Actor -> Actor
updateActor grid dt actor =
    let a = activeCell actor
        b = activeCell <| moveActor dt actor 
    in if a /= b then case (gridRef a grid) of
        Slash     -> bounceActor dt Slash actor
        BackSlash -> bounceActor dt BackSlash actor
        Empty     -> if inRange b 
            then moveActor dt actor
            else bounceOffWall dt actor
    else
        moveActor dt actor
```

#### Active cell

To get a smooth flying experience the actors have floating point coordinates. From the coordinates we need to compute the integer indices into the grid.

``` {.elm #update}
activeCell : Actor -> (Int, Int)
activeCell actor =
    let (x, y) = actor.location
    in case (actorDirection actor) of
        East  -> (round x, floor y)
        West  -> (round (x - 1.0), floor y)
        North -> (floor x, round (y - 1.0))
        South -> (floor x, round y)            
```

#### Direction

From the velocity we often need to know in which of the four cardinal directions an actor is moving.

``` {.elm #update}
type Direction = North | East | South | West

actorDirection : Actor -> Direction
actorDirection actor =
    let (vx, vy) = actor.velocity
    in if (abs vx) > (abs vy) then
        if vx > 0 then East else West
    else
        if vy > 0 then South else North
```

#### Actor dynamics

When an actor moves, it may do so in one of three ways:

* move without obstacle
* bounce off a (back)slash)
* bounce off the wall

For each of these cases we have a separate function.

``` {.elm #update}
moveActor : Float -> Actor -> Actor
moveActor dt actor =
    let (x, y)   = actor.location
        (vx, vy) = actor.velocity
    in { actor | location = (x + dt * vx, y + dt * vy) }

bounceActor : Float -> Cell -> Actor -> Actor
bounceActor dt cell actor =
    let (i, j)   = activeCell actor
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
```

## Snitch time

Each time step we decreased the `snitchTime` field. If it drops below 0, we ask the Elm runtime for a new random snitch location.

``` {.elm #update}
checkSnitchTime : Model -> Cmd Msg
checkSnitchTime {snitchTime} =
    let (w, h) = config.gridSize
    in if snitchTime < 0.0 
    then Random.generate
            PlaceSnitch
            (Random.pair (Random.int 0 (w - 1))
                         (Random.int 0 (h - 1)))
    else Cmd.none
```

### Moving the snitch

When the snitch is placed we reset the `snitchTime` to the configured value

``` {.elm #update}
placeSnitch : (Int, Int) -> Model -> Model
placeSnitch (x, y) ({actors} as model) = 
    { model
    | snitchTime = config.snitchTime
    , actors = 
        { actors 
        | snitch =
            { location = ((toFloat x) + 0.5, (toFloat y) + 0.5)
            , velocity = (0.0, 0.0) } } }
```

## Keymap

We listen to three keys: space, left arrow and right arrow. We could have listened for `\` and `/` keys, but in Firefox the `/` key also activates quick search. We could work around this, but it is a bit of a hassle.

``` {.elm #update}
keyMap : String -> Model -> Model
keyMap k ({state} as model) =
    let slash cell = if state == Running
                     then place cell model
                     else model
    in case k of
        " " -> let newState = case state of
                    Running -> Pause
                    Start   -> Running
                    Pause   -> Running
                    Won     -> Won
               in { model | state = newState }
        "ArrowLeft"  -> slash BackSlash
        "ArrowRight" -> slash Slash
        _            -> model
```

### Placing a (back)slash

``` {.elm #update}
place : Cell -> Model -> Model
place cell ({actors, grid} as model) =
    let loc = activeCell actors.player
    in { model | grid = gridSet loc cell grid }
```

# View

``` {.elm #view}
scale : Int
scale = config.scale

fScale : Float
fScale = toFloat scale

viewCell : (Int, Int) -> Cell -> List (Html Msg)
viewCell (i, j) c =
    case c of
        Slash ->     [ line [ x1 (String.fromInt (scale * i + scale))
                            , y1 (String.fromInt (scale * j))
                            , x2 (String.fromInt (scale * i))
                            , y2 (String.fromInt (scale * j + scale))
                            , class "slash" ] [] ]
        BackSlash -> [ line [ x1 (String.fromInt (scale * i))
                            , y1 (String.fromInt (scale * j))
                            , x2 (String.fromInt (scale * i + scale))
                            , y2 (String.fromInt (scale * j + scale))
                            , class "slash" ] [] ]
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
    in polygon [ points <| formatPath path, class "hero" ] []

viewSnitch : Actor -> Html Msg
viewSnitch actor =
    let (x, y) = actor.location
    in circle [ cx (String.fromFloat <| x * fScale)
              , cy (String.fromFloat <| y * fScale)
              , r (String.fromInt <| scale // 2)
              , class "snitch" ] []

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
        textA s = text_ [ x middleX, y middleY, textAnchor "middle" ]
                        [ text s ]
        overlay s = g [ id "overlay" ] [ rectA, textA s ]
    in case state of
        Running -> g [] []
        Won     -> overlay "YOU WIN!"
        Start   -> overlay "press space to start"
        Pause   -> overlay "PAUSE"

viewSnitchBar : Float -> Html Msg
viewSnitchBar t =
    let u       = t / config.snitchTime
        (w, h)  = config.gridSize
        sWidth  = String.fromFloat <| (toFloat w) * u * (toFloat config.scale)
    in rect [ x "0", y (String.fromInt <| h * config.scale + 10)
            , width sWidth, height (String.fromInt <| config.scale // 2)
            , id "snitch-bar" ] []

viewArena : Model -> Html Msg
viewArena ({actors, grid, state, snitchTime} as model) =
    let blurAtPause = class 
                    <| if state == Running
                       then "non-blurred"
                       else "blurred"
    in svg [ width "100%"
           , viewBox ("-3 -3 " ++ (String.fromInt <| scale * 80 + 4) ++ " " ++    (String.fromInt <| scale * 50 + 24))]
           [ g [] [rect [ x "0", y "0"
                        , width (String.fromInt (scale * 80))
                        , height (String.fromInt (scale * 50))
                        , rx (String.fromInt <| scale)
                        , ry (String.fromInt <| scale)
                        , id "box" ] []]
           , g [blurAtPause]
               (concat (toList
               (indexedMap 
                   (\ y rows -> (concat (toList (indexedMap 
                               (\ x cell -> viewCell (x, y) cell)
                               rows))))
                   grid)))
           , g [blurAtPause] [ viewHero actors.player
                  , viewSnitch actors.snitch ]
           , (viewOverlay state)
           , (viewSnitchBar snitchTime)
           ]

view : Model -> Html Msg
view model =
    main_ []
        [ div [ id "header" ] [ text "\\ \\ S L A S H E R / /" ]
        , div [ id "arena" ] [ viewArena model ]
        , div [ id "help" ] [ text "keys: Left \\ | Right / | Space pause" ]
        , div [ id "footer" ]
        [ text "Use the source, at "
        , a [ href "https://jhidding.github.io/enTangleD" ]
            [ text "enTangleD!" ] ]
        ]
```

# Completing the game

From the code we can either build a HTML or a JavaScript file for embedding. We will manually create the HTML so that we can also create a stylesheet. Here's the `Makefile` for creating an optimized and uglified version of `slasher` (you can install `uglifyjs` with `npm install -g uglify-js`):

``` {.makefile file=Makefile}
.RECIPEPREFIX +=
.PHONY: build

build: slasher.min.js

slasher.js: src/Main.elm
	elm make src/Main.elm --output=slasher.js --optimize

slasher.min.js: slasher.js
	uglifyjs slasher.js --compress 'pure_funcs="F2,F3,F4,F5,F6,F7,F8,F9,A2,A3,A4,A5,A6,A7,A8,A9",pure_getters,keep_fargs=false,unsafe_comps,unsafe' | uglifyjs --mangle --output=slasher.min.js
```

The HTML can be very short now:

``` {.html file=slasher.html}
<!DOCTYPE html>
<html>
  <head>
    <title>Slasher</title>
    <meta charset="UTF-8"> 
    <<google-font>> 
    <link rel="stylesheet" href="slasher.css">
  </head>

  <body>
    <div id="slasher"></div>

    <script src="slasher.min.js"></script>
    <script>
      Elm.Main.init({ node: document.getElementById("slasher") });
    </script>
  </body>
</html>
```

Browse Google fonts for a nice slashery font:

``` {.html #google-font}
<link href="https://fonts.googleapis.com/css?family=Love+Ya+Like+A+Sister" rel="stylesheet">
```

And add some style

``` {.css file=slasher.css}
body {
    font-family: "Love Ya Like A Sister", sans serif;
    background-image: radial-gradient(circle, #112233, #000033);
    color: white;
}

#arena svg {
    max-height: 80vh;
}

#header {
    font-size: 20pt;
    text-align: center;
    margin-bottom: 5pt;
}

#help {
    text-align: center;
}

#footer {
    font-family: sans serif;
    font-size: 8pt;
    background: #888;
    color: black;
    text-align: center;
    padding: 4pt 0;
    margin: 10pt 0 0 0;
}

#footer a {
    color: #008;
}

.blurred {
    filter: blur(1pt);
}

/* svg #overlay {
    filter: blur(1pt);
} */

svg #overlay rect {
    opacity: 0.3;
    fill: black;
}

svg #overlay text {
    font-size: 70pt;
    fill: white;
}

svg #box {
    opacity: 0.5;
    stroke: goldenrod;
    stroke-width: 2pt;
    fill: none;
}

svg line.slash {
    stroke: #8888ff;
    stroke-width: 2pt;
}

svg .hero {
    fill: red;
    stroke: black;
}

svg .snitch {
    fill: gold;
    stroke: black;
}

svg #snitch-bar {
    opacity: 0.8;
    fill: gold;
    stroke: black;
}
```
