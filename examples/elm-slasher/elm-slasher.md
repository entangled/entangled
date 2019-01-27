---
title: Slasher game in Elm
author: Johan Hidding
---

What better waste of time than writing a mind numbing game in a language you don't know? Let's dive into [Elm](https://elm-lang.org/). Elm is a functional language that compiles to JavaScript. Its design is centred around the Model/View/Controller concept.

``` {.elm file=src/Main.elm}
<<imports>>
<<main>>

<<model>>
<<update>>
<<subscriptions>>
<<view>>
```

Our game is simple. The original *slasher* game is one I remember from way back when I was playing with [GW Basic](https://en.wikipedia.org/wiki/GW-BASIC). Your hero is zipping around the screen at break neck speeds and the only way to control him is by placing slash `/` and backslash `\` characters from which the bugger bounces off. The goal is to catch the golden snitch which only stays in one place for so long.

``` {.elm #main}
main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view }
```

# The Model

The *model* will be a grid of cells and a list of actors with location and velocity.

``` {.elm #model}
type alias Model =
    { actors : { player : Actor, snitch : Actor }
    , grid : Grid
    , time : Float
    , state : GameState
    , lastPressed : String
    }

type GameState = Start | Running | Pause | Win

type alias Grid = Array (Array Cell)

type alias Actor =
    { location : (Float, Float)
    , velocity : (Float, Float)
    }

type Cell
    = Empty
    | Slash
    | BackSlash

makeGrid : (Int, Int) -> Array (Array Cell)
makeGrid (width, height) = repeat height (repeat width Empty)

init : () -> (Model, Cmd Msg)
init _ = (
    { actors =
        { player = { location = (40.5, 0.5), velocity = (0.0, 0.03) }
        , snitch = { location = (0.0, 0.0), velocity = (0.0, 0.0) }
        }
    , grid = makeGrid (80, 50)
    , time = 0.0
    , state = Pause
    , lastPressed = ""
    }, Cmd.batch
    [ Random.generate PlaceSnitch (Random.pair (Random.int 0 79)
                                               (Random.int 0 49))
    ])
```

# Events

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

``` {.elm #imports}
import Browser
import Array exposing (Array, repeat, indexedMap, toList, set, get)
import List exposing (concat)
import Browser.Events exposing (onAnimationFrameDelta, onKeyDown)
import Html exposing (Html, button, div, text, p, input, main_)
import Html.Events exposing (onClick, preventDefaultOn)
import Svg exposing (svg, circle, line, rect, g, polygon)
import Svg.Attributes exposing (..)
import Random

import Json.Decode as Decode
```

# Update

``` {.elm #update}
type Msg
    = KeyPress String
    | TimeStep Float
    | PlaceSnitch (Int, Int)

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
timeStep dt ({actors, grid, time, state} as model) =
    if state /= Running then model 
    else { model
        | time = time + dt
        , actors = { actors
            | player = updateActor grid dt actors.player } }

keyMap : String -> Model -> Model
keyMap k ({state} as model) =
    case k of
        " " -> { model | state = if state == Pause then Running else Pause }
        "ArrowLeft" -> place BackSlash model
        "ArrowRight" -> place Slash model
        _   -> model

placeSnitch : (Int, Int) -> Model -> Model
placeSnitch (x, y) ({actors} as model) = 
    { model 
    | actors = 
        { actors 
        | snitch =
            { location = ((toFloat x) + 0.5, (toFloat y) + 0.5)
            , velocity = (0.0, 0.0) } } }

didWeWin : Model -> Bool
didWeWin ({actors} as model) =
    activeGridLoc actors.snitch == activeGridLoc actors.player

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        TimeStep dt -> let next = timeStep dt model in
                       if didWeWin next
                       then ({ next | state = Win } , Cmd.none)
                       else (next, Cmd.none)
        KeyPress k -> (keyMap k { model | lastPressed = k }, Cmd.none)
        PlaceSnitch l -> (placeSnitch l model, Cmd.none)
        -- _ -> (model, Cmd.none)
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
    in polygon [ points <| formatPath path, style "fill: red" ] []

viewSnitch : Actor -> Html Msg
viewSnitch actor =
    let (x, y) = actor.location
    in circle [ cx (String.fromFloat <| x * fScale)
              , cy (String.fromFloat <| y * fScale)
              , r (String.fromInt <| scale // 2)
              , style "fill: gold"] []

viewArena : Model -> Html Msg
viewArena ({actors, grid} as model) =
    svg [ width "100%"
        , viewBox ("-3 -3 " ++ (String.fromInt <| scale * 80 + 4) ++ " " ++ (String.fromInt <| scale * 50 + 4))]
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
        ]

view : Model -> Html Msg
view ({time, lastPressed} as model) =
    main_ []
        [ p [] [ text "Hello, World!" ]
        , div [] [ viewArena model ]
        , p [] [ text (String.fromFloat time) ]
        , p [] [ text ("Last pressed: \"" ++ lastPressed ++ "\"") ]
        ]
```
