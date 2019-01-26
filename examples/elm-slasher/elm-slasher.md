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
    { actors : { player : Actor, buggers : List Actor, snitch : Actor }
    , grid : Array (Array Cell)
    , time : Float
    , pause : Bool
    , lastPressed : String
    }

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
        { player = { location = (40.0, 0.0), velocity = (0.0, 1.0) }
        , buggers = []
        , snitch = { location = (0.0, 0.0), velocity = (0.0, 0.0) }
        }
    , grid = makeGrid (80, 50)
    , time = 0.0
    , pause = True
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
import Svg exposing (svg, circle, line, rect)
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
activeGridLoc {location, velocity} =
    let (x, y) = location
        (vx, vy) = velocity
    in
    if (abs vx) > (abs vy) then
        if vx > 0 then
            (round x, round y)
        else
            (round (x - 1.0), round y)
    else
        if vy > 0 then
            (round x, round y)
        else
            (round x, round (y - 1.0))


place : Cell -> Model -> Model
place cell ({actors, grid} as model) =
    let (i, j) = activeGridLoc actors.player
        row = get j grid
    in case row of
        Just r -> { model
                  | grid = set j (set i cell r) grid
                  }
        Nothing -> model

timeStep : Float -> Model -> Model
timeStep dt ({time, pause} as model) =
    if pause then model else {model | time = time + dt}

keyMap : String -> Model -> Model
keyMap k ({pause} as model) =
    case k of
        " " -> { model | pause = not pause }
        "ArrowLeft" -> place BackSlash model
        "ArrowRight" -> place Slash model
        _   -> model

placeSnitch : (Int, Int) -> Model -> Model
placeSnitch (x, y) ({actors} as model) = 
    { model 
    | actors = 
        { actors 
        | snitch =
            { location = (toFloat x, toFloat y)
            , velocity = (0.0, 0.0) } } }

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        TimeStep dt -> (timeStep dt model, Cmd.none)
        KeyPress k -> (keyMap k { model | lastPressed = k }, Cmd.none)
        PlaceSnitch l -> (placeSnitch l model, Cmd.none)
        -- _ -> (model, Cmd.none)
```

# View

``` {.elm #view}
scale : Int
scale = 15

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
        Empty ->     [ rect [ x (String.fromInt (scale * i))
                            , y (String.fromInt (scale * j))
                            , width (String.fromInt scale)
                            , height (String.fromInt scale)
                            , style "fill: none; stroke: black;" ] [] ]

viewArena : Model -> Html Msg
viewArena ({grid} as model) =
    svg [ width "100%"
        , viewBox ("0 0 " ++ (String.fromInt (scale * 80)) ++ " " ++ (String.fromInt (scale * 50)))]
        (concat (toList
            (indexedMap 
                (\ y rows -> (concat (toList (indexedMap 
                            (\ x cell -> viewCell (x, y) cell)
                            rows))))
                grid)))

view : Model -> Html Msg
view ({time, lastPressed} as model) =
    main_ []
        [ p [] [ text "Hello, World!" ]
        , div [] [ viewArena model ]
        , p [] [ text (String.fromFloat time) ]
        , p [] [ text ("Last pressed: " ++ lastPressed) ]
        ]
```
