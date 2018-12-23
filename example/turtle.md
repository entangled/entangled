# Turtle Graphics in Python

In this example with pictures, we'll use Python and Cairo to render L-systems. 

An L-system ([Wikipedia](https://en.wikipedia.org/wiki/L-system)), short for Lindenmayer system is a notation for symbolic rewriting rules. We start with a short string and a few rules on replacing letters in such a string into longer strings. We may start with a simple example, `algae`.

We start with the string `"A"`, replace all `'A'`s with `"AB"` and all `'B'`s with `"A"`. Repeat this process. This resulting sequence starts with

| n   | algae(n)              |
| --- | --------------------- |
| 0   | A                     |
| 1   | AB                    |
| 2   | ABA                   |
| 3   | ABAAB                 |
| 4   | ABAABABA              |
| 5   | ABAABABAABAAB         |

We can implement such an L-system using the string formatting facility in Python. The following generator creates the above sequence:

``` {.py file=algae.py}
from itertools import islice

def algae():
    rules = {"A": "{A}{B}", "B": "{A}"}
    state = "{A}"
    vardict = {k: k for k in rules}
    while True:
        yield state.format(**vardict)
        state = state.format(**rules)

for x in islice(algae(), 6):
    print(x)
```

Notice that all the data about the L-system is contained in a string and a dictionary. This means that such a system can be easily parsed from a *YAML* file. For our next example we'll implement a more generic L-system expander.

## Generalised L-system

First, we have to write the generic L-system generator! This function is the same as our earliest version, except that the L-system is no longer hard-coded.

``` {.py #generic-lsystem-generator}
def lsystem(*, rules, axiom, **kwargs):
    <<define-quotevars>>

    rules = {k: quotevars(r) for k, r in rules.items()}
    vardict = {k: k for k in rules}
    state = quotevars(axiom)

    while True:
        yield state.format(**vardict)
        state = state.format(**rules)
```

We have to 'quote' some of the variables into `{`, `}` pairs, so that we can do string formatting on them. This leads to the rather exotic looking `expr.replace(v, "{{{}}}".format(v))`. Appreciate the little things in life ;).

``` {.py #define-quotevars}
def quotevars(expr):
    for v in rules:
        expr = expr.replace(v, "{{{}}}".format(v))
    return expr
```

For our second example we'll take the binary tree example:

``` {.yaml file=binary-tree.yaml}
l-system:
    vars:
        - O
        - I
    rules:
        O: "I[O]O"
        I: "II"
    axiom: "O"

<<binary-tree-turtle-actions>>
<<binary-tree-render-options>>
```

Here, we introduced the characters `[` and `]` that are not used in the recursion, but will give meaning when we will render the resulting tree. Now we can run the binary tree L-system by loading the YAML file and calling the generic L-system generator `lsystem`! Also, notice that we will expand the file `binary-tree.yaml` later on to implement rendering.

``` {.py file=binary-tree.py}
import yaml
import sys
from itertools import islice

<<generic-lsystem-generator>>

data = yaml.safe_load(open("binary-tree.yaml"))
bt = lsystem(**data["l-system"])
for x in islice(bt, 5):
    print(x)
```

## Turtle graphics

L-systems are well suited for rendering with Turtle graphics ([Wikipedia](https://en.wikipedia.org/wiki/Turtle_graphics)). We imagine a little turtle crawling over the screen. It acts as a little state machine that listens to a set of commands. In our case these are the following:

* **walk d**: walk a certain distance *d*.
* **jump d**: jump a certain distance *d* (doesn't draw).
* **turn a**: turn an angle *a* (in degrees).
* **push**: remember the current state.
* **pop**: restore the last saved state.

``` {.py file=turtle.py}
import numpy as np

class Turtle:
    def __init__(self, context):
        self._cx = context
        self._direction = 0.0
        self._x = 0.0
        self._y = 0.0
        self._stack = []

    @property
    def state(self):
        return (self._x, self._y, self._direction)

    @state.setter
    def state(self, v):
        self._x, self._y, self._direction = v

    <<turtle-step>>
    <<turtle-push-pop>>

    def walk(self, d):
        self._step(d)
        self._cx.line_to(self._x, self._y)
    
    def jump(self, d):
        self._step(d)
        self._cx.move_to(self._x, self._y)

    def turn(self, delta):
        self._direction += delta
```

``` {.py #turtle-step}
def _step(self, d):
    phi = np.radians(self._direction)
    dx, dy = d * np.cos(phi), d * np.sin(phi)
    self._x += dx
    self._y += dy
```

``` {.py #turtle-push-pop}
def push(self):
    self._stack.append(self.state)

def pop(self):
    self.state = self._stack.pop()
    self._cx.move_to(self._x, self._y)
```

Now let's render our tree.

``` {.py file=tree.py}
import cairo
import numpy as np
import yaml
import sys
from itertools import islice
from turtle import Turtle

<<generic-lsystem-generator>>

def draw_lsystem(cx, data, n_iter):
    s = lsystem(**data)
    t = Turtle(cx)
    string = next(islice(s, n_iter, None))
    for x in string:
        if x == "[":
            t.push()
            t.turn(45)
        elif x == "]":
            t.pop()
            t.turn(-45)
        elif x == "O" or x == "I":
            t.walk(1)

lsystem_data = yaml.safe_load(open("binary-tree.yaml"))["l-system"]

with cairo.SVGSurface("tree.svg", 200, 200) as surface:
    cx = cairo.Context(surface)
    cx.translate(0, 100)
    cx.scale(1.5, 1.5)
    cx.move_to(0, 0)
    draw_lsystem(cx, lsystem_data, 7)
    cx.set_line_width(0.6)
    cx.set_source_rgb(0, 0, 0)
    cx.stroke()
```

 We still need to translate the characters in the resulting string into actions on the turtle.

``` {.yaml #binary-tree-turtle-actions}
turtle:
    "[": [push, turn +45]
    "]": [pop, turn -45]
    "O": [walk 1]
    "I": [walk 1]
```

``` {.yaml #binary-tree-render-options}
render:
    filename: binary-tree.svg
    width: 200
    height: 200
    preamble: [translate 0 100, scale 1.5 1.5, move-to 0 0]
    iteration: 7
    postamble: [set-line-width 0.5, set-source-rgb 0 0 0, stroke]
```

## Executable script

We'll put the generic L-system generator in a module:

``` {.py file=lsystem.py}
from collections import defaultdict
from pyparsing import (
    Literal,Word, alphas, nums, ZeroOrMore, OneOrMore, Combine, Optional)
from inspect import signature
from itertools import islice
import sys

import yaml
from jsonschema import (validate, ValidationError)
import cairo

from turtle import Turtle

<<generic-lsystem-generator>>

turtle_allowed = ["walk", "jump", "turn", "push", "pop"]
cairo_allowed = ["translate", "scale", "move_to", "stroke", "set_source_rgb", "set_line_width"]

def action(turtle, expr, allowed):
    sign = Literal("+") | Literal("-")
    command = Word(alphas + '-') \
        .setParseAction(lambda s, l, t: t[0].replace('-', '_'))
    real = Combine(Optional(sign) + Word(nums) +
                   Optional(Literal(".") + Word(nums))) \
        .setParseAction(lambda s, l, t: float(t[0]))
    expression = (command + ZeroOrMore(real))

    cmd = list(expression.parseString(expr))
    if cmd[0] not in allowed:
        raise ValueError(
            f"Command `{cmd[0]}` is not allowed. List of allowed "
            f"commands:\n    {allowed}.")

    if not hasattr(turtle, cmd[0]):
        raise ValueError(
            f"Turtle doesn't know how to `{cmd[0]}` "
            f"in expression `{expr}`.")

    try:
        n_args = len(signature(getattr(turtle, cmd[0])).parameters)
        if n_args != len(cmd[1:]):
            raise ValueError(
                f"Turtle command `{cmd[0]}` needs {n_args} arguments "
                f"in expression `{expr}`.")
    except ValueError:
        pass
    
    return lambda: getattr(turtle, cmd[0])(*cmd[1:])

def render(data):
    filename = data["render"]["filename"]
    width = data["render"]["width"]
    height = data["render"]["height"]
    preamble = data["render"]["preamble"]
    postamble = data["render"]["postamble"]
    lsystem_gen = lsystem(**data["l-system"])

    with cairo.SVGSurface(filename, width, height) as surface:
        context = cairo.Context(surface)
        for expr in preamble:
            action(context, expr, cairo_allowed)()
        
        turtle = Turtle(context)
        actions = defaultdict(list, 
            {k: [action(turtle, v, turtle_allowed) for v in vs]
            for k, vs in data["turtle"].items()})

        n_iter = data["render"]["iteration"]
        string = next(islice(lsystem_gen, n_iter, None))

        for x in string:
            for a in actions[x]:
                a()

        for expr in postamble:
            action(context, expr, cairo_allowed)()

def load_lsystem(filename):
    try:
        schema = yaml.safe_load(open("lsystem-schema.yaml"))
        data = yaml.safe_load(open(filename))
        validate(data, schema)
    except yaml.YAMLError as exc:
        print(exc)
        sys.exit()
    except ValidationError as err:
        print(err)
        sys.exit()

    return data

if __name__ == "__main__":
    import argparse
    parser = argparse.ArgumentParser(description="Render L-systems.")
    parser.add_argument("filename", help="input YAML file")
    args = parser.parse_args()
    data = load_lsystem(args.filename)
    render(data)
```

## Example: Barnsley fern

``` {.yaml file=barnsley-fern.yaml}
l-system:
    rules:
        "X": "F+[[X]-X]-F[-FX]+X"
        "F": "FF"
    axiom: "X"

turtle:
    "[": [push]
    "]": [pop]
    "F": [walk 1]
    "+": [turn +25]
    "-": [turn -25]

render:
    filename: barnsley-fern.svg
    width: 200
    height: 200
    preamble: [translate 0 100, scale 2 2, move-to 0 0]
    iteration: 5
    postamble: [set-line-width 0.5, set-source-rgb 0 0 0, stroke]
```

## A Prettier Turtle

We'll design a Turtle graphics class that can do variable line thickness and pen color. We'll call this class `Galapagos`, technically a tortoise.

``` {.py file=galapagos.py}
import numpy as np
from numpy import linalg as la
from collections import (namedtuple, defaultdict)

Colour = namedtuple("Colour",
    ["r", "g", "b", "a"])

Point = namedtuple("Point",
    ["p", "width", "colour"])

class Galapagos:
    def __init__(self):
        self._x = 0.0
        self._y = 0.0
        self._direction = 0.0
        self._speed = 1.0
        self._width = 1.0
        self._colour = Colour(0.0, 0.0, 0.0, 1.0)
        self._index = None
        self._stack = []

        self.points = []
        self.edges = defaultdict(list, {})
    
    @property
    def state(self):
        return (self._x, self._y, self._direction, self._speed,
                self._width, self._colour, self._index)
    
    @state.setter
    def state(self, v):
        self._x, self._y, self._direction, self._speed \
        self._thickness, self._colour, self._index = v

    def _step(self, d):
        r = self._speed * d
        phi = np.radians(self._direction)
        self._x += r * np.cos(phi)
        self._y += r * np.sin(phi)

    @property
    def point(self):
        return Point(np.r_[self._x, self._y], self._width, self._colour)

    <<turtle-push-pop>>

    def walk(self, d):
        self._step(d)
        prev = self._index
        self.points.append(self.point)
        self._index = len(self.points)
        self.edges[self._index].append(prev)
        self.edges[prev].append(self._index)

    def jump(self, d):
        self._step(d)
        self.points.append(self.point)
        self._index = len(self.points)
    
    def shrink(self, f):
        self._width *= f
    
    def slow_down(self, f):
        self._speed *= f

    def turn(self, a):
        self._direction += a
    
    def rgba(self, r, g, b, a):
        self._colour = Colour(r, g, b, a)
    
    def darken(self, f):
        r, g, b, a = self._colour
        self._colour = Colour(r * f, g * f, b * f, a)
    
    def fade(self, f):
        self._colour.a *= f

<<render-painting>>
```

Painting such a structure is not straight forward. In stead of drawing lines we will trace a path around the rendered L-system. We'll need some math to do this. Suppose, at point $a$ we have a circle and at point $b$ a second circle with possibly a different radius.

![Bitangent to two circles](bitangent.svg)

We may draw the bitangent to both circles by computing the coordinates at $\bar{a} + \vec{r_a}$ and $\bar{b} + \vec{r_b}$. We'll denote the coordinates of $\vec{r_a} = (a_{\perp}, a_{\parallel})$, for the parts perpendicular and parallel to the line $\bar{ab}$. Then it can be shown that

$$\begin{aligned}
a_{\perp} &= |r_a| \sqrt{1 - \left(\frac{r_a - r_b}{|b - a|}\right)^2}\\
a_{\parallel} &= |r_a| \left(\frac{r_a - r_b}{|b - a|}\right),
\end{aligned}$$

and that

$$\vec{r_b} = \frac{|r_b|}{|r_a|} \vec{r_a}.$$

``` {.gnuplot file=bitangent.gnuplot}
reset
set term svg enhanced size 480, 300 font "Times"
set output 'bitangent.svg'

ra = 1
rb = 0.5
m = 2.0
ax = ra*((ra - rb)/m)
ay = sqrt(ra**2 - ax**2)
bx = (rb/ra)*ax
by = (rb/ra)*ay

set grid
set size ratio -1

set parametric
set trange [0:1]
set yrange [-1.2:1.2]
set xrange [-1.2:2.7]

set label "a" at 0, 0 point pt 7 ps 0.5 offset first -0.15, -0.15
set label "b" at m, 0 point pt 7 ps 0.5 offset first -0.15, -0.15
set label "r_a" at ax, ay offset first 0.03,0.1
set label "r_b" at m+bx, by offset first 0.03,0.1

set arrow 1 from 0,0 to ax, ay head filled size screen 0.02,15,45
set arrow 2 from m,0 to m+bx, by head filled size screen 0.02,15,45
set arrow from 0,-0.05 to ax, -0.05 heads size screen 0.008,90
set arrow from ax + 0.05, 0 to ax + 0.05, ay heads size screen 0.008,90
set label "a_∥" at ax/2, -0.13
set label "a_⟂" at ax+0.1, ay/2

plot ra*cos(2*pi*t), ra*sin(2*pi*t) t'' w l ls 1, \
     rb*cos(2*pi*t) + m, rb*sin(2*pi*t) t'' w l ls 1, \
     ax+t*(m+bx-ax), ay+t*(by-ay) t'' w l ls 2, \
     ax+t*(m+bx-ax), -ay-t*(by-ay) t'' w l ls 2
```

``` {.py #render-painting}
Edge = recordclass("Edge", ["target", "p1" "p2", "done"])

def bitangent(tgt, a, b):
    m = np.r_[b.p[0] - a.p[0], b.p[1] - a.p[1]]
    n = np.r_[b.p[1] - a.p[1], a.p[0] - b.p[0]]
    m2 = m.dot(m)
    
    f = (a.width - b.width) / (2 * m2)
    xa = a.width/2 * (f*m + np.sqrt(1/m2 - f**2)*n)
    xb = (b.width / a.width) * xa
    return Edge(tgt, xa, xb, False)

def render_painting(cx, points, edges):
    def vector_angle(x):
        return np.arctan2(x[:, 1], x[:, 0])
    
    es = {src: [bitangent(tgt, points[src], points[tgt])
                for tgt in tgts]
          for src, tgts in edges.items()}
        
    def find_next_edge(src, last):
        ps = [e for e in es[src] if e.target != last]
        x = np.array([points[p.target].p for p in ps])
        inc_angle = vector_angle(points[latest].p - points[src].p)
        angles = (vector_angle(x - points[src].p) - inc_angle) % (2 * np.pi)
        y = np.argmin(angles)
        return ps[y]
        
    while True:
        try:
            src = next(k for k, v in es.items() if any(not e.done for e in v))
        except StopIteration:
            return

        e = es[src][0]
        cx.move_to(*src.p1)
        
```

## YAML validation

### Pretty printing a Schema

``` {.py file=pretty_print_schema.py}
import textwrap

def coroutine(f):
    def g(*args, **kwargs):
        s = f(*args, **kwargs)
        next(s)
        return s
    return g

@coroutine
def indent(first, pre, sink):
    line = yield
    sink.send(first + line)
    while True:
        line = yield
        sink.send(pre + line)

@coroutine
def line_wrap(initpre, postpre, width, sink):
    while True:
        line = yield
        text = textwrap.wrap(line,
            width=width,
            initial_indent=initpre,
            subsequent_indent=postpre)
        for line in text:
            sink.send(line)
        
@coroutine
def print_sink():
    while True:
        line = yield
        print(line)

def pretty_print_schema(s, sink=print_sink()):
    description = s.get("description", "")
    data_type = s.get("type", "<no type>")
    if "type" not in s and "$ref" in s:
        data_type = s.get("$ref").split('/')[-1]
    if data_type == "object":
        line_wrap("# ", "# ", 60, sink) \
            .send(description)
    else:
        line_wrap("# <" + data_type + "> ", "# ", 60, sink) \
            .send(description)
    
    if "definitions" in s:
        sink.send("#")
        sink.send("# definitions:")
        for k, d in s["definitions"].items():
            sink.send("#   " + k + ":")
            pretty_print_schema(d, indent("#     ", "#     ", sink))
        sink.send("#")
            
    if data_type == "object":
        if "required" in s:
            line_wrap("# required: ", "#            ", 60, sink) \
                .send("{}".format(s["required"]))
            sink.send("#")
        if "properties" in s:
            for k, p in s["properties"].items():
                sink.send(k + ": ")
                pretty_print_schema(p, indent("  ", "  ", sink))
                sink.send("")
        if "additionalProperties" in s:
            p = s["additionalProperties"]
            pretty_print_schema(p, indent("# <key>: ", "#        ", sink))
            
    elif data_type == "array":
        pretty_print_schema(s["items"], indent("- ", "- ", sink))
        sink.send("- ...")
```

``` {.py file=test_schema.py}
from pretty_print_schema import pretty_print_schema
import yaml
from jsonschema import validate

schema = yaml.safe_load(open("lsystem-schema.yaml"))
data = yaml.safe_load(open("barnsley-fern.yaml"))
validate(data, schema)
pretty_print_schema(schema)
```
