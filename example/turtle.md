# Turtle Graphics in Python

``` {.py file=turtle.py}
import cairo
import numpy as np

with cairo.SVGSurface("dragon.svg", 200, 200) as surface:
    cx = cairo.Context(surface)
    cx.arc(100, 100, 50, 0, 2*np.pi)
    cx.set_source_rgb(1, 0, 0)  # red fill
    cx.fill_preserve()
    cx.set_source_rgb(0, 0, 0)  # black stroke
    cx.stroke()
```
