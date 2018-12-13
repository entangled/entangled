# Computing the Mandelbrot fractal using C++ and OpenCL

``` {.cpp file=src/common.hh}
#pragma once

#include "types.hh"

extern int iterate(function f, predicate pred, complex z, unsigned maxint);
extern bool pred(complex z);
extern function f(complex c);
extern void render(predicate pred, complex a, complex b, unsigned width);
extern void render_double_colour(unit_map f, complex a, complex b, unsigned width);

extern predicate mandelbrot(int maxit);
extern predicate julia(complex c, int maxit);
extern unit_map mandelbrot_c(int maxit);
extern unit_map julia_c(complex c, int maxit);
```

a

``` {.cpp file=src/common.cc}
#include "types.hh"

function f(complex c) {
    return [c] (complex z) {
        return z*z + c;
    };
}

bool pred(complex z) {
    return std::norm(z) < 4.0;
}
```

a

``` {.cpp file=src/iterate.cc}
#include "types.hh"

int iterate(function f, predicate pred, complex z, unsigned maxit) {
    unsigned i = 0;

    while (pred(z) && i < maxit) {
        z = f(z);
        ++i;
    }

    return i;
}
```

a

``` {.cpp file=src/julia.cc}
#include "common.hh"
#include <cmath>

predicate julia(complex c, int maxit) {
    return [c, maxit] (complex z) {
        return iterate(f(c), pred, z, maxit) == maxit;
    };
}

unit_map julia_c(complex c, int maxit) {
    return [c, maxit] (complex z) {
        return sqrt(double(iterate(f(c), pred, z, maxit)) / maxit);
    };
}
```

a

``` {.cpp file=src/mandel.cc}
#include <cmath>
#include "common.hh"

predicate mandelbrot(int maxit) {
    return [maxit] (complex c) {
        return iterate(f(c), pred, complex(0, 0), maxit) == maxit;
    };
}

unit_map mandelbrot_c(int maxit) {
    return [maxit] (complex c) {
        return sqrt(double(iterate(f(c), pred, complex(0, 0), maxit)) / maxit);
    };
}
```

a

``` {.cpp file=src/read.hh}
#pragma once

#include <string>
#include <sstream>

template <typename T>
T read(std::string s) {
    T v;
    std::istringstream iss(s);
    iss >> v;
    return v;
}
```

a

``` {.cpp file=src/render.cc}
#include "types.hh"
#include <iostream>

void render(predicate pred, complex a, complex b, unsigned width) {
    unsigned height = width/3;
    double scale_real = (b.real() - a.real()) / width;
    double scale_imag = (b.imag() - a.imag()) / height;

    for (unsigned j = 0; j < height; ++j) {
        for (unsigned i = 0; i < width; ++i) {
            complex c = a + complex(i * scale_real, j * scale_imag);

            if (pred(c))
                std::cout << '#';
            else
                std::cout << ' ';
        }
        std::cout << std::endl;
    }
}

struct Colour {
    int r, g, b;

    Colour(int r_, int g_, int b_):
        r(r_), g(g_), b(b_) {}
};

Colour colour_map(double x) {
    double r = (0.472-0.567*x+4.05*pow(x, 2))
                /(1.+8.72*x-19.17*pow(x, 2)+14.1*pow(x, 3)),
           g = 0.108932-1.22635*x+27.284*pow(x, 2)-98.577*pow(x, 3)
                +163.3*pow(x, 4)-131.395*pow(x, 5)+40.634*pow(x, 6),
           b = 1./(1.97+3.54*x-68.5*pow(x, 2)+243*pow(x, 3)
                -297*pow(x, 4)+125*pow(x, 5));

    return Colour(int(r*255), int(g*255), int(b*255));
}

void render_double_colour(unit_map f, complex a, complex b, unsigned width) {
    unsigned height = (width * 10) / 16;
    double scale_real = (b.real() - a.real()) / width;
    double scale_imag = (b.imag() - a.imag()) / height;

    for (unsigned j = 0; j < height; j += 2) {
        for (unsigned i = 0; i < width; ++i) {
            complex c1 = a + complex(i * scale_real, j * scale_imag);
            complex c2 = a + complex(i * scale_real, (j+1) * scale_imag);
            auto clr1 = colour_map(f(c1)),
                 clr2 = colour_map(f(c2));

            std::cout << "\033[38;2;" << clr1.r << ";"
                      << clr1.g << ";" << clr1.b << "m"
                      << "\033[48;2;" << clr2.r << ";"
                      << clr2.g << ";" << clr2.b << "m▀";
        }
        std::cout << "\033[m\n";
    }
}
```

a

``` {.cpp file=src/types.hh}
#pragma once

#include <complex>
#include <functional>

using complex = std::complex<double>;
using function = std::function<complex (complex)>;
using predicate = std::function<bool (complex)>;
using unit_map = std::function<double (complex)>;
```

## Main

``` {.cpp file=src/main.cc}
#include <iostream>
#include <string>
#include <vector>
#include <unistd.h>

#include "../src/common.hh"
#include "../src/read.hh"

int main(int argc_, char **argv_) {
    std::vector<std::string> argv(argv_, argv_ + argc_);

    int columns = 80;
    bool use_colour = false;
    int opt;
    if (argv.size() >= 2 and argv[1] == "mandelbrot") {
        while ((opt = getopt(argc_, argv_, "cw:")) != -1) {
            switch (opt) {
                case 'c': use_colour = true; break;
                case 'w': columns = read<int>(optarg);
            }
        }

        if (use_colour) {
            render_double_colour(mandelbrot_c(256),
                complex(-2.15, -1), complex(0.85, 1),
                columns);
        } else {
            render(mandelbrot(256),
                complex(-2, -1), complex(1, 1),
                columns);
        }
        exit(0);
    }

    if (argv.size() >= 4 and argv[1] == "julia") {
        complex c(read<double>(argv[2]), read<double>(argv[3]));
        while ((opt = getopt(argc_ - 3, argv_ + 3, "cw:")) != -1) {
            switch (opt) {
                case 'c': use_colour = true; break;
                case 'w': columns = read<int>(optarg);
            }
        }

        if (use_colour) {
            render_double_colour(julia_c(c, 256),
                complex(-2, -1.2), complex(2, 1.2),
                columns);
        } else {
            render(julia(c, 256),
                complex(-2, -1.2), complex(2, 1.2),
                columns);
        }
        exit(0);
    }

    std::cout << "Toy fractal renderer." << std::endl;
    std::cout << "usage: " << argv[0] << " mandelbrot [-c] [-w <n>] | julia <real> <imag> [-c] [-w <n>]\n\n";
    std::cout << "Some nice coordinates for the Julia set: \n"
              << "    0.26, -1, -0.123+0.745i, -1+0.29i, -0.03+0.7i, etc. \n"
              << "Pro tip: set your terminal to fullscreen and tiny font, \n"
              << "    then run with '-w $COLUMNS'.\n";
    std::cout << "The '-c' option adds colour; have fun!" << std::endl;
}
```
