Genetic Algorithms
==================

Assignment for Comp 5206 - Evolutionary Computation - Carleton University

# Run

## One Max

    $ racket maxone.rkt

## Simple Max

    $ racket simple-max.rkt

## Travelling Salesperson

There is a boolean flag in the code named `*example*`. When this is `#t`, the program will output crossover, mutation, and selection examples. 

When `*example*` is set to `#f`, the given filename specifying city locations will be opened, and a GUI depicting controls for a GA on the given city list.

    $ racket tsp.rkt <filename> <separator> <column>

Example using the berlin52 problem:

    $ racket tsp.rkt berlin52.txt “ “ 1

The `filename` should refer to a list of cities. An example list is provided for the berlin52 problem (`berlin52.txt`). In general, it should be of the form:

```
x1,y1
x2,y2
x3,y3
etc
```

The `separator` indicates the separator between the values in the file. The above example uses a comma, while the berlin52 example uses spaces.

The `column` specifies which column of the file the coordinates begin at. All columns before this are ignored. The above example would specify 0, while the berlin52 problem starts at 1.
