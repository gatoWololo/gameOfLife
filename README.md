# Conway's Game of Life

## Description
Conway's Game Of Life, cellular autamaton implemented in haskell using Repa mutable arrays
to represent data. OpenGL and Glut binding for graphics implementations.

![](images/gameOfLife2.png "PRogram Running:")


Arrow Keys, mouse, space bar and shift work to move about the world. Note mouse camera
rotation still needs some work.

# Build

Clone this repo:

    git clone https://github.com/gatoWololo/gameOfLife.git

Use Haskell's build tool `Stack`:

    stack build # compile
    stack exec GameOfLife-exe # run

Warning! Glut bindings take a long time to compile.

Much optimization to be done!


