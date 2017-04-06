Conway's game of life implementation in Haskell.

Conway's Game Of Life, cellular autamaton implemented in haskell using Repa mutable arrays
to represent data. OpenGL and Glut binding for graphics implementations.

![](images/gameOfLife2.png "PRogram Running:")

Arrow Keys, mouse, space bar and shift work to move about the world. Note mouse camera
rotation still needs some work.

Uses stack. Usage:

    stack build # compile
    stack exec GameOfLife-exe # run

Warning! Glut bindings take a long time to compile.

Much optimization to be done!


