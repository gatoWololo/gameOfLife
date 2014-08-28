module Main where
{- |
   Conway's game of life written in Haskell. From random 1's and 0's
-}

import System.Environment(getArgs)
import System.Random
import Grid(printArray,gridSize)
import Life(getInitGrid,nextLife)

main :: IO()
main = do
  grid <- getInitGrid gridSize
  nextGrid <- nextLife grid
  putStrLn $ printArray grid
  putStrLn $ printArray nextGrid
