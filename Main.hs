module Main where
{- |
   Conway's game of life written in Haskell. From random 1's and 0's
   By: Chava Rea
-}

import System.Environment(getArgs)
import System.Random
import System.IO
import Control.Concurrent(threadDelay)
import Grid(printArray,gridSize,Array2D)
import Life(getInitGrid,nextLife)
import System.Console.ANSI(clearScreen)

main :: IO()
main = do
  n:[] <- getArgs --No error :P
  let timesN = read n :: Int
  grid <- getInitGrid gridSize
  iterateN grid timesN


-- | Given a number as a command Line argument will iterate that many times through grids printing them.
iterateN :: Array2D -> Int -> IO ()
iterateN grid 1 = toScreen grid
iterateN grid n = do
  nextGrid <- nextLife grid
  toScreen nextGrid
  iterateN nextGrid $ (n-1)

toScreen :: Array2D -> IO ()
toScreen grid = do
  clearScreen
  hFlush stdout
  putStrLn $ printArray grid
  hFlush stdout
  threadDelay 1000000
