module Life (nextLife,getInitGrid) where

-- | This module contains functions used by Conway's Game of Life for various operations.
--   Mostly related to array computations.
import Data.Array.Repa as R
import Grid (Array2D,findNumber,grid2D)
import System.Random
-- ==========================================================================================
-- | Treat rules like mapping of array. Make return a new array wrapped as an IO Action.
--   Not sure why actually...
nextLife:: Array2D -> IO (Array2D)
nextLife grid = do
  let lazyGrid = R.traverse grid id applyRules
  computeP lazyGrid :: IO (Array2D)
    where applyRules f p@(Z :. x :. y) = lifePred (findNumber grid (x,y)) (f p)
-- ==========================================================================================
-- | Life Predicate, Given a number returns true of false based on whether
-- the cell should be alive, we need to know it's current state i.e. dead | alive.
-- lifePred Sum -> State -> Int
lifePred:: Int -> Int -> Int
lifePred 3 0 = 1 --Any dead cell with exactly three live neighbours becomes a live cell, as if by reproduction.
lifePred _ 0 = 0 --Else remains dead.
lifePred n _
   | n < 2 = 0 --Any live cell with fewer than two live neighbours dies, as if caused by under-population.
   | n > 3 = 0 --Any live cell with more than three live neighbours dies, as if by overcrowding.
   | otherwise = 1 --Any live cell with two or three live neighbours lives on to the next generation.
-- ==========================================================================================
-- | Creates initial grid populated with random life.
getInitGrid:: Int -> IO (Array2D)
getInitGrid n = do
  randStream <- makeRandList
  let randList = take (n*n) randStream
  --Create actual grid from random list.
  let grid = fromListUnboxed grid2D randList
  return grid
-- ==========================================================================================
-- | Creates Lazy list of random 1's and 0's.
makeRandList:: IO [Int]
makeRandList = do
  g <- newStdGen
  return $ randomRs (0,1) g
-- ==========================================================================================
