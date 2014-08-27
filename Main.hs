module Main where
{- |
   Conway's game of life written in Haskell. From random 1's and 0's
-}

import System.Environment(getArgs)
import System.Random
import Data.Array.Repa
import Grid

main :: IO()
main = do
  randStream <-  makeRandList
  let randList = take  (gridSize*gridSize) randStream
      --We have created our grid of randomly assinged life.
      grid = fromListUnboxed lifeGrid randList
  putStrLn $ printArray grid
  putStrLn . show $ findNumber grid (0,0)



{- | Creates Lazy list of random 1's and 0's. -}
makeRandList:: IO [Int]
makeRandList = do
  g <- newStdGen
  return $ randomRs (0,1) g

