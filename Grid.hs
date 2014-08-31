-- | General functions for array operations.
module Grid (grid2D,printArray,findNumber,Array2D,getElem,gridSize) where
import Data.Array.Repa

type Array2D = Array U DIM2 Int
type Coordinate = (Int,Int)

gridSize = 50

grid2D = Z :. gridSize :. gridSize :: DIM2

-- | Give a grid a number will find the sum of adjacent cells.
findNumber:: Array2D -> Coordinate -> Int
findNumber grid x = (getSum grid . filterNeighbors . getNeighbors) x

-- | Return the element at the (x,y) spot given an array.
getElem :: Array2D -> Coordinate -> Int
getElem grid (n,m) = grid ! (Z :. n :. m)

printArray :: Array2D -> String
printArray grid = unlines [unwords [toSquare (getElem grid (x,y)) | x <- [0..n]] | y <- [0..n]]
    where n = gridSize -1
          toSquare 0 = "□"
          toSquare 1 = "■"

-- | Returns a list of the neighbors to that index. Note: does not care if out of bounds in array.
getNeighbors:: Coordinate -> [Coordinate]
getNeighbors (n,m) = [(n+x,m+y)|x <- [-1..1], y <- [-1..1], x /= 0 || y /= 0]

{- | Since some adjacent cells may be out of bounds we will weed these guys out for simplicity.
   In the future we may wrap around the array. We will call it a torus at that point B) -}
filterNeighbors:: [Coordinate] -> [Coordinate]
filterNeighbors xs = filter (\(x,y) -> check x && check y) xs
    where n = gridSize
          check x = x < n && x >= 0

-- | Given a list of tuples and a matrix will return the sum of adding the number at those spots.
getSum:: Array2D -> [Coordinate] -> Int
getSum grid list = foldr (\x -> ((getElem grid x) +)) 0 list


