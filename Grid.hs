module Grid (lifeGrid,gridSize,printArray,findNumber,Board) where
import Data.Array.Repa

lifeGrid = (Z :. gridSize :. gridSize :: DIM2)
type Board = Array U DIM2 Int
type Coordinate = (Int,Int)

gridSize :: Int
gridSize = 30

{- | Give a grid a number will find the sum of the neighbors. As needed by the rules of Game of Life. -}
findNumber:: Board -> Coordinate -> Int
findNumber grid x = (getSum grid . filterNeighbors . getNeighbors) x

{- | Return the element at the (x,y) spot given an array -}
getElem :: Board -> Coordinate -> Int
getElem grid (n,m) = grid ! (Z :. n :. m)

printArray :: Board -> String
printArray grid = unlines [unwords [show (getElem grid (x,y)) | x <- [0..n]] | y <- [0..n]]
    where n = gridSize - 1

{- | Returns a list of the neighbors to that index. Note: does not care if out of bounds in array -}
getNeighbors:: Coordinate -> [Coordinate]
getNeighbors (n,m) = [(n+x,m+y)|x <- [-1..1], y <- [-1..1], x /= 0 || y /= 0]

{- | Since some neighbors may be out of bounds we will weed these guys out for simplicity.
   In the future we may wrap around the array, to feel cool we will call it a torus B) -}
filterNeighbors:: [Coordinate] -> [Coordinate]
filterNeighbors xs = filter (\(x,y) -> check x && check y) xs
    where n = gridSize
          check x = x <= n && x >= 0

{- | Given a list of tuples and a matrix will return the sum of adding the number at those spots -}
getSum:: Board ->[Coordinate] -> Int
getSum grid list = foldr (\x -> ((getElem grid x) +)) 0 list

