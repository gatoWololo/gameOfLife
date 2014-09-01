module Main where
{- |
   Conway's game of life written in Haskell. From random 1's and 0's
   By: Chava Rea
-}
import System.Environment(getArgs)
import System.Random
import System.IO
import System.Console.ANSI(clearScreen)
import Control.Monad (forever)
import Control.Concurrent (threadDelay)
import Data.List.Split(chunksOf)
--User Defined.
import Grid(getIntArray,gridSize,Array2D)
import Life(getInitGrid,nextLife)
import Graphics
--Needed for Graphics.
import qualified Graphics.UI.GLFW as GLFW
import Graphics.Rendering.OpenGL.Raw
import Data.Bits ((.|.))
--IO Refernces
import Data.IORef ( IORef, newIORef, readIORef, writeIORef )

main :: IO()
main = do
  grid' <- getInitGrid gridSize
  grid <- newIORef grid'
  window <- doGLFW (drawScene grid)
  forever $ do
         GLFW.pollEvents
         drawScene grid window
         GLFW.swapBuffers window

-- | Looping function reponsible for all the rendering and calculation of grids. Uses
-- | IORef objects to read and Write grids to allow for mutability.
drawScene :: IORef (Array2D) -> GLFW.Window -> IO ()
drawScene ioGrid _ = do
  grid <- readIORef ioGrid

  -- clear the screen and the depth bufer
  glClear $ fromIntegral (gl_COLOR_BUFFER_BIT .|. gl_DEPTH_BUFFER_BIT)
  glLoadIdentity -- reset view
  glTranslatef (-50.0) (50.0) (-100.0) --Initial Position.

  --Draws grid of Elements.
  mapM (\x -> drawRow x >> moveDown) $ intLists grid

  glFlush
  --Get next grid through IO references.
  grid' <- nextLife grid
  writeIORef ioGrid grid'
  threadDelay 500000
    where intLists grid = chunksOf gridSize $ getIntArray grid

-- | Given 3 points and a List it will draw square depeding on the value of list[n]
drawRow  :: [Int] -> IO ()
drawRow [] = return ()
drawRow (0:xs) = moveOne >> (drawRow xs)
drawRow (1:xs) = drawSquare >> moveOne >> (drawRow xs)

moveOne = glTranslatef 1 0 0
moveDown = glTranslatef (-100) (-1) 0
