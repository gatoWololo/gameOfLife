-- | Main display function for the whole scene.
module Display (display) where
import Data.Time.Clock
import Graphics.UI.GLUT
import Control.Monad
import Camera
import Shapes
import Types
import Life
import Grid
-- ==========================================================================================
{- | Main draw function for OpenGL scene.
   Arguments:
   deltaAngle: State of angle based on user input.
   deltaMove: State of direction based on user input.
   xPos yPos zPos: Current postion of camera.
   camDir: Current Direction of camera.
-}
display :: DoubleIO -> DoubleIO -> DoubleIO -> VertexIO -> VectorIO -> Array2dIO -> TimeIO -> DisplayCallback
display moveFB moveLR moveUD camPos camDir ioGrid prevTime = do
  grid <- get ioGrid

  clear [ColorBuffer,DepthBuffer]
  loadIdentity

  --Update camera of world according to user keyboard and mouse input.
  updateCamera moveFB moveLR moveUD camPos camDir
  drawScene grid

  swapBuffers

  --Get time to see if we should update life to next iteration yet.
  t <- getCurrentTime
  prevT <- get prevTime
  if diffUTCTime t prevT > 1 then do
    grid' <- nextLife grid
    ioGrid $= grid'
    prevTime $= t
  else return ()
-- ==========================================================================================
drawScene :: Array2D -> IO()
drawScene grid = do
  --Zip up coordinate of cube with it's value.
  let lifeCoordPair = zip (getIntArray grid) lifeDrawPoints
  --Draw Cubes based on the state of life at that array index and move to next position.
  forM_ lifeCoordPair drawCube
-- ==========================================================================================
-- | Given a Coordinate representing the position to draw the cube at, plus the value of the
-- array at that point draw a life cube.
drawCube :: (Int,FloatCoord) -> IO()
drawCube (0,(x,z)) = preservingMatrix $ do
  translate (Vector3 x 0 z)
  color (Color3 1.0 0.3 (0::GLfloat))
  cubeFrame 1
drawCube (1,(x,z)) = preservingMatrix $ do
  color (Color3 1.0 0.5 (0::GLfloat))
  translate (Vector3 x 0 z)
  cubeSolid 1
  color (Color3 1.0 0.3 (0::GLfloat))
  cubeFrame 1
-- ==========================================================================================
-- | Given 3 points and a List it will draw square depeding on the value of list[n]
drawRow  :: [Int] -> IO ()
drawRow [] = return ()
drawRow (0:xs) = moveOne >> drawRow xs
drawRow (1:xs) = cubeSolid 1 >> moveOne >> drawRow xs

moveOne :: IO()
moveOne = translate (Vector3 1 0 (0::GLfloat))
moveDown = translate (Vector3 (-100) (-1) (0::GLfloat))
-- ==========================================================================================
-- | Get list of points that will be used for translationg for drawing cubes of life based
-- on the size of our matrix. On the x-z plane.
lifeDrawPoints :: [FloatCoord]
lifeDrawPoints = [(x,z)| x <- [0..n], z <- [0..n]]
    where n = (fromIntegral gridSize :: GLfloat) -1
