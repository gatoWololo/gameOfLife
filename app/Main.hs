-- | Conway game of life in haskell using white cubes to represent life with ability to
-- move about world.
import Graphics.UI.GLUT
import Data.IORef
import Bindings
import Grid
import Life
import Data.Time.Clock
-- ==========================================================================================
main:: IO()
main = do
  (programName,args) <- getArgsAndInitialize
  initialDisplayMode $= [DoubleBuffered,WithDepthBuffer]
  initialWindowSize $= Size 900 900
  window <- createWindow "Conway's Game Of Life"

  -- Key states, will be zero when no key is being pressed.
  moveFB <- newIORef 0.0
  moveLR <- newIORef 0.0
  moveUD <- newIORef 0.0
  --Point representing camera positon.
  camPos <- newIORef (Vertex3 (-30) 80 (-30))
  --Vector representing camera direction.
  camDir <- newIORef (Vector3 3 (-3) 3)

  --Our callback functions.
  reshapeCallback $= Just reshape
  --Comparison function for depth of buffer.
  depthFunc $= Just Less
  --Time variable used to see if matrix should be updated or not.
  t <- getCurrentTime --time as well as grid must be taken out of IO monad.
  prevTime <- newIORef t

  --Light Source
--  lighting $= Enabled
--  position (Light 0) $= Vertex4 1 0.4 0.8 1
--  light (Light 0) $= Enabled
  grid <- getInitGrid gridSize
  myGrid <- newIORef $ grid

  globalKeyRepeat $= GlobalKeyRepeatOff
  keyboardMouseCallback $= Just (keyboardMouse moveFB moveLR moveUD)
  passiveMotionCallback $= Just (mouseMovement camDir)
  displayCallback $= display moveFB moveLR moveUD camPos camDir myGrid prevTime
  idleCallback $= Just (display moveFB moveLR moveUD camPos camDir myGrid prevTime)
  mainLoop
-- ==========================================================================================
