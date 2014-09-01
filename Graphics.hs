-- | Simple OpenGL general functions for callbacks and keypresses. Cabal package: nehe-tuts
-- | and tutorial: http://nehe.gamedev.net/tutorial/
module Graphics where
import qualified Graphics.UI.GLFW as GLFW
-- everything from here starts with gl or GL
import Graphics.Rendering.OpenGL.Raw
import Graphics.Rendering.GLU.Raw ( gluPerspective )
import Data.Bits ( (.|.) )
import System.Exit ( exitWith, ExitCode(..) )

initGL :: GLFW.Window -> IO ()
initGL win = do
  glShadeModel gl_SMOOTH -- enables smooth color shading
  glClearColor 0 0 0 0 -- Clear the background color to black
  glClearDepth 1 -- enables clearing of the depth buffer
  glEnable gl_DEPTH_TEST
  glDepthFunc gl_LEQUAL  -- type of depth test
  glHint gl_PERSPECTIVE_CORRECTION_HINT gl_NICEST
  (w,h) <- GLFW.getFramebufferSize win
  resizeScene win w h

resizeScene :: GLFW.FramebufferSizeCallback
resizeScene win w     0      = resizeScene win w 1 -- prevent divide by zero
resizeScene _   width height = do
  glViewport 0 0 (fromIntegral width) (fromIntegral height)
  glMatrixMode gl_PROJECTION
  glLoadIdentity
  gluPerspective 45 (fromIntegral width/fromIntegral height) 0.1 100
  glMatrixMode gl_MODELVIEW
  glLoadIdentity
  glFlush

shutdown :: GLFW.WindowCloseCallback
shutdown win = do
  GLFW.destroyWindow win
  GLFW.terminate
  _ <- exitWith ExitSuccess
  return ()

keyPressed :: GLFW.KeyCallback
keyPressed win GLFW.Key'Escape _ GLFW.KeyState'Pressed _ = shutdown win
keyPressed _   _               _ _                     _ = return ()

-- | Sets up variables for GLFW functions and OpenGL calls for window.
--doGLFW :: IO window
doGLFW f = do
  True <- GLFW.init
  -- select type of display mode:
  -- Double buffer
  -- RGBA color
  -- Alpha components supported
  -- Depth buffer
  GLFW.defaultWindowHints
  -- open a window
  Just win <- GLFW.createWindow 1000 1000 "Game Of Life:: Hakell" Nothing Nothing
  GLFW.makeContextCurrent (Just win)
  -- register the function to do all our OpenGL drawing
  GLFW.setWindowRefreshCallback win (Just f)
  -- register the funciton called when our window is resized
  GLFW.setFramebufferSizeCallback win (Just resizeScene)
  -- register the function called when the keyboard is pressed.
  GLFW.setKeyCallback win (Just keyPressed)
  GLFW.setWindowCloseCallback win (Just shutdown)
  -- initialize our window.
  initGL win
  return win

-- | Draws Simple 1x1 white square.
drawSquare :: IO()
drawSquare = do
  glBegin gl_QUADS
  glVertex3f (-0.5) 0.5 0 -- top left
  glVertex3f  0.5 0.5 0 -- top right
  glVertex3f  0.5 (-0.5) 0 -- bottom right
  glVertex3f (-0.5) (-0.5) 0 -- bottom left
  glEnd
