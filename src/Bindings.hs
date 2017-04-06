-- | Holds general functions for gluing GLUT callbacks together.
module Bindings (display,reshape,keyboardMouse,mouseMovement) where
import Graphics.UI.GLUT
import System.Exit
import Display
import Types
-- ==========================================================================================
-- | Reshape function for when window needs to be resized.
-- | Adjust viewport, and persperctive for thr Projection matrix.
reshape :: ReshapeCallback
reshape (Size w h) = do
  --Work in projection matrix.
  matrixMode $= Projection
  loadIdentity
  --Set viewport to cover whole window.
  viewport $=  (Position 0 0, (Size w h))

  let myRatio = (fromIntegral w) / (fromIntegral h)
  perspective 45.0 myRatio 0.1 300
  matrixMode $= Modelview 0
-- ==========================================================================================
-- | Callback function for keyboard and mouse events.
keyboardMouse:: DoubleIO -> DoubleIO -> DoubleIO -> KeyboardMouseCallback
keyboardMouse  moveFB moveLR moveUD key Down _ _ = case key of
  (Char 'w') -> moveFB $= 8.0
  (Char 's') -> moveFB $= -8.0
  (Char 'a') -> moveLR $= -8.0
  (Char 'd') -> moveLR $= 8.0
  (Char ' ') -> moveUD $= 8.0
  (SpecialKey KeyShiftL) -> moveUD $= -8.0
  _ -> return ()
keyboardMouse moveFB moveLR moveUD key Up _ _ = case key of
  (Char 'w') -> moveFB $= 0.0
  (Char 's') -> moveFB $= 0.0
  (Char 'a') -> moveLR $= 0.0
  (Char 'd') -> moveLR $= 0.0
  (Char ' ') -> moveUD $= 0.0
  (SpecialKey KeyShiftL) -> moveUD $= 0
  (Char '\ESC') -> exitSuccess
  _ -> return ()
-- ==========================================================================================
-- | Callback function for passive mouse movements, called everytime the user moves
-- | the mouse around and camera should be updated appropriately. Takes in:
mouseMovement:: VectorIO -> MotionCallback
mouseMovement camDir  (Position xNew yNew) = do
  let xOffset = fromIntegral xNew * 0.01
      --Stretch period of sin and offset to right degrees :P
      yOffset = (fromIntegral yNew * 0.005) + 400
  camDir $= (Vector3 (sin xOffset) ((-sin yOffset)) ((-cos xOffset)))
-- ==========================================================================================
