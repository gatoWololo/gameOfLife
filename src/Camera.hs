-- | Holds functions related to camera movements of programs.
module Camera (updateCamera) where

import Graphics.UI.GLUT
import Types
-- ==========================================================================================
updateCamera:: DoubleIO -> DoubleIO -> DoubleIO -> VertexIO -> VectorIO  -> IO()
updateCamera moveFB moveLR moveUD camPos camDir = do
  moveFB' <- get moveFB
  moveLR' <- get moveLR
  moveUD' <- get moveUD

  if moveFB' /= 0.0 then updateMoveFB moveFB' camDir camPos else return ()
  if moveLR' /= 0.0 then updateMoveLR moveLR' camDir camPos else return ()
  if moveUD' /= 0.0 then updateMoveUD moveUD' camPos else return ()

  --Pattern match our vectors for use.
  (Vertex3 xPos yPos zPos) <- get camPos
  (Vector3 xDir yDir zDir) <- get camDir

  --Update our camera based on new changes.
  lookAt (Vertex3 xPos yPos zPos) (Vertex3 (xPos+xDir) (yPos+yDir) (zPos+zDir)) (Vector3 0 1 0)
-- ==========================================================================================
updateMoveFB:: GLdouble -> VectorIO -> VertexIO -> IO()
updateMoveFB move camDir camPos = do
  (Vector3 xDir _ zDir) <- get camDir
  (Vertex3 xPos yPos zPos) <- get camPos
  let x' = xPos + move*xDir*0.1
      z' = zPos + move*zDir*0.1
  camPos $= (Vertex3 x' yPos z')
-- ==========================================================================================
updateMoveLR:: GLdouble -> VectorIO -> VertexIO -> IO()
updateMoveLR move camDir camPos = do
  (Vector3 xDir _ zDir) <- get camDir
  (Vertex3 xPos yPos zPos) <- get camPos
  --Calculate cross prodcut for correct left right strafling.
  let x' = xPos + move*zDir*(-0.1)
      z' = zPos + move*xDir*0.1
  camPos $= (Vertex3 x' yPos z')
-- ==========================================================================================
updateMoveUD:: GLdouble -> VertexIO -> IO()
updateMoveUD move camPos = do
  (Vertex3 xPos yPos zPos) <- get camPos
  camPos $= (Vertex3 xPos (yPos+move*0.1) zPos)
-- ==========================================================================================
