-- | Shape actions of Glut built in objects.

module Shapes where
import Graphics.UI.GLUT

vertex3f:: (GLfloat,GLfloat,GLfloat) -> IO()
vertex3f (x,y,z) = vertex $ Vertex3 x y z

sphereSolid:: Radius -> Slices -> Stacks -> IO()
sphereSolid r s1 s2  = renderObject Solid $ Sphere' r s1 s2

sphereFrame:: Radius -> Slices -> Stacks -> IO()
sphereFrame r s1 s2  = renderObject Wireframe $ Sphere' r s1 s2

coneSolid:: Radius -> Height -> Slices -> Stacks -> IO()
coneSolid r h slices stacks = renderObject Solid $ Cone r h slices stacks

cubeSolid :: Height -> IO()
cubeSolid h = renderObject Solid $ Cube h

cubeFrame :: Height -> IO()
cubeFrame h = renderObject Wireframe $ Cube h
