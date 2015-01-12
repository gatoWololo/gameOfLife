module Types where

import Graphics.UI.GLUT
import Data.IORef
import Data.Array.Repa
import Data.Time.Clock

-- | Direction used for telling wheter camera moving or not.
data Direction = Positive | Negative | None
-- | List of types used throughout program.
type DoubleIO = IORef GLdouble
type VectorIO = IORef (Vector3 GLdouble)
type VertexIO = IORef (Vertex3 GLdouble)
type DirectionIO = IORef Direction
type PositionIO = IORef Position
type IntIO = IORef Int
type Array2dIO = IORef Array2D
type Array2D = Array U DIM2 Int
type Coordinate = (Int,Int)
type FloatCoord = (GLfloat,GLfloat)
type TimeIO = IORef UTCTime
