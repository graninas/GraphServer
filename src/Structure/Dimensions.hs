module Structure.Dimensions where

import qualified Graphics.Rendering.OpenGL as GL
import Common.GLTypes
import Common.Units

data DerivedDimensions = FuncDimensions (GLfVector3 -> GLfVector3)

derivedDimensions :: DerivedDimensions -> GLfVector3 -> GLfVector3
derivedDimensions (FuncDimensions f) dims = f dims

funcBoxDerivedDims :: GLfVector3 -> GLfVector3 -> GLfVector3
funcBoxDerivedDims (GL.Vector3 opl oph opw) (GL.Vector3 fBoxl fBoxh fBoxw)
    = (GL.Vector3 (f opl fBoxl) (f oph fBoxh) (f opw fBoxw))
  where
    f a b | b >= (a + 1) = b
          | b < a        = a + 1
          | otherwise    = a + 1


generalizedDimension :: Geometries -> Dimension
generalizedDimension (g:gs) = toDimension (foldr f g gs)
  where
    f ((GL.Vector3  x1  y1  z1), (GL.Vector3  x2  y2  z2))
      ((GL.Vector3 ax1 ay1 az1), (GL.Vector3 ax2 ay2 az2)) =
             (vector3 (min x1 ax1) (min y1 ay1) (min z1 az1),
              vector3 (max x2 ax2) (max y2 ay2) (max z2 az2))
    toDimension ((GL.Vector3  x1  y1  z1), (GL.Vector3  x2  y2  z2)) =
        vector3 (abs (x2-x1)) (abs (y2 - y1)) (abs (z2 - z1))