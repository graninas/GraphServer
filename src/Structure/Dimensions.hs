module Structure.Dimensions where

import qualified Graphics.Rendering.OpenGL as GL
import Common.GLTypes
import Common.Units

data DerivedDimensions = FuncDimensions (GLfVector3 -> GLfVector3)
                       | FoundationDimensions

derivedDimensions :: DerivedDimensions -> GLfVector3 -> GLfVector3
derivedDimensions (FuncDimensions f) dim = f dim
derivedDimensions  FoundationDimensions (GL.Vector3 l h w) = vector3 (l + 2) 0.25 (w + 2)

   
-- Functions to place into DerivedDimensions
-- | Calculates function box dimensions according to it's argument dims 
funcBoxDerivedDims :: GLfVector3 -> GLfVector3 -> GLfVector3
funcBoxDerivedDims (GL.Vector3 opl oph opw) (GL.Vector3 fBoxl fBoxh fBoxw) =
    (GL.Vector3 (f opl fBoxh) fBoxh (max opw fBoxw))
  where
    f  op box | op >= box       = op + 1
              | (box - op) <  1 = op + 1
              | (box - op) >= 1 = box

-- | Calculates dims for variable box
variableBoxDims :: GLfVector3 -> GLfVector3
variableBoxDims (GL.Vector3 varl varh varw) =
    (GL.Vector3 (if varl < 2 then 2 else varl) varh varw)

-- | Calculates general dimensions of the object group
generalizedDimension :: Geometries -> Dimension
generalizedDimension (g:gs) = toDimension (foldr f g gs)
  where
    f ((GL.Vector3 dx1 dy1 dz1), (GL.Vector3 ax1 ay1 az1))
      ((GL.Vector3 dx2 dy2 dz2), (GL.Vector3 ax2 ay2 az2)) =
             (vector3 (min dx1 dx2) (min dy1 dy2) (min dz1 dz2),
              vector3 (max (dx1 + ax1) (dx2 + ax2))
                      (max (dy1 + ay1) (dy2 + ay2))
                      (max (dz1 + az1) (dz2 + az2)))
    toDimension ((GL.Vector3  x1  y1  z1), (GL.Vector3  x2  y2  z2)) =
        vector3 (abs (x2-x1)) (abs (y2 - y1)) (abs (z2 - z1))