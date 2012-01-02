module Structure.Dimensions where

import qualified Graphics.Rendering.OpenGL as GL
import Common.GLTypes


dimensions :: DerivedDimensions -> GLfVector3 -> GLfVector3
dimensions NoDeriving   dims = dims
dimensions (FuncDims f) dims = f dims 


functionBoxRelativeDims :: GLfVector3 -> GLfVector3 -> GLfVector3
functionBoxRelativeDims (GL.Vector3 opl oph op) (GL.Vector3 fBoxl fBoxh fBoxw)
    = (GL.Vector3 newL fBoxh fBoxw)
  where
    newL | fBoxl >= (opl + 1) = fBoxl
         | fBoxl < opl        = opl + 1
         | otherwise       = opl + 1