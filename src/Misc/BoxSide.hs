module Misc.BoxSide where

import qualified Graphics.Rendering.OpenGL as GL
import Common.GLTypes
import Common.Units

boxSideList :: [BoxSide]
boxSideList = [SideTop, SideBottom, SideLeft, SideRight, SideRear, SideFront]

boxSide :: GLfVertex3 -> BoxSide -> IO ()

boxSide (GL.Vertex3 x y z) SideTop = do
            GL.texCoord texCoordUR >> GL.vertex (vertex3 x y z)
            GL.texCoord texCoordDR >> GL.vertex (vertex3 x y 0)
            GL.texCoord texCoordDL >> GL.vertex (vertex3 0 y 0)
            GL.texCoord texCoordUL >> GL.vertex (vertex3 0 y z)

boxSide (GL.Vertex3 x y z) SideFront = do
            GL.texCoord texCoordUR >> GL.vertex (vertex3 x y z)
            GL.texCoord texCoordDR >> GL.vertex (vertex3 0 y z)
            GL.texCoord texCoordDL >> GL.vertex (vertex3 0 0 z)
            GL.texCoord texCoordUL >> GL.vertex (vertex3 x 0 z)

boxSide (GL.Vertex3 x y z) SideRight = do
            GL.texCoord texCoordUR >> GL.vertex (vertex3 x y z)
            GL.texCoord texCoordUL >> GL.vertex (vertex3 x 0 z)
            GL.texCoord texCoordDL >> GL.vertex (vertex3 x 0 0)
            GL.texCoord texCoordDR >> GL.vertex (vertex3 x y 0)
            
boxSide (GL.Vertex3 x y z) SideRear = do
            GL.texCoord texCoordUR >> GL.vertex (vertex3 0 0 0)
            GL.texCoord texCoordDR >> GL.vertex (vertex3 0 y 0)
            GL.texCoord texCoordDL >> GL.vertex (vertex3 x y 0)
            GL.texCoord texCoordUL >> GL.vertex (vertex3 x 0 0)

boxSide (GL.Vertex3 x y z) SideLeft = do
            GL.texCoord texCoordUR >> GL.vertex (vertex3 0 0 0)
            GL.texCoord texCoordDR >> GL.vertex (vertex3 0 0 z)
            GL.texCoord texCoordDL >> GL.vertex (vertex3 0 y z)
            GL.texCoord texCoordUL >> GL.vertex (vertex3 0 y 0)

boxSide (GL.Vertex3 x y z) SideBottom = do
            GL.texCoord texCoordUR >> GL.vertex (vertex3 0 0 0)
            GL.texCoord texCoordDR >> GL.vertex (vertex3 x 0 0)
            GL.texCoord texCoordDL >> GL.vertex (vertex3 x 0 z)
            GL.texCoord texCoordUL >> GL.vertex (vertex3 0 0 z)

boxSides    boxDim = mapM_ (boxSide boxDim)
allBoxSides boxDim = boxSides boxDim boxSideList
