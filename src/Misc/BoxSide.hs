module Misc.BoxSide where

import qualified Graphics.Rendering.OpenGL as GL
import Data.List (cycle) 
import Common.GLTypes
import Common.Units

boxSideList :: [BoxSide]
boxSideList = [SideTop, SideBottom, SideLeft, SideRight, SideRear, SideFront]

quadTexRotating QuadUp    = 0
quadTexRotating QuadLeft  = 1
quadTexRotating QuadDown  = 2
quadTexRotating QuadRight = 3 




boxSide :: GLfVertex3 -> BoxSide -> IO ()

boxSide (GL.Vertex3 x y z) SideTop = do
            GL.texCoord texCoordDR >> GL.vertex (vertex3 x y z)
            GL.texCoord texCoordUR >> GL.vertex (vertex3 x y 0)
            GL.texCoord texCoordUL >> GL.vertex (vertex3 0 y 0)
            GL.texCoord texCoordDL >> GL.vertex (vertex3 0 y z)

boxSide (GL.Vertex3 x y z) SideFront = do
            GL.texCoord texCoordUR >> GL.vertex (vertex3 x y z)
            GL.texCoord texCoordUL >> GL.vertex (vertex3 0 y z)
            GL.texCoord texCoordDL >> GL.vertex (vertex3 0 0 z)
            GL.texCoord texCoordDR >> GL.vertex (vertex3 x 0 z)

boxSide (GL.Vertex3 x y z) SideRight = do
            GL.texCoord texCoordUL >> GL.vertex (vertex3 x y z)
            GL.texCoord texCoordDL >> GL.vertex (vertex3 x 0 z)
            GL.texCoord texCoordDR >> GL.vertex (vertex3 x 0 0)
            GL.texCoord texCoordUR >> GL.vertex (vertex3 x y 0)
            
boxSide (GL.Vertex3 x y z) SideRear = do
            GL.texCoord texCoordDR >> GL.vertex (vertex3 0 0 0)
            GL.texCoord texCoordUR >> GL.vertex (vertex3 0 y 0)
            GL.texCoord texCoordUL >> GL.vertex (vertex3 x y 0)
            GL.texCoord texCoordDL >> GL.vertex (vertex3 x 0 0)

boxSide (GL.Vertex3 x y z) SideLeft = do
            GL.texCoord texCoordDL >> GL.vertex (vertex3 0 0 0)
            GL.texCoord texCoordDR >> GL.vertex (vertex3 0 0 z)
            GL.texCoord texCoordUR >> GL.vertex (vertex3 0 y z)
            GL.texCoord texCoordUL >> GL.vertex (vertex3 0 y 0)

boxSide (GL.Vertex3 x y z) SideBottom = do
            GL.texCoord texCoordDL >> GL.vertex (vertex3 0 0 0)
            GL.texCoord texCoordDR >> GL.vertex (vertex3 x 0 0)
            GL.texCoord texCoordUR >> GL.vertex (vertex3 x 0 z)
            GL.texCoord texCoordUL >> GL.vertex (vertex3 0 0 z)

boxSides    boxDim = mapM_ (boxSide boxDim)
allBoxSides boxDim = boxSides boxDim boxSideList
