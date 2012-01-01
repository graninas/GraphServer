module Draw.GOCompile where

import Structure.GraphObject
import Structure.PrimitiveObject
import qualified Graphics.Rendering.OpenGL as GL
import Common.Units

compilePrimitiveObject (PrimitiveBox (GL.Vertex3 x y z)) = 
    (GL.Quads, do
        GL.color (color3 1 0 1)
        GL.vertex (vertex3 x y z)
        GL.vertex (vertex3 0 y z)
        GL.vertex (vertex3 0 0 z)
        GL.vertex (vertex3 x 0 z)
        GL.color (color3 0 1 1)
        GL.vertex (vertex3 x y z)
        GL.vertex (vertex3 x y 0)
        GL.vertex (vertex3 x 0 0)
        GL.vertex (vertex3 x 0 z)
        GL.color (color3 0 0 1)
        GL.vertex (vertex3 x y z)
        GL.vertex (vertex3 x y 0)
        GL.vertex (vertex3 0 y 0)
        GL.vertex (vertex3 0 y z)
        GL.color (color3 1 0 0)
        GL.vertex (vertex3 0 0 0)
        GL.vertex (vertex3 0 y 0)
        GL.vertex (vertex3 x y 0)
        GL.vertex (vertex3 x 0 0)
        GL.color (color3 1 1 0)
        GL.vertex (vertex3 0 0 0)
        GL.vertex (vertex3 0 0 z)
        GL.vertex (vertex3 0 y z)
        GL.vertex (vertex3 0 y 0)
        GL.color (color3 1 0 0)
        GL.vertex (vertex3 0 0 0)
        GL.vertex (vertex3 x 0 0)
        GL.vertex (vertex3 x 0 z)
        GL.vertex (vertex3 0 0 z)
    )

compileGraphObject (GO primitiveObj) = compilePrimitiveObject primitiveObj

