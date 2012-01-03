module Draw.GOCompile where

import Structure.GraphObject
import Structure.PrimitiveObject
import qualified Graphics.Rendering.OpenGL as GL
import Common.Units

compilePrimitiveObject texRes (PrimitiveBox (GL.Vertex3 x y z)) texName = 
    (GL.Quads, do
        GL.textureBinding GL.Texture2D GL.$= lookup texName texRes
        GL.color colorWhite
        GL.texCoord texCoordUR >> GL.vertex (vertex3 x y z)
        GL.texCoord texCoordDR >> GL.vertex (vertex3 0 y z)
        GL.texCoord texCoordDL >> GL.vertex (vertex3 0 0 z)
        GL.texCoord texCoordUL >> GL.vertex (vertex3 x 0 z)
        
        GL.texCoord texCoordUR >> GL.vertex (vertex3 x y z)
        GL.texCoord texCoordDR >> GL.vertex (vertex3 x y 0)
        GL.texCoord texCoordDL >> GL.vertex (vertex3 x 0 0)
        GL.texCoord texCoordUL >> GL.vertex (vertex3 x 0 z)
        
        GL.texCoord texCoordUR >> GL.vertex (vertex3 x y z)
        GL.texCoord texCoordDR >> GL.vertex (vertex3 x y 0)
        GL.texCoord texCoordDL >> GL.vertex (vertex3 0 y 0)
        GL.texCoord texCoordUL >> GL.vertex (vertex3 0 y z)
        
        GL.texCoord texCoordUR >> GL.vertex (vertex3 0 0 0)
        GL.texCoord texCoordDR >> GL.vertex (vertex3 0 y 0)
        GL.texCoord texCoordDL >> GL.vertex (vertex3 x y 0)
        GL.texCoord texCoordUL >> GL.vertex (vertex3 x 0 0)
        
        GL.texCoord texCoordUR >> GL.vertex (vertex3 0 0 0)
        GL.texCoord texCoordDR >> GL.vertex (vertex3 0 0 z)
        GL.texCoord texCoordDL >> GL.vertex (vertex3 0 y z)
        GL.texCoord texCoordUL >> GL.vertex (vertex3 0 y 0)
        
        GL.texCoord texCoordUR >> GL.vertex (vertex3 0 0 0)
        GL.texCoord texCoordDR >> GL.vertex (vertex3 x 0 0)
        GL.texCoord texCoordDL >> GL.vertex (vertex3 x 0 z)
        GL.texCoord texCoordUL >> GL.vertex (vertex3 0 0 z)
    )

compileGraphObject texRes (GO primitiveObj texName) = Just $ compilePrimitiveObject texRes primitiveObj texName
compileGraphObject _ NoGraphObject = Nothing

