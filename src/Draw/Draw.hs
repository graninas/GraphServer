module Draw.Draw where

import qualified Graphics.Rendering.OpenGL as GL

import Language.Haskell.Syntax
import Structure.StructureObject
import Structure.SOConstruct
import Draw.Colors
import Draw.Render
import Draw.Texture
import Common.Units
import Common.GLTypes
import Common.Constants
import Common.TestData


drawVertex (VertexSpec col tCoord vert) = do
    GL.color    col
    GL.texCoord tCoord
    GL.vertex   vert

drawVertexes texts (textureName, mode, vertexes) = do
      GL.textureBinding GL.Texture2D GL.$= lookup textureName texts
      GL.renderPrimitive mode $ (mapM_ drawVertex vertexes)

test (GLResources texRes) = mapM_ (drawVertexes texRes) testData

draw :: DrawFunction
draw ress@(GLResources texRes) n = do
    putStr $ "Current n = " ++ show n
    GL.clear [GL.ColorBuffer, GL.DepthBuffer]
    GL.loadIdentity
    GL.rotate 10 (vector3 0 1 0)
    GL.rotate 15 (vector3 1 0 0)
    GL.translate (vector3 (-5) (-5) (-30))
    --test ress



    let c = constructGuardedRhs (OcsGuardedRhs t4)
    --let c = constructExp (OcsExpArgument t5)
    putStrLn . show $ c
    render texRes c
    putStrLn " Ok."
    
    
    
    