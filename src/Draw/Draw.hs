module Draw.Draw where

import qualified Graphics.Rendering.OpenGL as GL

import Language.Haskell.Syntax
import Structure.StructureObject
import Draw.Colors
import Draw.Render
import Draw.Texture
import Common.Units
import Common.GLTypes
import Common.Constants

data VertexSpec = VertexSpec GLfColor4 GLfTexCoord2 GLfVertex3
type ObjSpec = [(TextureName, GL.PrimitiveMode, [VertexSpec])]

testData = [
    (tex1, GL.Quads,
        [VertexSpec colorWhite (texCoord2 0 1) (vertex3 0 3 0),
         VertexSpec colorWhite (texCoord2 0 0) (vertex3 0 0 0),
         VertexSpec colorWhite (texCoord2 1 0) (vertex3 3 0 0),
         VertexSpec colorWhite (texCoord2 1 1) (vertex3 3 3 0)]),
    
    (tex2, GL.Quads,
        [VertexSpec colorWhite (texCoord2 0 1) (vertex3 3 6 0),
         VertexSpec colorWhite (texCoord2 0 0) (vertex3 3 3 0),
         VertexSpec colorWhite (texCoord2 1 0) (vertex3 6 3 0),
         VertexSpec colorWhite (texCoord2 1 1) (vertex3 6 6 0)])
    ]

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
    GL.translate (vector3 1 (-5) (-20))
    --test ress

    let t1 = HsInfixApp
                (HsVar (UnQual (HsIdent "n")))
                (HsQVarOp (UnQual (HsSymbol "-")))
                (HsLit (HsInt 1))
    let t2 = HsApp
                (HsVar (UnQual (HsIdent "fact'")))
                (HsParen t1)

    let t3 = HsInfixApp
                (HsApp
                    (HsVar (UnQual (HsIdent "fact'")))
                    (HsParen
                        (HsInfixApp
                            (HsVar (UnQual (HsIdent "n")))
                            (HsQVarOp (UnQual (HsSymbol "-")))
                            (HsLit (HsInt 1)))))
                (HsQVarOp
                    (UnQual (HsSymbol "*")))
                    (HsVar (UnQual (HsIdent "n")))

    let c = constructExpr t2 NoObjectSpec NoDeriving

    render texRes c
    putStrLn " Ok." 
    
    
    
    