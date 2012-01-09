module Common.TestData where

import qualified Graphics.Rendering.OpenGL as GL
import Language.Haskell.Syntax
import Structure.StructureObject
import Structure.Texture
import Common.GLTypes
import Common.Units
import Common.Constants

data VertexSpec = VertexSpec GLfColor4 GLfTexCoord2 GLfVertex3
type ObjSpec = [(TextureName, GL.PrimitiveMode, [VertexSpec])]

testData = 
    [ (hazardStripeTex, GL.Quads,
       [ VertexSpec colorWhite (texCoord2 0 1) (vertex3 0 3 0)
       , VertexSpec colorWhite (texCoord2 0 0) (vertex3 0 0 0)
       , VertexSpec colorWhite (texCoord2 1 0) (vertex3 3 0 0)
       , VertexSpec colorWhite (texCoord2 1 1) (vertex3 3 3 0)
       ])
    , (helloTex, GL.Quads,
       [ VertexSpec colorWhite (texCoord2 0 1) (vertex3 3 6 0)
       , VertexSpec colorWhite (texCoord2 0 0) (vertex3 3 3 0)
       , VertexSpec colorWhite (texCoord2 1 0) (vertex3 6 3 0)
       , VertexSpec colorWhite (texCoord2 1 1) (vertex3 6 6 0)
       ])
    ]

drawVertex (VertexSpec col tCoord vert) = do
    GL.color    col
    GL.texCoord tCoord
    GL.vertex   vert

drawVertexes texts (textureName, mode, vertexes) = do
      GL.textureBinding GL.Texture2D GL.$= lookup textureName texts
      GL.renderPrimitive mode $ (mapM_ drawVertex vertexes)

test (GLResources texRes) = mapM_ (drawVertexes texRes) testData


t1 = HsInfixApp (HsVar (UnQual (HsIdent "n")))
                (HsQVarOp (UnQual (HsSymbol "-")))
                (HsLit (HsInt 1))

t2 = HsApp (HsVar (UnQual (HsIdent "fact'")))
           (HsParen t1)

t3 = HsInfixApp t2
                (HsQVarOp (UnQual (HsSymbol "*")))
                (HsVar (UnQual (HsIdent "n")))

t4 = HsGuardedRhs (SrcLoc {srcFilename = "<unknown>", srcLine = 7, srcColumn = 9})
                  (HsVar (UnQual (HsIdent "otherwise")))
                  t3

t5 = HsGuardedRhs (SrcLoc {srcFilename = "<unknown>", srcLine = 6, srcColumn = 9})
                  (HsInfixApp (HsVar (UnQual (HsIdent "n"))) (HsQVarOp (UnQual (HsSymbol "=="))) (HsLit (HsInt 0)))
                  (HsLit (HsInt 1))

t6 = HsGuardedRhss [t4, t5]