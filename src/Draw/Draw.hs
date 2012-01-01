module Draw.Draw where

import qualified Graphics.Rendering.OpenGL as GL

import Language.Haskell.Syntax
import Structure.StructureObject
import Draw.Colors
import Draw.Render
import Draw.Texture
import Units
import GLTypes

data TestData = TestData GLfColor4 GLfTexCoord2 GLfVertex3

testData1 :: [TestData]
testData1 = [
    TestData (color4 1 0 0 0) (texCoord2 0 1) (vertex3 0 3 0),
    TestData (color4 0 1 0 0) (texCoord2 0 0) (vertex3 0 0 0),
    TestData (color4 0 0 1 0) (texCoord2 1 0) (vertex3 3 0 0),
    TestData (color4 1 1 0 0) (texCoord2 1 1) (vertex3 3 3 0)
    ]

drawVertex (TestData col tCoord vert) = do
    GL.color    col
    GL.texCoord tCoord
    GL.vertex   vert

testData (GLResources ((texName, texObject):_)) = do
    GL.textureBinding GL.Texture2D GL.$= Just texObject
    mapM_ drawVertex testData1


draw :: DrawFunction
draw ress n = do
    putStr $ "Current n = " ++ show n
    GL.clear [GL.ColorBuffer, GL.DepthBuffer]
    GL.loadIdentity
    GL.rotate 10 (vector3 0 1 0)
    GL.rotate 15 (vector3 1 0 0)
    GL.translate (vector3 1 (-5) (-20))
    GL.renderPrimitive GL.Quads $ (testData ress)

    let t1 = HsInfixApp
                (HsVar (UnQual (HsIdent "n")))
                (HsQVarOp (UnQual (HsSymbol "-")))
                (HsLit (HsInt 1))
    let t2 = HsApp
                (HsVar (UnQual (HsIdent "fact'")))
                (HsParen
                    (HsInfixApp
                        (HsVar (UnQual (HsIdent "n")))
                        (HsQVarOp (UnQual (HsSymbol "-")))
                        (HsLit (HsInt 1))))


    let c = constructInfixApp t1

    --render c
    putStrLn " Ok." 
    
    
    
    