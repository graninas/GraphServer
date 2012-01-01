module Draw.Draw where

import qualified Graphics.Rendering.OpenGL as GL

import Language.Haskell.Syntax
import Structure.StructureObject
import Draw.Colors
import Draw.Render
import Draw.Texture
import Units
import GLTypes

testData (GLResources ((texName, texObject):texs)) = do
    GL.textureBinding GL.Texture2D GL.$= Just texObject
    --GL.color (colors !! 0)
    texCoord2 0 1
    GL.vertex $ vertex3 0 0 0
    --GL.color (colors !! 1)
    texCoord2 0 0
    GL.vertex $ vertex3 1 1 1
    --GL.color (colors !! 2)
    texCoord2 1 0
    GL.vertex $ vertex3 (-1) (-1) (-1)
    --GL.color (colors !! 3)
    texCoord2 1 1
    GL.vertex $ vertex3 0 1 0
    --GL.color (colors !! 4)
    texCoord2 0 1
    GL.vertex $ vertex3 1 0 1
    --GL.color (colors !! 5)
    texCoord2 0 0
    GL.vertex $ vertex3 (-1) 1 (-1)
    --GL.color (colors !! 6)
    texCoord2 1 0
    GL.vertex $ vertex3 1 0 (-1)
    --GL.color (colors !! 7)
    texCoord2 1 1
    GL.vertex $ vertex3 0 (-1) 1
    --GL.color (colors !! 8)
    texCoord2 0 1
    GL.vertex $ vertex3 1 1 0


draw :: DrawFunction
draw ress n = do
    putStr $ "Current n = " ++ show n
    GL.clear [GL.ColorBuffer, GL.DepthBuffer]
    GL.loadIdentity
    GL.rotate 10 (vector3 0 1 0)
    GL.rotate 15 (vector3 1 0 0)
    GL.translate (vector3 1 (-5) (-20))
    GL.renderPrimitive GL.TriangleStrip $ (testData ress)

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
    
    
    
    