module Draw.Draw where

import qualified Graphics.Rendering.OpenGL as GL

import Language.Haskell.Syntax
import Structure.GraphStructure
import Draw.Colors
import Units

testData = do
    GL.color (colors !! 0)
    GL.vertex $ vertex3 0 0 0
    GL.color (colors !! 1)
    GL.vertex $ vertex3 1 1 1
    GL.color (colors !! 2)
    GL.vertex $ vertex3 (-1) (-1) (-1)
    GL.color (colors !! 3)
    GL.vertex $ vertex3 0 1 0
    GL.color (colors !! 4)
    GL.vertex $ vertex3 1 0 1
    GL.color (colors !! 5)
    GL.vertex $ vertex3 (-1) 1 (-1)
    GL.color (colors !! 6)
    GL.vertex $ vertex3 1 0 (-1)
    GL.color (colors !! 7)
    GL.vertex $ vertex3 0 (-1) 1
    GL.color (colors !! 8)
    GL.vertex $ vertex3 1 1 0


draw :: GL.GLfloat -> IO ()
draw n = do
    putStr $ "Current n = " ++ show n
    GL.clear [GL.ColorBuffer, GL.DepthBuffer]
    GL.loadIdentity
    GL.rotate 10 (GL.Vector3 (0.0::GL.GLfloat) 1.0 0.0)
    GL.rotate 15 (GL.Vector3 (1.0::GL.GLfloat) 0.0 0.0)
    GL.translate (GL.Vector3 (1.0::GL.GLfloat) (-5.0) (-20.0))
    GL.renderPrimitive GL.TriangleStrip $ testData

    let t1 = HsInfixApp
                (HsVar (UnQual (HsIdent "n")))
                (HsQVarOp (UnQual (HsSymbol "-")))
                (HsLit (HsInt 1))


    let res = constructInfixApp nullVertex3 nullVertex3 t1



    putStrLn " Ok." 
    
    
    
    