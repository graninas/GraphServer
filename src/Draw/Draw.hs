module Draw.Draw where

import qualified Graphics.Rendering.OpenGL as GL

import Language.Haskell.Syntax
import Structure.StructureObject
import Structure.SOConstruct
import Draw.Render
import Draw.TextureInit
import Common.Units
import Common.GLTypes
import Common.TestData


draw :: DrawFunction
draw ress@(GLResources texRes) n = do
    putStr $ "Current n = " ++ show n
    GL.clear [GL.ColorBuffer, GL.DepthBuffer]
    GL.loadIdentity
    GL.rotate 10 (vector3 0 1 0)
    GL.rotate 20 (vector3 1 0 0)
    GL.translate (vector3 (-5) (-10) (-30))
    --test ress



    --let c = constructFramedGRhss (OcsGuardedRhss t6)
    let c = constructMatch (OcsMatch t7)
    --let c = constructFramedGRhs (constructGuardedRhs (OcsGuardedRhs t4))
    putStrLn . show $ c
    render texRes c
    putStrLn " Ok."
    
    
    
    