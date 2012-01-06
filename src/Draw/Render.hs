module Draw.Render where

import qualified Graphics.Rendering.OpenGL as GL

import Structure.StructureObject
import Structure.GOCompile
import Common.Units
import Common.GLTypes


renderGraphObject texRes (goTrans, _, go) = do
    GL.translate goTrans
    let compiled = compileGraphObject texRes go
    sequence_ compiled
    GL.translate . negateVector3 $ goTrans

render texRes (StructureObject _ (soTrans, _) goSpec objects) = do
    GL.translate soTrans
    mapM_ (render texRes) objects
    renderGraphObject texRes goSpec
    GL.translate . negateVector3 $ soTrans
