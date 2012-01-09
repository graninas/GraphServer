module Draw.Render where

import qualified Graphics.Rendering.OpenGL as GL

import Structure.StructureObject
import Structure.GOCompile
import Common.Units
import Common.GLTypes


render texRes (StructureObject _ (soTrans, _) goSpec objects) = do
    GL.translate soTrans
    mapM_ (render texRes) objects
    sequence_ $ compileGraphObjectSpec texRes goSpec
    GL.translate . negateVector3 $ soTrans
