module Draw.Render where

import qualified Graphics.Rendering.OpenGL as GL

import Structure.StructureObject
import Draw.GOCompile
import Common.Units
import Common.GLTypes



render texRes (StructureObject dif _ go objects) = do
    GL.translate dif
    case compileGraphObject texRes go of
        Just (po, compiled) -> GL.renderPrimitive po compiled
        Nothing -> return ()
    mapM_ (render texRes) objects
    GL.translate (negateVector3 dif)
