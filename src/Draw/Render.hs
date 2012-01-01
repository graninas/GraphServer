module Draw.Render where

import qualified Graphics.Rendering.OpenGL as GL

import Structure.StructureObject
import Draw.GOCompile
import Common.Units
import Common.GLTypes




render texRes (StructObj dif _ go) = do
    let (po, compiled) = compileGraphObject texRes go
    GL.translate dif
    GL.renderPrimitive po compiled
    GL.translate (negateVector3 dif)


render _ (Construction _ _ []) = return ()
render texRes (Construction dif _ os) = do
    GL.translate dif
    mapM_ (render texRes) os
    GL.translate (negateVector3 dif)