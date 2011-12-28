module Draw.Render where

import qualified Graphics.Rendering.OpenGL as GL

import Structure.StructureObject
import Draw.GOCompile
import Units




render (StructObj dif _ go) = do
    let (po, compiled) = compileGraphObject $ go
    GL.translate dif
    GL.renderPrimitive po compiled 


render (Construction _ _ []) = return ()
render (Construction dif _ os) = do
    GL.translate dif
    mapM_ render os