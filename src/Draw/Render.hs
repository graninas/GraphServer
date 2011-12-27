module Draw.Render where

import qualified Graphics.Rendering.OpenGL as GL

import Structure.StructureObject
import Draw.GOCompile
import Units




render (StructObj dif dim go) = do
    let (po, compiled) = compileGraphObject $ go
    GL.translate dif
    GL.renderPrimitive po compiled 


render (ConstructObj _ _ []) = return ()
render (ConstructObj dif dim os) = do
    GL.translate dif
    mapM_ render os