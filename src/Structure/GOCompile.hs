module Structure.GOCompile
    (
        compileGraphObject
    ) where

import qualified Graphics.Rendering.OpenGL as GL
import Structure.GraphObject
import Structure.Texture
import Common.Units
import Common.GLTypes
import Common.Constants
import Misc.BoxSide

-- | Sets texture or/and current color to draw next objects.
setQuadColorSpec :: PreparedTextureObjects -> QuadColorSpec -> IO ()
setQuadColorSpec _ NoQuadColorSpec = return ()
setQuadColorSpec texRes (QuadTexture (quadSide, texName)) =
    do  GL.color colorWhite
        GL.textureBinding GL.Texture2D GL.$= lookup texName texRes
setQuadColorSpec texRes (QuadPlainColor col) = do
    GL.textureBinding GL.Texture2D GL.$= Nothing
    GL.color col

-- | Collects actions for specified box side drawings.
-- | It should be used only in this module.
f :: PreparedTextureObjects
    -> GLfVertex3 
    -> (BoxSide, QuadColorSpec)
    -> ([BoxSide], [IO()])
    -> ([BoxSide], [IO()])
f texRes boxDim (side, qColorSpec) (sList, ioList) = let
    boxIO = do  setQuadColorSpec texRes qColorSpec
                GL.renderPrimitive GL.Quads (boxSide boxDim side)
    in (side : sList, boxIO : ioList) 

-- | Compiles GraphObject into action list structure, which is ready-to-eval. ([IO ()]) 
compileGraphObject texRes (PrimitiveBox boxDim texName) =
    [do GL.color colorWhite
        GL.textureBinding GL.Texture2D GL.$= lookup texName texRes
        GL.renderPrimitive GL.Quads (allBoxSides boxDim)]

compileGraphObject texRes (TexturedBox boxDim boxTexSpec) = let
    (BoxTextureSpec sideTexes defTex) = boxTexSpec
    (textedSides, textedSideDrawList) = foldr (f texRes boxDim) ([], []) sideTexes
    untextedSides = [s | s <- boxSideList, s `notElem` textedSides]
    untextedQColor = setQuadColorSpec texRes defTex
    untextedSidesDraw = GL.renderPrimitive GL.Quads (boxSides boxDim untextedSides)
    in untextedQColor : untextedSidesDraw : textedSideDrawList

compileGraphObject _ NoGraphObject = []
