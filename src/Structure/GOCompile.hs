module Structure.GOCompile
    (
        compileGraphObject
    ) where

import qualified Graphics.Rendering.OpenGL as GL
import Structure.GraphObject
import Structure.Texture
import Common.Units
import Common.GLTypes
import Misc.BoxSide

setQuadColorSpec :: PreparedTextureObjects -> QuadColorSpec -> IO ()
setQuadColorSpec _ NoQuadColorSpec = return ()
setQuadColorSpec texRes (QuadTexture (quadSide, texName)) =
    do  GL.color colorWhite
        GL.textureBinding GL.Texture2D GL.$= lookup texName texRes
setQuadColorSpec texRes (QuadPlainColor col) = GL.color col

f :: PreparedTextureObjects
    -> GLfVertex3 
    -> (BoxSide, QuadColorSpec)
    -> ([BoxSide], [IO()])
    -> ([BoxSide], [IO()])
f texRes boxDim (side, qColorSpec) (sList, ioList) = let
    boxIO = do  setQuadColorSpec texRes qColorSpec
                boxSide boxDim side
    in (side : sList, boxIO : ioList) 
     


compileGraphObject texRes (PrimitiveBox boxDim texName) =
    [do GL.color colorWhite
        GL.textureBinding GL.Texture2D GL.$= lookup texName texRes
        GL.renderPrimitive GL.Quads (allBoxSides boxDim)]

compileGraphObject texRes (TexturedBox boxDim boxTexSpec) = let
    (BoxTextureSpec sideTexes defTex) = boxTexSpec
    (textedSides, textedSideDrawList) = foldr (f texRes boxDim) ([], []) sideTexes
    untextedSides = [s | s <- boxSideList, s `notElem` textedSides]
    untextedQColor = setQuadColorSpec texRes defTex
    untextedSidesDraw = boxSides boxDim untextedSides
    in untextedQColor : untextedSidesDraw : textedSideDrawList

compileGraphObject _ NoGraphObject = []

{-
data QuadColorSpec = QuadTexture (QuadSide, TextureName)
                   | QuadPlainColor GLfColor4
    deriving (Show)

data ObjectTextureSpec = BoxTextureSpec
        {
            quadSideTexes  :: [(BoxSide, QuadColorSpec)]
          , defQuadSideTex :: Maybe QuadColorSpec
        }
    deriving (Show)
    -}