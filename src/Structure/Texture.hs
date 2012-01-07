module Structure.Texture where

import qualified Graphics.Rendering.OpenGL as GL
import Common.GLTypes
import Common.Units

data QuadColorSpec = QuadTexture (QuadSide, TextureName)
                   | QuadPlainColor GLfColor4
                   | NoQuadColorSpec
    deriving (Show)

data ObjectTextureSpec = BoxTextureSpec
        { quadSideTexes  :: [(BoxSide, QuadColorSpec)]
        , defQuadSideTex :: QuadColorSpec
        } deriving (Show)
