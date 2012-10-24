module Structure.Texture where

import qualified Graphics.Rendering.OpenGL as GL
import Common.GLTypes
import Common.Units

data QuadColorSpec = QuadTexture TextureName
                   | QuadPlainColor GLfColor4
                   | NoQuadColorSpec
    deriving (Show)

data ObjectTextureSpec = BoxTextureSpec
        { quadSideTexes  :: [(BoxSide, QuadColorSpec)]
        , defQuadSideTex :: QuadColorSpec
        } deriving (Show)

data CornerCoord = UR | DR | DL | UL 
    deriving (Show)

-- | Texture coordinates for quad.
-- Default direction for texture (when there is no texture rotating on the quad)
-- is right (QuadRight).
texCoordUR, texCoordDR, texCoordDL, texCoordUL :: GLfTexCoord2
texCoordUR = texCoord2 1 1
texCoordDR = texCoord2 1 0
texCoordDL = texCoord2 0 0
texCoordUL = texCoord2 0 1

-- | Texture coordinates for quad corner.
cornerTexCoord :: CornerCoord -> GLfTexCoord2
cornerTexCoord UR = texCoordUR
cornerTexCoord DR = texCoordDR
cornerTexCoord DL = texCoordDL
cornerTexCoord UL = texCoordUL
