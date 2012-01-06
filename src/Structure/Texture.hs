module Structure.Texture where

import Common.GLTypes

data QuadColorSpec = QuadTexture (QuadSide, TextureName)
                   | QuadPlainColor GLfColor4
                   | NoQuadColorSpec
    deriving (Show)

data ObjectTextureSpec = BoxTextureSpec
        {
            quadSideTexes  :: [(BoxSide, QuadColorSpec)]
          , defQuadSideTex :: QuadColorSpec
        }
    deriving (Show)


