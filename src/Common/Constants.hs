module Common.Constants where

import Common.GLTypes

applicationPath = "./"
texturesPath    = "./Textures"


tex1 = "Crate.tga"
tex2 = "Crate.tga"

rawTextureData :: RawTextures
rawTextureData = [(tex1, texturesPath ++ "/" ++ tex1),
                  (tex2, texturesPath ++ "/" ++ tex2)]