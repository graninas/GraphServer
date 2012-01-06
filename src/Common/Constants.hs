module Common.Constants where

import Common.GLTypes

applicationPath = "./"
texturesPath    = "./Textures"


tex1 = "hello.tga"
tex2 = "hazard_stripe.tga"
tex3 = "arrow.tga"

rawTextureData :: RawTextures
rawTextureData = [(tex1, texturesPath ++ "/" ++ tex1),
                  (tex2, texturesPath ++ "/" ++ tex2),
                  (tex3, texturesPath ++ "/" ++ tex3)]