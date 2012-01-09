module Common.Constants where

import Common.GLTypes

texturesPath    = "./Textures"

helloTex        = "hello.tga"
hazardStripeTex = "hazard_stripe.tga"
arrowTex        = "arrow.tga"
equalSignTex    = "arrow.tga"

rawTextureData :: RawTextures
rawTextureData = [ (helloTex,        texturesPath ++ "/" ++ helloTex)
                 , (hazardStripeTex, texturesPath ++ "/" ++ hazardStripeTex)
                 , (arrowTex,        texturesPath ++ "/" ++ arrowTex)
                 , (equalSignTex,    texturesPath ++ "/" ++ equalSignTex)
                 ]