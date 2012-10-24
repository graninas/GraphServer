module Common.Constants where

import Common.GLTypes

texturesPath    = "./Textures"

helloTex        = "hello.tga"
hazardStripeTex = "hazard_stripe.tga"
yellowBaseTex   = "yellow_base.tga" 
arrowTex        = "arrow.tga"
equalSignTex    = "equal_sign.tga"

rawTextureData :: RawTextures
rawTextureData = [ (helloTex,        texturesPath ++ "/" ++ helloTex)
                 , (hazardStripeTex, texturesPath ++ "/" ++ hazardStripeTex)
                 , (arrowTex,        texturesPath ++ "/" ++ arrowTex)
                 , (equalSignTex,    texturesPath ++ "/" ++ equalSignTex)
                 , (yellowBaseTex,   texturesPath ++ "/" ++ yellowBaseTex)
                 ]