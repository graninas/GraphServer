module Draw.Texture where

import Graphics.Rendering.OpenGL
import ThirdParty.ImageFormats.TGA
import ThirdParty.GLUtil.Textures

import GLTypes
import Constants

makeTextureData :: RawTexture -> IO PreparedTextureObject
makeTextureData (textureName, fileName) = 
    do (width, height, pixels) <- readTGA fileName
       texture <- loadTexture $ texInfo width height TexBGR pixels
       textureFilter   Texture2D   $= ((Linear', Nothing), Linear')
       textureWrapMode Texture2D S $= (Mirrored, ClampToEdge)
       textureWrapMode Texture2D T $= (Mirrored, ClampToEdge)
       return (textureName, texture)

textures :: IO PreparedTextureObjects
textures = mapM makeTextureData rawTextureData