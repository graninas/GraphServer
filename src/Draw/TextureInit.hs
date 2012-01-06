module Draw.TextureInit where

import qualified Graphics.UI.GLUT as GLUT (TextureObject(..))
import qualified Graphics.Rendering.OpenGL as GL
import Common.GLTypes

import qualified ThirdParty.Frag.Textures as FragTex

import qualified ThirdParty.ImageFormats.TGA as IFTGA 
import qualified ThirdParty.GLUtil.Textures  as GLUtTex

makeTextureData_GLUtils :: RawTexture -> IO PreparedTextureObject
makeTextureData_GLUtils (textureName, fileName) = do
    (width, height, pixels) <- IFTGA.readTGA fileName
    texture <- GLUtTex.loadTexture $ GLUtTex.texInfo width height GLUtTex.TexBGR pixels
    GL.textureFilter   GL.Texture2D      GL.$= ((GL.Linear', Nothing), GL.Linear')
    GL.textureWrapMode GL.Texture2D GL.S GL.$= (GL.Mirrored, GL.ClampToEdge)
    GL.textureWrapMode GL.Texture2D GL.T GL.$= (GL.Mirrored, GL.ClampToEdge)
    return (textureName, texture)

makeTextureDataFrag :: RawTexture -> IO PreparedTextureObject
makeTextureDataFrag (textureName, fileName) = do
    Just (GLUT.TextureObject tex) <- FragTex.getAndCreateTexture fileName
    return (textureName, GL.TextureObject (fromIntegral tex))


makeTextures = mapM makeTextureDataFrag
