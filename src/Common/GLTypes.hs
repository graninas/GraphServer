module Common.GLTypes where

import qualified Graphics.Rendering.OpenGL as GL

type GLfVertex2 = GL.Vertex2 GL.GLfloat
type GLfVertex3 = GL.Vertex3 GL.GLfloat
type GLfVector2 = GL.Vector2 GL.GLfloat
type GLfVector3 = GL.Vector3 GL.GLfloat
type GLfColor3  = GL.Color3  GL.GLfloat
type GLfColor4  = GL.Color4  GL.GLfloat
type GLfTexCoord2 = GL.TexCoord2 GL.GLfloat
type Translation  = GLfVector3
type Dimension    = GLfVector3
type Dimensions   = [Dimension]
type Geometry     = (Translation, Dimension)
type Geometries   = [Geometry]

type TextureName = String
type RawTexture  = (TextureName, FilePath)
type RawTextures = [RawTexture]

type PreparedTextureObject  = (TextureName, GL.TextureObject)
type PreparedTextureObjects = [PreparedTextureObject]

data GLResources = GLResources PreparedTextureObjects

type DrawFunction = GLResources -> GL.GLfloat -> IO ()