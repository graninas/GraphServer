module Common.Units where

import qualified Graphics.Rendering.OpenGL as GL
import Common.GLTypes

max3 a b c = max a (max b c)

vertex3 :: GL.GLfloat -> GL.GLfloat -> GL.GLfloat -> GLfVertex3
vertex3 = GL.Vertex3
vector3 :: GL.GLfloat -> GL.GLfloat -> GL.GLfloat -> GLfVector3
vector3 = GL.Vector3
color3 ::  GL.GLfloat -> GL.GLfloat -> GL.GLfloat -> GLfColor3
color3 = GL.Color3
color4 ::  GL.GLfloat -> GL.GLfloat -> GL.GLfloat -> GL.GLfloat -> GLfColor4
color4 = GL.Color4
texCoord2 :: GL.GLfloat -> GL.GLfloat -> GLfTexCoord2
texCoord2 =  GL.TexCoord2

nullVertex3 = vertex3 0 0 0
nullVector3 = vector3 0 0 0

translation = vector3
dimension   = vector3

colorWhite :: GLfColor4
colorWhite = color4 1 1 1 1

texCoordUR = texCoord2 1 0
texCoordDR = texCoord2 0 0
texCoordDL = texCoord2 0 1
texCoordUL = texCoord2 1 1

negateVector3 (GL.Vector3 x y z) = GL.Vector3 (-x) (-y) (-z)

unit :: GL.GLfloat -> GL.GLfloat
unit n = (1.0 * n)
unit0  =  unit 0
unit0_25 = unit 0.25
unit0_5  = unit 0.5
unit1  =  unit 1
unit2  =  unit 2
unit3  =  unit 3
unit4  =  unit 4
nunit1 = -unit1
nunit2 = -unit2
nunit0_5  = -unit0_5
nunit0_25 = -unit0_25

strUnits :: String -> GL.GLfloat
strUnits s = let
    l = length s
    d = fromIntegral l * 0.5
    in d :: GL.GLfloat

