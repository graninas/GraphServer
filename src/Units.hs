module Units where

import qualified Graphics.Rendering.OpenGL as GL

type GLfVertex2 = GL.Vertex2 GL.GLfloat
type GLfVertex3 = GL.Vertex3 GL.GLfloat
type GLfVector2 = GL.Vector2 GL.GLfloat
type GLfVector3 = GL.Vector3 GL.GLfloat
type GLfColor3  = GL.Color3  GL.GLfloat
type GLfColor4  = GL.Color4  GL.GLfloat
type Translation = GLfVector3
type Dimensions  = GLfVector3

max3 a b c = max a (max b c)

vertex3 :: GL.GLfloat -> GL.GLfloat -> GL.GLfloat -> GLfVertex3
vertex3 = GL.Vertex3
vector3 :: GL.GLfloat -> GL.GLfloat -> GL.GLfloat -> GLfVector3
vector3 = GL.Vector3
color3 ::  GL.GLfloat -> GL.GLfloat -> GL.GLfloat -> GLfColor3
color3 = GL.Color3
color4 ::  GL.GLfloat -> GL.GLfloat -> GL.GLfloat -> GL.GLfloat -> GLfColor4
color4 = GL.Color4

nullVertex3 = vertex3 0 0 0
nullVector3 = vector3 0 0 0

translation = vector3
dimension   = vector3

addX (GL.Vector3 x y z, dir) xTrans = (GL.Vector3 (x+xTrans)  y          z,         dir)
addZ (GL.Vector3 x y z, dir) yTrans = (GL.Vector3  x         (y+yTrans)  z,         dir)
addY (GL.Vector3 x y z, dir) zTrans = (GL.Vector3  x          y         (z+zTrans), dir) 

addXs pos [] = pos
addXs pos (xTrans:xs) = addXs (addX pos xTrans) xs
addYs pos [] = pos
addYs pos (yTrans:ys) = addYs (addY pos yTrans) ys
addZs pos [] = pos
addZs pos (zTrans:zs) = addZs (addZ pos zTrans) zs

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

