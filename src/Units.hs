module Units where

import qualified Graphics.Rendering.OpenGL as GL

type GLfVertex2 = GL.Vertex2 GL.GLfloat
type GLfVertex3 = GL.Vertex3 GL.GLfloat
type GLfVector2 = GL.Vector2 GL.GLfloat
type GLfVector3 = GL.Vector3 GL.GLfloat
type GLfColor4  = GL.Color4  GL.GLfloat

max3 a b c = max a (max b c)

vertex3 :: GL.GLfloat -> GL.GLfloat -> GL.GLfloat -> GLfVertex3
vertex3 x y z = GL.Vertex3 x y z

nullVertex3 = vertex3 0 0 0

diff = vertex3
dimension = vertex3

addX (GL.Vertex3 x y z, dir) xDiff = (GL.Vertex3 (x+xDiff)  y         z,        dir)
addZ (GL.Vertex3 x y z, dir) yDiff = (GL.Vertex3  x        (y+yDiff)  z,        dir)
addY (GL.Vertex3 x y z, dir) zDiff = (GL.Vertex3  x         y        (z+zDiff), dir) 

addXs pos [] = pos
addXs pos (xDiff:xs) = addXs (addX pos xDiff) xs
addYs pos [] = pos
addYs pos (yDiff:ys) = addYs (addY pos yDiff) ys
addZs pos [] = pos
addZs pos (zDiff:zs) = addZs (addZ pos zDiff) zs

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

