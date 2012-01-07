module Structure.GraphObject where

import qualified Graphics.Rendering.OpenGL as GL
import Common.Units
import Common.GLTypes
import Common.Constants
import Structure.Texture

data GraphObject = PrimitiveBox GLfVertex3 TextureName
                 | TexturedBox  GLfVertex3 ObjectTextureSpec
                 | NoGraphObject
  deriving (Show)

type GraphObjectSpec = (Translation, Dimension, GraphObject)

graphObjectFromSpec :: GraphObjectSpec -> GraphObject
graphObjectFromSpec (_, _, go) = go

variableBox, functionBox :: String -> GLfVector3 -> GraphObjectSpec
foundationBox            :: GLfVector3 -> GraphObjectSpec

variableBox _  dim@(GL.Vector3 l h w) = (nullVector3, dim, PrimitiveBox (vertex3 l h w) helloTex)
functionBox _  dim@(GL.Vector3 l h w) = (nullVector3, dim, PrimitiveBox (vertex3 l h w) helloTex)
foundationBox  dim@(GL.Vector3 l h w) = (nullVector3, dim, PrimitiveBox (vertex3 l h w) hazardStripeTex)
arrowBridgeBox dim@(GL.Vector3 l h w) = (nullVector3, dim, TexturedBox  (vertex3 l h w) boxTexSpec)
  where
      boxTexSpec = BoxTextureSpec texes defTex
      texes      = [(SideTop, QuadTexture arrowTex)]
      defTex     = QuadPlainColor colorWhite

          