module Structure.GraphObject where

import qualified Graphics.Rendering.OpenGL as GL
import Structure.PrimitiveObject
import Common.Units
import Common.GLTypes
import Common.Constants

data GraphObject = GraphObject
     {
         goPrimitiveObject :: PrimitiveObject
       , goTextureName     :: TextureName
     }
     | NoGraphObject
  deriving (Show)

type GraphObjectSpec = (Translation, Dimension, GraphObject)

graphObjectFromSpec :: GraphObjectSpec -> GraphObject
graphObjectFromSpec (_, _, go) = go

variableBox, functionBox :: String -> GLfVector3 -> GraphObjectSpec
foundationBox            :: GLfVector3 -> GraphObjectSpec

variableBox _  dim@(GL.Vector3 l h w) = (nullVector3, dim, GraphObject (PrimitiveBox (vertex3 l h w)) tex2)
functionBox _  dim@(GL.Vector3 l h w) = (nullVector3, dim, GraphObject (PrimitiveBox (vertex3 l h w)) tex2)
foundationBox  dim@(GL.Vector3 l h w) = (nullVector3, dim, GraphObject (PrimitiveBox (vertex3 l h w)) tex2)
arrowBridgeBox dim@(GL.Vector3 l h w) = (nullVector3, dim, GraphObject (PrimitiveBox (vertex3 l h w)) tex2)