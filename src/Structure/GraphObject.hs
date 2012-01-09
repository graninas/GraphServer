module Structure.GraphObject where

import qualified Graphics.Rendering.OpenGL as GL
import Common.Units
import Common.GLTypes
import Common.Constants
import Structure.Texture
import Structure.Dimensions

data GraphObject = NoGraphObject
                 | PrimitiveBox GLfVertex3 TextureName
                 | TexturedBox  GLfVertex3 ObjectTextureSpec
                 | GraphObjects [GraphObjectSpec]

  deriving (Show)

type GraphObjectSpec = (Translation, Dimension, GraphObject)

graphObjectFromSpec :: GraphObjectSpec -> GraphObject
graphObjectFromSpec (_, _, go) = go

nullGraphObjSpec = (nullTranslation, nullDimension, NoGraphObject)

primitiveBox   trans dim@(GL.Vector3 l h w) texName = (trans, dim, PrimitiveBox (vertex3 l h w) texName)
variableBox _  dim@(GL.Vector3 l h w) = (nullVector3, dim, PrimitiveBox (vertex3 l h w) helloTex)
functionBox _  dim@(GL.Vector3 l h w) = (nullVector3, dim, PrimitiveBox (vertex3 l h w) helloTex)
foundationBox  dim@(GL.Vector3 l h w) = (nullVector3, dim, PrimitiveBox (vertex3 l h w) hazardStripeTex)
arrowBridgeBox     dim = bridgeBox dim arrowTex
equalSignBridgeBox dim = bridgeBox dim equalSignTex

bridgeBox dim@(GL.Vector3 l h w) texName =
    (nullVector3, dim, TexturedBox (vertex3 l h w) boxTexSpec)
  where
      boxTexSpec = BoxTextureSpec texes defTex
      texes      = [(SideTop, QuadTexture texName)]
      defTex     = QuadTexture yellowBaseTex

-- | Guard frame is graph object for pipe sign ('|') in guards.
-- | ____________
-- | |  |_3__|  |
-- | |  |    |  |
-- | |1 |____|2 |
-- | |__|_4__|__| | y1
-- | ___
-- |  x1
guardFrame outBoxDim inBoxDim = let
     (GL.Vector3 obl obh obw) = outBoxDim
     (GL.Vector3 ibl ibh _)   = inBoxDim
     (x1, y1) = ((obl - ibl) / 2, (obh - ibh) / 2)
     b1@(b1Trans, b1Dim) = (vector3 0           0 0, vector3 x1 obh obw)
     b2@(b2Trans, b2Dim) = (vector3 (ibl + x1)  0 0, vector3 x1 obh obw)
     b3@(b3Trans, b3Dim) = (vector3 x1 (ibh + y1) 0, vector3 ibl y1 obw)
     b4@(b4Trans, b4Dim) = (vector3 x1          0 0, vector3 ibl y1 obw)
     box1Go = primitiveBox b1Trans b1Dim hazardStripeTex
     box2Go = primitiveBox b2Trans b2Dim hazardStripeTex
     box3Go = primitiveBox b3Trans b3Dim hazardStripeTex
     box4Go = primitiveBox b4Trans b4Dim hazardStripeTex
     genDim = generalizedDimension [b1, b2, b3, b4]
     in (nullVector3, genDim, GraphObjects [box1Go, box2Go, box3Go, box4Go])

