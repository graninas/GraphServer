module Structure.GraphObject where

import qualified Graphics.Rendering.OpenGL as GL
import Common.Units
import Common.GLTypes
import Common.Constants
import Structure.Texture

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
      defTex     = QuadPlainColor colorWhite

-- | Guard frame is graph object for '|' sing in guards.
-- | ____________
-- | |  |_3__|  |
-- | |  |    |  |
-- | |1 |____|2 |
-- | |__|_4__|__| | y1
-- | ___
-- |  x1
guardFrame trans outBoxDim inBoxDim = let
     (GL.Vector3 dx  dy  dz)  = trans
     (GL.Vector3 obl obh obw) = outBoxDim
     (GL.Vector3 ibl ibh _)   = inBoxDim
     x1 = (obl - ibl) / 2
     y1 = (obh - ibh) / 2
     box1Trans = trans
     box2Trans = vector3 (dx + ibl + x1) dy dz   
     box3Trans = vector3 (x1 + dx) (dy + ibh + y1) dz
     box4Trans = vector3 (x1 + dx) dy dz
     box1Spec  = primitiveBox box1Trans (vector3 x1 obh obw) hazardStripeTex
     box2Spec  = primitiveBox box2Trans (vector3 x1 obh obw) hazardStripeTex
     box3Spec  = primitiveBox box3Trans (vector3 ibl y1 obw) hazardStripeTex
     box4Spec  = primitiveBox box4Trans (vector3 ibl y1 obw) hazardStripeTex
     in GraphObjects [box1Spec, box2Spec, box3Spec, box4Spec]