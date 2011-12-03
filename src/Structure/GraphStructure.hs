module Structure.GraphStructure where

import qualified Graphics.Rendering.OpenGL as GL 
import Language.Haskell.Syntax

import Structure.HsSyntaxTools
import Structure.GraphObject
import Units

data StructureObject = StructObj
    { 
      diffs    :: GLfVertex3
    , dimms    :: GLfVertex3
    , graphObject :: GraphObject
    }
    | ConstructObj
    {
      diffs   :: GLfVertex3
    , dimms   :: GLfVertex3
    , objects :: [StructureObject]   
    }
    deriving (Show)
    
hsNameLength (moduleName, hsName) = 1 + (strUnits (moduleName ++ hsName))
hsLitLength str = 1 + (strUnits str)


constructExpr (GL.Vertex3 adx ady adz) (GL.Vertex3 al ah az) (HsVar hsVar) =
    let n = getHsName hsVar
        boxLength = hsNameLength n
    in  StructObj  (diff (adx + al) ady adz)
                   (dimension (al + boxLength) 2 2)
                   (variableBox n   boxLength  2 2)

constructExpr (GL.Vertex3 adx ady adz) (GL.Vertex3 al ah az) (HsLit hsLit) =
    let n = getHsLitStr hsLit
        boxLength = hsLitLength n
    in  StructObj  (diff (adx + al) ady adz)
                   (dimension (al + boxLength) 2 2)
                   (variableBox n   boxLength  2 2)




constructOp   (GL.Vertex3 adx ady adz) (GL.Vertex3 al ah az) (HsQVarOp opDef) = 
    let n = getHsName opDef
        boxLength = hsNameLength n 
    in  StructObj  (diff (adx + al) ady adz)
                   (dimension (al + boxLength) 2 2)
                   (functionBox n   boxLength  2 2)




constructInfixApp adiff adim (HsInfixApp l op r) =
  let
     lObj @(StructObj ldiff   ldim@(GL.Vertex3 ll  _ _) _) = constructExpr adiff   adim l
     opObj@(StructObj opdiff opdim@(GL.Vertex3 opl _ _) _) = constructOp   ldiff   ldim op
     rObj @(StructObj rdiff   rdim@(GL.Vertex3 rl  _ _) _) = constructExpr opdiff opdim r
  in  ConstructObj adiff (dimension (ll + opl + rl) 2 2) [lObj, opObj, rObj]


