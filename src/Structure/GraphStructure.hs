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
    , graphObj :: GraphObject
    }
    | ConstructObj
    {
      diffs   :: GLfVertex3
    , dimms   :: GLfVertex3
    , objects :: [StructureObject]   
    }

hsNameLength (moduleName, hsName) = 1 + (strUnits (moduleName ++ hsName))
hsLitLength str = 1 + (strUnits str)


constructExpr (GL.Vertex3 adx ady adz) (GL.Vertex3 al ah az) (HsVar hsVar) =
    let boxLength = hsNameLength $ getHsName $ hsVar
    in  StructObj  (diff (adx + al) ady adz)
                   (dimension (al + boxLength) 2 2)
                   (variableBox boxLength 2 2)

constructExpr (GL.Vertex3 adx ady adz) (GL.Vertex3 al ah az) (HsLit hsLit) =
    let boxLength = hsLitLength $ getHsLitStr $ hsLit
    in  StructObj  (diff (adx + al) ady adz)
                   (dimension (al + boxLength) 2 2)
                   (variableBox boxLength 2 2)


constructOp   (GL.Vertex3 adx ady adz) (GL.Vertex3 al ah az) (HsQVarOp opDef) = 
    let boxLength = hsNameLength $ getHsName $ opDef
    in  StructObj  (diff (adx + al) ady adz)
                   (dimension (al + boxLength) 2 2)
                   (functionBox boxLength 2 2)


constructInfixApp adiff adim (HsInfixApp l op r) =
  let
     lObj @(StructObj ldiff   ldim@(GL.Vertex3 ll  _ _) lGraphObj ) = constructExpr adiff adim l
     opObj@(StructObj opdiff opdim@(GL.Vertex3 opl _ _) opGraphObj) = constructOp   ldiff ldim op
     rObj @(StructObj rdiff   rdim@(GL.Vertex3 rl  _ _) rGraphObj ) = constructExpr rdiff rdim r
  in  ConstructObj adiff (dimension (ll + opl + rl) 2 2) [lObj, opObj, rObj]


    
    
--        (HsInfixApp
--                    (HsVar (UnQual (HsIdent "n")))
--                    (HsQVarOp (UnQual (HsSymbol "-")))
--                    (HsLit (HsInt 1)))