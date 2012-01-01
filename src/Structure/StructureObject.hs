module Structure.StructureObject where

import qualified Graphics.Rendering.OpenGL as GL 
import Language.Haskell.Syntax

import Structure.HsSyntaxTools
import Structure.GraphObject
import Units
import GLTypes

data StructureObject = 
    StructObj
        {
          trans       :: Translation        
        , dimms       :: Dimensions
        , graphObject :: GraphObject
        }
    | Construction
        {
          trans       :: Translation
        , dimms   :: Dimensions
        , objects :: [StructureObject]   
        }
    deriving (Show)
    
hsNameLength (moduleName, hsName) = 1 + (strUnits (moduleName ++ hsName))
hsLitLength str = 1 + (strUnits str)


constructExpr (HsVar hsVar) =
    let n = getHsName hsVar
        boxLength = hsNameLength n
    in  StructObj  nullVector3
                   (dimension boxLength 2 2)
                   (variableBox n boxLength 2 2)

constructExpr (HsLit hsLit) =
    let n = getHsLitStr hsLit
        boxLength = hsLitLength n
    in  StructObj  nullVector3
                   (dimension boxLength 2 2)
                   (variableBox n boxLength  2 2)

constructOp (HsQVarOp opDef) = 
    let n = getHsName opDef
        boxLength = hsNameLength n 
    in  StructObj  nullVector3
                   (dimension boxLength 1 2)
                   (functionBox n boxLength  1 2)

constructInfixApp (HsInfixApp l op r) =
  let
     lc @(StructObj _ (GL.Vector3 ll  _ _) _) = constructExpr l
     opc@(StructObj _ (GL.Vector3 opl _ _) _) = constructOp   op
     rc @(StructObj _ (GL.Vector3 rl  _ _) _) = constructExpr r
     lObj  = lc {trans = translation 0 0 0}
     opObj = opc{trans = translation ll 0 0}
     rObj  = rc {trans = translation (ll + opl) 0 0}
  in  Construction nullVector3 (dimension (ll + opl + rl) 2 2) [lObj, opObj, rObj]


{- HsApp
                (HsVar (UnQual (HsIdent "fact'")))
                (HsParen
                    (HsInfixApp
                        (HsVar (UnQual (HsIdent "n")))
                        (HsQVarOp (UnQual (HsSymbol "-")))
                        (HsLit (HsInt 1))))
                        -}