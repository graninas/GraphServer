module Structure.StructureObject where

import qualified Graphics.Rendering.OpenGL as GL 
import Language.Haskell.Syntax

import Structure.HsSyntaxTools
import Structure.GraphObject
import Structure.Dimensions
import Common.Units
import Common.GLTypes

data StructureObject = 
    StructObj
        {
          trans       :: Translation        
        , dimms       :: Dimensions
        , graphObject :: GraphObject
        }
    | Construction
        {
          trans   :: Translation
        , dimms   :: Dimensions
        , objects :: [StructureObject]   
        }
    deriving (Show)



hsNameLength (moduleName, hsName) = 1 + (strUnits (moduleName ++ hsName))
hsLitLength str = 1 + (strUnits str)

constructOp (HsQVarOp opDef) = 
    let n = getHsName opDef
        boxLength = hsNameLength n 
    in  StructObj  nullVector3
                   (dimension boxLength 1 2)
                   (functionBox n boxLength  1 2)

constructExpr :: HsExp -> DerivedDimensions -> StructureObject
constructExpr (HsVar hsVar) derivedDims =
    let n = getHsName hsVar
        textLen = hsNameLength n
        (GL.Vector3 boxLength _ _) = dimensions derivedDims (vector3 textLen 0 0)
    in  StructObj  nullVector3
                   (dimension boxLength 2 2)
                   (variableBox n boxLength 2 2)

constructExpr (HsLit hsLit) derivedDims =
    let n = getHsLitStr hsLit
        textLen = hsLitLength n
        (GL.Vector3 boxLength _ _) = dimensions derivedDims (vector3 textLen 0 0)
    in  StructObj  nullVector3
                   (dimension boxLength 2 2)
                   (variableBox n boxLength  2 2)

constructExpr (HsParen parenExp) derivedDims = constructExpr parenExp derivedDims

constructExpr (HsInfixApp l op r) derivedDims =
  let
     lc @(StructObj _ (GL.Vector3 ll  _ _) _) = constructExpr l  derivedDims
     opc@(StructObj _ (GL.Vector3 opl _ _) _) = constructOp   op
     rc @(StructObj _ (GL.Vector3 rl  _ _) _) = constructExpr r  derivedDims
     lObj  = lc {trans = translation 0 0 0}
     opObj = opc{trans = translation ll 0 0}
     rObj  = rc {trans = translation (ll + opl) 0 0}
  in  Construction nullVector3 (dimension (ll + opl + rl) 2 2) [lObj, opObj, rObj]

constructExpr (HsApp func op) _ =
    let
        opc   @(Construction _ opDims@(GL.Vector3 opl oph _) _) = constructExpr op NoDeriving
        fDims = FuncDims (functionBoxRelativeDims opDims)
        funcc @(StructObj    _ (GL.Vector3 funcl funch _) _)    = constructExpr func fDims
        opHalfL = opl / 2
        funcHalfL = funcl / 2
        opObj    = opc   {trans = translation 0 0 0}
        funcObj  = funcc {trans = translation (opHalfL - funcHalfL) (-oph) 0}
    in  Construction nullVector3 (dimension funcl (oph + funch) 2) [opObj, funcObj]
