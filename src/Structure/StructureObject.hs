module Structure.StructureObject where

import qualified Graphics.Rendering.OpenGL as GL 
import Language.Haskell.Syntax

import Structure.HsSyntaxTools
import Structure.GraphObject
import Structure.Dimensions
import Common.Units
import Common.GLTypes

data StructureObject = StructureObject
        {
          trans       :: Translation        
        , dimms       :: Dimensions
        , graphObject :: GraphObject
        , objects :: [StructureObject]
        }
    deriving (Show)

data ObjectSpec = NoObjectSpec
                | OSFunction
                | OSVariable
                | OSOperator

hsNameLength (moduleName, hsName) = 1 + (strUnits (moduleName ++ hsName))
hsLitLength str = 1 + (strUnits str)

structObject OSVariable objText diffVector objDims@(GL.Vector3 l _ _) =
    StructureObject diffVector (dimension l 2 2) (variableBox objText l 2 2) []
structObject OSFunction objText diffVector objDims@(GL.Vector3 l _ _) =
    StructureObject diffVector (dimension l 1 2) (functionBox objText l 1 2) []
structObject OSOperator objText diffVector objDims@(GL.Vector3 l _ _) =
    StructureObject diffVector (dimension l 1 2) (functionBox objText l 1 2) []
structObject NoObjectSpec _ _ _ = undefined


constructOp (HsQVarOp opDef) = 
    let objText = getHsName opDef
        boxLength = hsNameLength objText 
    in structObject OSOperator objText nullVector3 (GL.Vector3 boxLength 0 0)

constructExpr :: HsExp -> ObjectSpec -> DerivedDimensions -> StructureObject
constructExpr (HsVar hsVar) objSpec derivedDims =
    let objText = getHsName hsVar
        textLen = hsNameLength objText
        objDims = dimensions derivedDims (vector3 textLen 0 0)
    in structObject objSpec objText nullVector3 objDims 

constructExpr (HsLit hsLit) objSpec derivedDims =
    let objText = getHsLitStr hsLit
        textLen = hsLitLength objText
        objDims = dimensions derivedDims (vector3 textLen 0 0)
    in structObject objSpec objText nullVector3 objDims
        
    
constructExpr (HsParen parenExp) objSpec derivedDims = constructExpr parenExp objSpec derivedDims

constructExpr (HsInfixApp l op r) _ derivedDims =
  let
     lc @(StructureObject _ (GL.Vector3 ll  _ _) _ _) = constructExpr l OSVariable derivedDims
     opc@(StructureObject _ (GL.Vector3 opl _ _) _ _) = constructOp   op
     rc @(StructureObject _ (GL.Vector3 rl  _ _) _ _) = constructExpr r OSVariable derivedDims
     lObj  = lc {trans = translation 0 0 0}
     opObj = opc{trans = translation ll 0 0}
     rObj  = rc {trans = translation (ll + opl) 0 0}
  in StructureObject nullVector3 (dimension (ll + opl + rl) 2 2) NoGraphObject [lObj, opObj, rObj]

constructExpr (HsApp func op) _ _ =
    let
        opc   @(StructureObject _ (GL.Vector3 opl oph _)     _ _) = constructExpr op   OSOperator NoDeriving
        fDims = FuncDims (functionBoxRelativeDims (GL.Vector3 opl 1 2))
        funcc @(StructureObject _ (GL.Vector3 funcl funch _) _ _) = constructExpr func OSFunction fDims
        opHalfL = opl / 2
        funcHalfL = funcl / 2
        opObj    = opc   {trans = translation 0 0 0}
        funcObj  = funcc {trans = translation (opHalfL - funcHalfL) (-funch) 0}
    in StructureObject nullVector3 (dimension funcl (oph + funch) 2) NoGraphObject [opObj, funcObj]
