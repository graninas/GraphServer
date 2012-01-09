module Structure.SOConstruct where

import qualified Graphics.Rendering.OpenGL as GL 
import Language.Haskell.Syntax
import Structure.StructureObject
import Structure.GraphObject
import Structure.HsSyntaxTools
import Structure.Dimensions
import Structure.SOConnect
import Common.Units



constructQOp (OcsInfixOperator op) = let
    opText       = getHsQOpName op
    dim          = GL.Vector3 (hsNameLength opText) 1 2
    graphObjSpec = functionBox opText dim
    in StructureObject OsInfixOperator (nullVector3, dim) graphObjSpec []

constructExp :: ObjectConstructSpec -> StructureObject

constructExp (OcsExpFuncName (HsVar func) (StructureObject _ (_, foundObjDims) _ _)) = let
    funcText     = makeName . getHsQualName $ func
    rawDim       = GL.Vector3 (hsNameLength funcText) 1 2
    dim          = derivedDimensions (FuncDimensions (funcBoxDerivedDims foundObjDims)) rawDim
    graphObjSpec = functionBox funcText dim
    in StructureObject OsFunction (nullVector3, dim)    graphObjSpec []

constructExp (OcsExpArgument (HsVar var)) = let
    varText      = makeName . getHsQualName $ var
    rawDim       = GL.Vector3 (hsNameLength varText) 2 2
    dim          = derivedDimensions (FuncDimensions variableBoxDims) rawDim
    graphObjSpec = variableBox varText dim
    in StructureObject OsArgument (nullVector3, dim) graphObjSpec []

constructExp (OcsExpArgument (HsLit lit)) = let
    litText      = getHsLitStr lit
    rawDim       = GL.Vector3 (hsNameLength litText) 2 2
    dim          = derivedDimensions (FuncDimensions variableBoxDims) rawDim
    graphObjSpec = variableBox litText dim
    in StructureObject OsArgument (nullVector3, dim) graphObjSpec []

constructExp (OcsExpArgument (HsParen exp)) =
    constructExp (OcsExpArgument exp)

constructExp (OcsExpArgument app@(HsApp exp1 exp2)) =
    constructExp (OcsApp app)

constructExp (OcsExpArgument (HsInfixApp exp1 qOp exp2)) = let
    exp1So = constructExp (OcsExpArgument   exp1)
    qOpSo  = constructQOp (OcsInfixOperator qOp)
    exp2So = constructExp (OcsExpArgument   exp2)
    in connectStructureObjects OsInfixApp [exp1So, qOpSo, exp2So]

constructExp (OcsApp (HsApp func exp)) = let
    expSo  = constructExp (OcsExpArgument exp)
    funcSo = constructExp (OcsExpFuncName func expSo)
    in connectStructureObjects OsFunction [funcSo, expSo]

constructFoundation (OcsFoundationExp expSo) = let
    expSoDim     = geometryDim . soGeometry $ expSo
    dim          = derivedDimensions FoundationDimensions expSoDim
    graphObjSpec = foundationBox dim
    in StructureObject OsFoundation (nullVector3, dim) graphObjSpec []

constructBridge osBridgeType = let
    dim = vector3 2 0.25 2
    graphObjSpec = case osBridgeType of
                    OsArrowBridge     -> arrowBridgeBox     dim
                    OsEqualSignBridge -> equalSignBridgeBox dim
    in StructureObject osBridgeType (nullVector3, dim) graphObjSpec []

--constructGuardFrame (StructureObject OsGuardedRhs )

constructGuardedRhs (OcsGuardedRhs (HsGuardedRhs _ boolExp exp)) = let
    expSo                = constructExp (OcsExpArgument exp)
    boolExpSo            = constructExp (OcsExpArgument boolExp)
    expFoundationSo      = constructFoundation (OcsFoundationExp expSo)
    boolExpFoundationSo  = constructFoundation (OcsFoundationExp boolExpSo)
    equalSignBridgeSo    = constructBridge      OsEqualSignBridge
    arrowBridgeSo        = constructBridge      OsArrowBridge
    expStructObjects     = [expFoundationSo, expSo]
    boolExpStructObjects = [boolExpFoundationSo, boolExpSo]
    expResSo             = connectStructureObjects OsExpFoundation expStructObjects
    boolExpResSo         = connectStructureObjects OsExpFoundation boolExpStructObjects
    guargedRhsSoObjects  = [boolExpResSo, equalSignBridgeSo, expResSo]
    in connectStructureObjects (OsGuardedRhs arrowBridgeSo) guargedRhsSoObjects

{-
constructGuardedRhss (OcsGuardedRhss (HsGuardedRhss gRhss)) = let
    constructedGrhss = map (constructGuardedRhs . OcsGuardedRhs)
    framedGrhss      = map  constructGuardFrame constructedGrhss
    in undefined
    
    -}