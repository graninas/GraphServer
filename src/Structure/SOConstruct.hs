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

