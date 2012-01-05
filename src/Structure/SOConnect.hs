module Structure.SOConnect where

import qualified Graphics.Rendering.OpenGL as GL
import Structure.StructureObject
import Structure.GraphObject
import Structure.Dimensions
import Common.Units

connectStructureObjects :: ObjectSpec -> StructureObjects -> StructureObject
connectStructureObjects OsInfixApp (exp1So:opSo:exp2So:[]) = let
    exp1SoDim@(GL.Vector3 e1dl e1dh e1dw) = geometryDim . soGeometry $ exp1So
    exp2SoDim                             = geometryDim . soGeometry $ exp2So
    opSoDim  @(GL.Vector3 opdl opdh opdw) = geometryDim . soGeometry $ opSo
    exp1Trans    = nullVector3
    opTrans      = vector3  e1dl         0 0
    exp2Trans    = vector3 (e1dl + opdl) 0 0
    generalDim   = generalizedDimension [(exp1Trans, exp1SoDim), (exp2Trans, exp2SoDim), (opTrans, opSoDim)]
    newOpGoSpec  = (opTrans, opSoDim, graphObjectFromSpec . soGraphObjectSpec $ opSo)
    newExp1So    = exp1So {soGeometry = (exp1Trans, exp1SoDim)}
    newExp2So    = exp2So {soGeometry = (exp2Trans, exp2SoDim)}
    in StructureObject OsInfixApp (nullVector3, generalDim) newOpGoSpec [newExp1So, newExp2So]

connectStructureObjects OsFunction (funcSo:expSo:[]) = let
    funcSoDim@(GL.Vector3 funcdl funcdh funcdw) = geometryDim . soGeometry $ funcSo
    expSoDim @(GL.Vector3  expdl  expdh  expdw) = geometryDim . soGeometry $ expSo
    expdx     = (funcdl / 2) - (expdl  / 2)
    expdy     = funcdh
    expdz     = (funcdw / 2) - (expdw  / 2)
    funcTrans = nullVector3
    expTrans  = vector3 expdx expdy expdz
    newExpSo  = expSo {soGeometry = (expTrans, expSoDim)}
    newFuncGoSpec = (funcTrans, funcSoDim, graphObjectFromSpec . soGraphObjectSpec $ funcSo)
    generalDim    = generalizedDimension [(expTrans, expSoDim), (funcTrans, funcSoDim)]
    in StructureObject OsFunction (nullVector3, generalDim) newFuncGoSpec [newExpSo]
        
    