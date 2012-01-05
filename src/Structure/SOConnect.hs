module Structure.SOConnect where

import qualified Graphics.Rendering.OpenGL as GL
import Structure.StructureObject
import Structure.GraphObject
import Structure.Dimensions
import Common.Units

connectStructureObjects :: ObjectSpec -> StructureObjects -> StructureObject
connectStructureObjects OsInfixApp (exp1SO:opSO:exp2SO:[]) = let
    exp1SoDim@(GL.Vector3 e1dl e1dh e1dw) = geometryDim . soGeometry $ exp1SO
    exp2SoDim                             = geometryDim . soGeometry $ exp2SO
    opSoDim  @(GL.Vector3 opdl opdh opdw) = geometryDim . soGeometry $ opSO
    exp1Trans = nullVector3
    opTrans   = vector3  e1dl         0 0
    exp2Trans = vector3 (e1dl + opdl) 0 0
    newOpGoSpec  = (opTrans, opSoDim, graphObjectFromSpec . soGraphObjectSpec $ opSO)
    generalDim   = generalizedDimension [(exp1Trans, exp1SoDim), (exp2Trans, exp2SoDim), (opTrans, opSoDim)]
    newExp1So = exp1SO {soGeometry = (exp1Trans, exp1SoDim)}
    newExp2So = exp2SO {soGeometry = (exp2Trans, exp2SoDim)}
    in StructureObject OsInfixApp (nullVector3, generalDim) newOpGoSpec [newExp1So, newExp2So]