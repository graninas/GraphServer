module Structure.SOConnect (
    connectStructureObjects
    ) where

import qualified Graphics.Rendering.OpenGL as GL
import Structure.StructureObject
import Structure.GraphObject
import Structure.Dimensions
import Common.Units

connectStructureObjects :: ObjectSpec -> StructureObjects -> StructureObject

connectStructureObjects OsInfixApp (exp1So : opSo : exp2So : []) = let
    exp1SoDim@(GL.Vector3 e1dl e1dh e1dw) = geometryDim . soGeometry $ exp1So
    exp2SoDim                             = geometryDim . soGeometry $ exp2So
    opSoDim  @(GL.Vector3 opdl opdh opdw) = geometryDim . soGeometry $ opSo
    exp1Trans    = nullVector3
    opTrans      = vector3  e1dl         0 0
    exp2Trans    = vector3 (e1dl + opdl) 0 0
    generalDim   = generalizedDimension [ (exp1Trans, exp1SoDim)
                                        , (exp2Trans, exp2SoDim)
                                        , (opTrans, opSoDim)]
    newOpGoSpec  = (opTrans, opSoDim, graphObjectFromSpec . soGraphObjectSpec $ opSo)
    newExp1So    = exp1So {soGeometry = (exp1Trans, exp1SoDim)}
    newExp2So    = exp2So {soGeometry = (exp2Trans, exp2SoDim)}
    in StructureObject OsInfixApp (nullVector3, generalDim) newOpGoSpec [newExp1So, newExp2So]

connectStructureObjects OsFunction (funcSo : expSo : []) = let
    (generalDim, newFuncGoSpec, newExpSo) = calcFoundationLikeParams funcSo expSo
    in StructureObject OsFunction (nullVector3, generalDim) newFuncGoSpec [newExpSo]

connectStructureObjects OsExpFoundation (foundSo:expSo:[]) = let
    (generalDim, newFoundGoSpec, newExpSo) = calcFoundationLikeParams foundSo expSo
    in StructureObject OsExpFoundation (nullVector3, generalDim) newFoundGoSpec [newExpSo]

connectStructureObjects (OsGuardedRhs arrowBridgeSo)
                        (boolExpFoundSo : equalSignSo : expFoundSo : []) = let
    arrDim    @(GL.Vector3 arrl arrh arrw) = geometryDim . soGeometry $ arrowBridgeSo
    boolExpDim@(GL.Vector3 bel  beh  bew)  = geometryDim . soGeometry $ boolExpFoundSo
    eqSignDim @(GL.Vector3 eql  eqh  eqw)  = geometryDim . soGeometry $ equalSignSo
    expDim    @(GL.Vector3 expl exph expw) = geometryDim . soGeometry $ expFoundSo
    (_, (GL.Vector3 _ bfdh bfdw), _)       = soGraphObjectSpec boolExpFoundSo
    (_, (GL.Vector3 _ efdh efdw), _)       = soGraphObjectSpec expFoundSo
    arrTrans         = nullVector3
    boolExpTrans     = vector3 arrl (arrh - bfdh) ((arrw - bfdw) / 2)
    eqSTrans         = vector3 (arrl + bel) 0 0
    expTrans         = vector3 (arrl + bel + eql) (arrh - efdh) ((arrw - efdw) / 2)
    generalDim       = generalizedDimension [ (boolExpTrans, boolExpDim)
                                            , (expTrans,     expDim)
                                            , (arrTrans,     arrDim)
                                            , (eqSTrans,     eqSignDim)]
    newArrBridgeSo   = arrowBridgeSo  {soGeometry = (arrTrans,     arrDim)}
    newBoolExpSo     = boolExpFoundSo {soGeometry = (boolExpTrans, boolExpDim)}
    newEqSBridgeSo   = equalSignSo    {soGeometry = (eqSTrans,     eqSignDim)}
    newExpSo         = expFoundSo     {soGeometry = (expTrans,     expDim)}
    geom             = (nullVector3, generalDim)
    arrBridgeSpec    = OsGuardedRhs newArrBridgeSo
    structObjects    = [newArrBridgeSo, newBoolExpSo, newEqSBridgeSo, newExpSo]
    in StructureObject arrBridgeSpec geom nullGraphObjSpec structObjects

connectStructureObjects OsFramedGrhs (frBridgeSo : frameSo : grhsSo : []) = let
    (OsGuardFrame outer inner)  = soObjectSpec frameSo
    frameDim      @(GL.Vector3 fl  fh  fw)  = outer
    frameInnerDim @(GL.Vector3 fil fih fiw) = inner
    frBridgeDim   @(GL.Vector3 fbl fbh fbw) = geometryDim . soGeometry $ frBridgeSo
    grhsDim       @(GL.Vector3 gl gh gw)    = geometryDim . soGeometry $ grhsSo
    frBridgeTrans = nullVector3
    frameTrans    = vector3 (fbl - (fl / 2)) ((fih - fh) / 2) ((fbw - fw) / 2)
    grhsTrans     = vector3 fbl 0 0
    generalDim    = generalizedDimension [ (frameTrans,    frameDim)
                                         , (frBridgeTrans, frBridgeDim)
                                         , (grhsTrans,     grhsDim)]
    newFrBridgeSo = frBridgeSo {soGeometry = (frBridgeTrans, frBridgeDim)}
    newFrameSo    = frameSo    {soGeometry = (frameTrans,    frameDim)}
    newGhhsSo     = grhsSo     {soGeometry = (grhsTrans,     grhsDim)}
    geom          = (nullVector3, generalDim)
    structObjects = [newFrBridgeSo, newFrameSo, newGhhsSo]
    in StructureObject OsFramedGrhs geom nullGraphObjSpec structObjects


calcFoundationLikeParams funcSo expSo = let
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
    in (generalDim, newFuncGoSpec, newExpSo)

