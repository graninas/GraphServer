module Structure.SOConnect (
    connectStructureObjects
    ) where

import qualified Graphics.Rendering.OpenGL as GL
import Structure.StructureObject
import Structure.GraphObject
import Structure.Dimensions
import Common.Units


framedGrhsDistance = unit2


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

connectStructureObjects OsGuardedRhs
        (arrowBridgeSo : boolExpFoundSo : equalSignSo : expFoundSo : []) = let
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
    structObjects    = [newArrBridgeSo, newBoolExpSo, newEqSBridgeSo, newExpSo]
    in StructureObject OsGuardedRhs geom nullGraphObjSpec structObjects

connectStructureObjects objSpec@(OsFramedGrhs _)
                        (frBridgeSo : frameSo : grhsSo : []) = let
    (OsGuardFrame outer inner)  = soObjectSpec frameSo
    frameDim      @(GL.Vector3 fl  fh  fw)  = outer
    frameInnerDim @(GL.Vector3 fil fih fiw) = inner
    frBridgeDim   @(GL.Vector3 fbl fbh fbw) = geometryDim . soGeometry $ frBridgeSo
    grhsDim       @(GL.Vector3 gl gh gw)    = geometryDim . soGeometry $ grhsSo
    frBridgeTrans = nullVector3
    frameTrans    = vector3 (fbl - (fl / 2)) ((fih - fh) / 2) ((fbw - fw) / 2)
    grhsTrans     = vector3 fbl 0 0
    generalDim    = generalizedDimension   [ (frameTrans,    frameDim)
                                           , (frBridgeTrans, frBridgeDim)
                                           , (grhsTrans,     grhsDim)]
    newFrBridgeSo = frBridgeSo {soGeometry = (frBridgeTrans, frBridgeDim)}
    newFrameSo    = frameSo    {soGeometry = (frameTrans,    frameDim)}
    newGhhsSo     = grhsSo     {soGeometry = (grhsTrans,     grhsDim)}
    geom          = (nullVector3, generalDim)
    structObjects = [newFrBridgeSo, newFrameSo, newGhhsSo]
    objSpec       = OsFramedGrhs frBridgeDim
    in StructureObject objSpec geom nullGraphObjSpec structObjects

connectStructureObjects (OsFramedGrhss _) objects = let
    nullStartPos                            = ([], nullVector3, nullVector3, nullVector3)
    (newObjects, genDim, _, frBridgeGenDim) = foldr dispatchFramedGrhs nullStartPos objects
    objSpec                                 = OsFramedGrhss frBridgeGenDim
    in StructureObject objSpec (nullVector3, genDim) nullGraphObjSpec newObjects

-- Special code for article.
connectStructureObjects OsMatch
        (funcMatchSo : funcFoundSo : fBridgeSo : genConnectorSo : framedGrhssSo : []) = let

    funcMatchFoundSo                      = connectStructureObjects OsExpFoundation (funcFoundSo : funcMatchSo : [])

    fmfDim   @(GL.Vector3 fmfl fmfh fmfw) = geometryDim . soGeometry $ funcMatchFoundSo
    foundDim @(GL.Vector3 fl   fh   fw)   = geometryDim . soGeometry $ funcFoundSo
    bridgeDim@(GL.Vector3 bl   bh   bw)   = geometryDim . soGeometry $ fBridgeSo
    connDim  @(GL.Vector3 cl   ch   cw)   = geometryDim . soGeometry $ genConnectorSo
    fgrhssDim@(GL.Vector3 fgrl fgrh fgrw) = geometryDim . soGeometry $ framedGrhssSo

    bridgeTrans   = vector3  fl            0 ((fmfw - bw) / 2) --(fmfh - fh)   ((bw - fw)   / 2)
    genConnTrans  = vector3 (fl + bl)      0 0 --(fmfh - ch)   ((bw - cw)   / 2)
    fgrhssTrans   = vector3 (fl + bl + cl) 0 0 --(fmfh - fgrh) ((cw - fgrw) / 2)

   -- newFmfSo      = funcMatchFoundSo {soGeometry = (nullVector3,  fmfDim)}
    newBridgeSo   = fBridgeSo      {soGeometry = (bridgeTrans,  bridgeDim)}
    newConnSo     = genConnectorSo {soGeometry = (genConnTrans, connDim)}
    newFGrhssSo   = framedGrhssSo  {soGeometry = (fgrhssTrans,  fgrhssDim)}
    newObjects    = [funcMatchFoundSo, newBridgeSo, newConnSo, newFGrhssSo]
    
    generalDim    = generalizedDimension [ (nullVector3, fmfDim)
                                         , (bridgeTrans,  bridgeDim)
                                         , (genConnTrans, connDim)
                                         , (fgrhssTrans,  fgrhssDim) ]
    in StructureObject OsMatch (nullVector3, generalDim) nullGraphObjSpec newObjects

-- End of special code.


-- calculates general dimension of all framed grhs-objects.
-- translates framed grhs-objects by z coordinate.
-- also calculates frame bridges general dimension. 
-- bridge translation == 0 because it is a pivot object.
dispatchFramedGrhs so@(StructureObject
                         (OsFramedGrhs bridgeDim) 
                         (_, dim)
                         _ _)
                   (doneObjects, trans, genDim, bridgesGenDim) = let
    (GL.Vector3 dx dy dz) = trans
    (GL.Vector3 _  _  w)  = dim
    newSo                 = so {soGeometry = (trans, dim)}
    newTrans              = vector3 dx dy (dz + w + framedGrhsDistance)
    generalDim            = generalizedDimension [ (trans,       dim)
                                                 , (nullVector3, genDim)]
    newBridgesGenDim      = generalizedDimension [ (trans,       bridgeDim)
                                                 , (nullVector3, bridgesGenDim)]
    in (newSo : doneObjects, newTrans, generalDim, newBridgesGenDim)

-- | Calculates and returns parameters for objects which one above another
-- | like foundation objects (see sketches).
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

