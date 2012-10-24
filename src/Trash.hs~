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

constructExpr (HsApp func operand) _ _ =
    let
        opc   @(StructureObject (GL.Vector3 opdx opdy opdz) (GL.Vector3 opl oph _)   _ _)
            = constructExpr operand NoObjectSpec NoDeriving
        funcDims = FuncDims (functionBoxRelativeDims (GL.Vector3 opl 1 2))
        funcc @(StructureObject _ (GL.Vector3 funcl funch _) _ _) = constructExpr func OSFunction funcDims
        opHalfL   = opl   / 2
        funcHalfL = funcl / 2
        opObj     = opc   {trans = translation opdx opdy opdz}
        funcObj   = funcc {trans = translation (opdx + opHalfL - funcHalfL) (opdy-funch) opdz}
    in StructureObject nullVector3 (dimension funcl (oph + funch) 2) NoGraphObject [funcObj, opObj]

constructExpr (HsInfixApp l op r) _ derivedDims =
  let
     lc @(StructureObject (GL.Vector3 ldx ldy ldz) (GL.Vector3 ll _ _) _ _) = constructExpr l OSVariable derivedDims
     opc@(StructureObject _ (GL.Vector3 opl _ _) _ _) = constructOp   op
     rc @(StructureObject _ (GL.Vector3 rl  _ _) _ _) = constructExpr r OSVariable derivedDims
     lObj  = lc {trans = translation ldx ldy ldz}
     opObj = opc{trans = translation (ll + ldx) ldy ldz}
     rObj  = rc {trans = translation (ll + opl + ldx) ldy ldz}
  in StructureObject nullVector3 (dimension (ll + opl + rl) 2 2) NoGraphObject [lObj, opObj, rObj]