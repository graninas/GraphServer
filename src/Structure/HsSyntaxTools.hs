module Structure.HsSyntaxTools where

import Language.Haskell.Syntax

makeName (moduleName, hsName) = moduleName ++ "." ++ hsName

getHsQualName (UnQual (HsIdent  identName))  = ("", identName)
getHsQualName (UnQual (HsSymbol symbolName)) = ("", symbolName)
getHsQualName (Qual (Module m) (HsIdent  identName))  = (m, identName)
getHsQualName (Qual (Module m) (HsSymbol symbolName)) = (m, symbolName)

getHsLitStr (HsInt i)        = show i
getHsLitStr (HsChar c)       = [c]
getHsLitStr (HsString s)     = s
getHsLitStr (HsFrac frac)    = show frac
getHsLitStr (HsCharPrim c)   = [c]
getHsLitStr (HsStringPrim s) = s
getHsLitStr (HsIntPrim i)    = show i
getHsLitStr (HsFloatPrim fl) = show fl
getHsLitStr (HsDoublePrim d) = show d

getHsQOpName op = case op of
    HsQVarOp x -> makeName . getHsQualName $ x
    HsQConOp x -> makeName . getHsQualName $ x