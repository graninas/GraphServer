module Structure.HsSyntaxTools where

import Language.Haskell.Syntax

getHsName (UnQual (HsIdent  identName))  = ("", identName)
getHsName (UnQual (HsSymbol symbolName)) = ("", symbolName)
getHsName (Qual (Module m) (HsIdent  identName))  = (m, identName)
getHsName (Qual (Module m) (HsSymbol symbolName)) = (m, symbolName)

getHsLitStr (HsInt i)   = show i
getHsLitStr (HsChar c)   = [c]
getHsLitStr (HsString s) = s
getHsLitStr (HsFrac frac) = show frac
getHsLitStr (HsCharPrim c) = [c]
getHsLitStr (HsStringPrim s) = s
getHsLitStr (HsIntPrim i) = show i
getHsLitStr (HsFloatPrim fl) = show fl
getHsLitStr (HsDoublePrim d) = show d