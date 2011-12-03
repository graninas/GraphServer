module Draw.Temp where

import Language.Haskell.Syntax

simpleAst1 =
    HsModule (SrcLoc {srcFilename = "<unknown>", srcLine = 1, srcColumn = 1})
    (Module "Main") Nothing
    
    [HsImportDecl {
        importLoc = SrcLoc {srcFilename = "<unknown>", srcLine = 3, srcColumn = 1}
        , importModule = Module "Language.Haskell.Parser"
        , importQualified = False
        , importAs = Nothing
        , importSpecs = Nothing}]
    
    [HsPatBind
        (SrcLoc {srcFilename = "<unknown>", srcLine = 6, srcColumn = 1})
        (HsPVar (HsIdent "main"))
        (HsUnGuardedRhs
            (HsDo [HsGenerator
                    (SrcLoc {srcFilename = "<unknown>", srcLine = 7, srcColumn = 7})
                    (HsPVar (HsIdent "s"))
                    (HsApp
                        (HsVar (UnQual (HsIdent "readFile")))
                        (HsLit (HsString "Language.hs")))
                  ,HsLetStmt [HsPatBind
                                (SrcLoc {srcFilename = "<unknown>", srcLine = 8, srcColumn = 9})
                                (HsPVar (HsIdent "parsed"))
                                (HsUnGuardedRhs (HsApp
                                                    (HsVar (UnQual (HsIdent "parseModule")))
                                                    (HsVar (UnQual (HsIdent "s")))))
                                []]
                ,HsQualifier (HsInfixApp
                                (HsInfixApp
                                    (HsVar (UnQual (HsIdent "putStrLn")))
                                    (HsQVarOp (UnQual (HsSymbol ".")))
                                    (HsVar (UnQual (HsIdent "show"))))
                                (HsQVarOp (UnQual (HsSymbol "$")))
                                (HsVar (UnQual (HsIdent "parsed"))))
                ,HsQualifier (HsApp
                                (HsApp
                                    (HsVar (UnQual (HsIdent "writeFile")))
                                    (HsLit (HsString "parsed.txt")))
                                (HsParen (HsApp
                                    (HsVar (UnQual (HsIdent "show")))
                                    (HsVar (UnQual (HsIdent "parsed"))))))])
        ) []]
        
simpleAstFacts =
    HsModule
    (SrcLoc {srcFilename = "<unknown>", srcLine = 3, srcColumn = 1})
    (Module "Main")
    (Just [HsEVar (UnQual (HsIdent "main"))])
    []
    [HsFunBind
        [HsMatch
            (SrcLoc {srcFilename = "<unknown>", srcLine = 3, srcColumn = 1})
            (HsIdent "fact")
            [HsPLit (HsInt 0)]
            (HsUnGuardedRhs (HsLit (HsInt 1)))
            []
        , HsMatch
            (SrcLoc {srcFilename = "<unknown>", srcLine = 4, srcColumn = 1})
            (HsIdent "fact")
            [HsPVar (HsIdent "n")]
            (HsUnGuardedRhs (HsInfixApp (HsApp (HsVar (UnQual (HsIdent "fact"))) (HsParen (HsInfixApp (HsVar (UnQual (HsIdent "n"))) (HsQVarOp (UnQual (HsSymbol "-"))) (HsLit (HsInt 1))))) (HsQVarOp (UnQual (HsSymbol "*"))) (HsVar (UnQual (HsIdent "n")))))
            []]
    , HsFunBind
        [HsMatch
            (SrcLoc {srcFilename = "<unknown>", srcLine = 6, srcColumn = 1})
            (HsIdent "fact'")
            [HsPVar (HsIdent "n")]
            (HsGuardedRhss
                [HsGuardedRhs
                    (SrcLoc {srcFilename = "<unknown>", srcLine = 6, srcColumn = 9})
                    (HsInfixApp (HsVar (UnQual (HsIdent "n"))) (HsQVarOp (UnQual (HsSymbol "=="))) (HsLit (HsInt 0)))
                    (HsLit (HsInt 1))
                , HsGuardedRhs
                    (SrcLoc {srcFilename = "<unknown>", srcLine = 7, srcColumn = 9})
                    (HsVar (UnQual (HsIdent "otherwise")))
                    (HsInfixApp
                        (HsApp
                            (HsVar (UnQual (HsIdent "fact'")))
                            (HsParen
                                (HsInfixApp
                                    (HsVar (UnQual (HsIdent "n")))
                                    (HsQVarOp (UnQual (HsSymbol "-")))
                                    (HsLit (HsInt 1)))))
                        (HsQVarOp
                            (UnQual (HsSymbol "*")))
                            (HsVar (UnQual (HsIdent "n"))))]) []]])