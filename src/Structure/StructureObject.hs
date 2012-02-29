module Structure.StructureObject where

import qualified Graphics.Rendering.OpenGL as GL 
import Language.Haskell.Syntax

import Structure.HsSyntaxTools
import Structure.GraphObject
import Structure.Dimensions
import Common.Units
import Common.GLTypes

data StructureObject = StructureObject
        { soObjectSpec       :: ObjectSpec
        , soGeometry         :: Geometry
        , soGraphObjectSpec  :: GraphObjectSpec
        , soStructureObjects :: StructureObjects
        } deriving (Show)

type StructureObjects = [StructureObject]

data ObjectSpec = OsFunction
                | OsArgument
                | OsInfixOperator
                | OsInfixApp
                | OsFoundation
                | OsExpFoundation
                | OsArrowBridge
                | OsEqualSignBridge
                | OsGuardFrame Dimension Dimension -- outerDim, innerDim
                | OsGuardedRhs
                | OsFramedGrhs (Translation, Dimension) -- Left bridge of guard frame
                | OsFramedGrhss
    deriving (Show)

data ObjectConstructSpec
                = OcsApp HsExp
                | OcsExpArgument   HsExp
                | OcsExpFuncName   HsExp StructureObject
                | OcsInfixOperator HsQOp
                | OcsGuardedRhs    HsGuardedRhs
                | OcsFoundationExp StructureObject
                | OcsGuardedRhss   HsRhs
                | OcsArrowBridge
                | OcsEqualSignBridge
                | OcsMatch HsMatch
                
                -- Special for article
                | OcsGeneralConnector StructureObject
                | OcsFuncMatch HsName HsPat
                
                
                
