module Structure.StructureObject where

import qualified Graphics.Rendering.OpenGL as GL 
import Language.Haskell.Syntax

import Structure.HsSyntaxTools
import Structure.GraphObject
import Structure.Dimensions
import Common.Units
import Common.GLTypes

data StructureObject = StructureObject
        {
          soObjectSpec       :: ObjectSpec
        , soGeometry         :: Geometry
        , soGraphObjectSpec  :: GraphObjectSpec
        , soStructureObjects :: StructureObjects
        }
    deriving (Show)

type StructureObjects = [StructureObject]

data ObjectSpec = OsFunction
                | OsArgument
                | OsInfixOperator
                | OsInfixApp
    deriving (Show)

data ObjectConstructSpec
                = OcsApp HsExp
                | OcsExpArgument HsExp
                | OcsExpFuncName HsExp StructureObject
                | OcsInfixOperator HsQOp
