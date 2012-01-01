module Structure.GraphObject where

import Structure.PrimitiveObject
import Common.Units
import Common.Constants
import Common.GLTypes

data GraphObject = GO PrimitiveObject TextureName
    deriving (Show)

variableBox _ l h w = GO (PrimitiveBox (vertex3 l h w)) tex2
functionBox _ l h w = GO (PrimitiveBox (vertex3 l h w)) tex2
