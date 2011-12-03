module Structure.GraphObject where

import Structure.PrimitiveObject
import Units

data GraphObject = GO PrimitiveObject
    deriving (Show)

variableBox _ l h w = GO . PrimitiveBox $ (vertex3 l h w)
functionBox _ l h w = GO . PrimitiveBox $ (vertex3 l h w)
