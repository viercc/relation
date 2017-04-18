module Data.Relation.Internal.Both where

import           Data.Relation.Internal.Bare

data Rel a b = Rel {
                 rmap :: Rel_ a b,
                 lmap :: Rel_ b a
               }

instance (Eq a, Eq b) => Eq (Rel a b) where
    r == s = (rmap r) == (rmap s)

instance (Show a, Show b) => Show (Rel a b) where
    show r = "fromList " ++ show (toList_ (rmap r))
