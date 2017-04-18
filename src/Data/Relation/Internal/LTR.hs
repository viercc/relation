module Data.Relation.Internal.LTR where

import           Data.Relation.Internal.Bare

newtype Rel a b = Rel { impl :: Rel_ a b }
                deriving (Eq, Ord)

instance (Show a, Show b) => Show (Rel a b) where
    show (Rel r) = "fromList " ++ show (toAscList_ r)
