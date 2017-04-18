module Data.Relation.Internal.RTL where

import           Data.Relation.Internal.Bare
import           Data.Tuple                  (swap)

newtype Rel a b = Rel { impl :: Rel_ b a }
                deriving Eq

instance (Show a, Show b) => Show (Rel a b) where
    show (Rel r) = "fromList " ++ show (swap <$> toAscList_ r)
