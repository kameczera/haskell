import Data.Monoid ((<>))
import Data.Monoid (Product(..))
import Sized

data JoinList m a = Empty
                  | Single m a
                  | Append m (JoinList m a) (JoinList m a)
                  deriving (Eq, Show)

(+++) :: Monoid m => JoinList m a -> JoinList m a -> JoinList m a
(+++) jl1 jl2 = Append (tag jl1 <> tag jl2) jl1 jl2

tag :: Monoid m => JoinList m a -> m
tag Empty        = mempty
tag (Single m _) = m
tag (Append m _ _) = m

-- indexJ :: (Sized b, Monoid b) => Integer -> JoinList b a -> Maybe a
-- indexJ _ Empty = Nothing
-- indexJ 0 (Single _ a) = Just a
-- indexJ _ (Single _ _) = Nothing
-- indexJ i (Append m jl1 jl2)
--   | i < 0 || i >= root = Nothing
--   | i < left           = indexJ i jl1
--   | otherwise          = indexJ (i - left) jl2
--   where root = getSize . size $ m
--       left = getSize . size . tag $ jl1

indexJ :: (Sized b, Monoid b) => Int -> JoinList b a -> Maybe a
indexJ _ Empty     = Nothing

indexJ 0 (Single _ a) = Just a
indexJ _ (Single _ _) = Nothing

indexJ i (Append m jl1 jl2)
  | i < 0 || i >= root = Nothing
  | i < left           = indexJ i jl1
  | otherwise          = indexJ (i - left) jl2
  where root = getSize . size $ m
        left = getSize . size . tag $ jl1