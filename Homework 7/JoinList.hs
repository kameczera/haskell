module JoinList where

import Data.Monoid ((<>))
import Data.Monoid (Product(..))
import Sized
import Scored
import Buffer
import Editor (runEditor, editor)


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

dropJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
dropJ 0 jl = jl

dropJ n (Append m jl1 jl2)
 | n >= root = Empty
 | n == left = jl2
 | n < left = (Append ((tag $ newjl1) <> (tag $ jl2)) newjl1 jl2)
 | otherwise = (Append ((tag $ jl1) <> (tag $ newjl2)) jl1 newjl2)
  where root = getSize . size $ m
        left = getSize . size . tag $ jl1
        newjl1 = (dropJ n jl1)
        newjl2 = dropJ (n - left) jl2

takeJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
takeJ 0 _ = Empty

takeJ n jl@(Append m jl1 jl2)
 | n >= root = jl
 | n == left = jl1
 | n < left = takeJ n jl1
 | otherwise = takeJ (n - left) jl2
  where root = getSize . size $ m
        left = getSize . size . tag $ jl1

scoreLine :: String -> JoinList Score String
scoreLine []  = Empty
scoreLine str = Single (scoreString str) str

-- Exercise 4
instance Buffer (JoinList (Score, Size) String) where

    toString Empty              = ""
    toString (Single _ str)     = str
    toString (Append _ jl1 jl2) = toString jl1 ++ toString jl2

    fromString []     = Empty
    fromString (x:[]) = Single (score x, 1) (x:[])
    fromString str    = Single (scoreString str, 1) str
    
    line = indexJ

    replaceLine i ln jl = takeJ i jl +++ newJl +++ dropJ (i+1) jl
      where
        newJl = Single (scoreString ln, 1) ln

    numLines jl = getSize(size (tag jl))

    value Empty                          = 0
    value (Single (Score score', _) _)   = score'
    value (Append (Score score', _) _ _) = score'

main = runEditor editor (Empty :: JoinList (Score, Size) String)

(!!?) :: [a] -> Int -> Maybe a
[] !!? _        = Nothing
_ !!? i | i < 0 = Nothing
(x:xs) !!? 0    = Just x
(x:xs) !!? i    = xs !!? (i-1)

jlToList :: JoinList m a -> [a]
jlToList Empty            = []
jlToList (Single _ a)     = [a]
jlToList (Append _ l1 l2) = jlToList l1 ++ jlToList l2