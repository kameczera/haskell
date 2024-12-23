{-# LANGUAGE GeneralizedNewtypeDeriving, FlexibleInstances #-}

module Sized where

import Data.Monoid

newtype Size = Size Int
  deriving (Eq, Ord, Show, Num)

getSize :: Size -> Int
getSize (Size i) = i

class Sized a where
  size :: a -> Size

instance Sized Size where
  size = id

instance Sized b => Sized (a,b) where
  size = size . snd

instance Semigroup Size where
  (<>) (Size x) (Size y) = Size (x + y)
  
instance Monoid Size where
  mempty = Size 0
  mappend = (<>)
