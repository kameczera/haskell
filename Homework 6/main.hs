{-# LANGUAGE FlexibleInstances #-}

fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)

fibs :: [Integer]
fibs = [fib i | i <- [0..]]

fibs2 :: [Integer]
fibs2 = [x | (x, _) <- iterate (\(a, b) -> (b, a + b)) (0, 1)]

data Stream a = Element a (Stream a)

streamToList :: Stream a -> [a]
streamToList (Element a next) = [a] ++ streamToList next

instance Show a => Show (Stream a) where
  show stream = show (take 20 (streamToList stream))

streamRepeat :: a -> Stream a
streamRepeat elem = Element elem (streamRepeat elem)

streamMap :: (a -> b) -> Stream a -> Stream b
streamMap f (Element elem next) = Element (f elem) (streamMap f next)

streamFromSeed :: (a -> a) -> a -> Stream a
streamFromSeed f value = (Element value (streamFromSeed f (f value)))

nats :: Stream Integer
nats = streamFromSeed (+1) 0

interleaveStreams :: Stream Integer -> Stream Integer -> Stream Integer
interleaveStreams (Element elem1 next1) stream2 = (Element elem1 (interleaveStreams stream2 next1))

ruler :: Stream Integer
ruler = interleaveStreams (streamRepeat 0) (streamMap howManyTwos (streamFromSeed (+2) 2))

howManyTwos :: Integer -> Integer
howManyTwos n
 | even n = 1 + howManyTwos (n `div` 2)
 | otherwise = 0

changeElem :: Integer -> Integer -> Stream Integer -> Stream Integer
changeElem 0 cElem (Element _ next) = (Element cElem next)
changeElem index cElem (Element elem next) = (Element elem (changeElem (index - 1) cElem next)) 

x :: Stream Integer
x = changeElem 1 1 (streamRepeat 0)

fib3 :: Stream Integer
fib3 = x / (1 - x - x^2)

instance Num (Stream Integer) where
  fromInteger n = streamFromSeed (\_ -> 0) n
  negate stream = streamMap (\x -> -x) stream
  (+) (Element elemA streamA) (Element elemB streamB) = (Element (elemA+elemB) (streamA + streamB))
  (*) (Element elemA streamA) b@(Element elemB streamB) = (Element (elemA*elemB) ((streamMap (*elemA) streamB) + (b * streamA)))

instance Fractional (Stream Integer) where
  (/) (Element elemA streamA) (Element elemB streamB) = q where
      first   = elemA `div` elemB
      divider = \x -> x `div` elemB
      remainder = streamA - q * streamB
      q = (Element first (streamMap divider remainder))

data Matrix a = Matrix ((a, a), (a, a))

instance Show a => Show (Matrix a) where
  show (Matrix ((a,b), (c,d))) = "((" ++ show a ++ "," ++ show b ++ ")," ++ "(" ++ show c ++ "," ++ show d ++ "))"

instance Num (Matrix Integer) where
  (*) (Matrix ((a,b),(c,d))) (Matrix ((e,f),(g,h))) = (Matrix ((a * e + b * g, a * g + b * h),(c * e + d * g, c * f + d * h)))

fib4 :: Integer -> Integer
fib4 0 = 0
fib4 n = f ((Matrix ((1, 1), (1, 0)))^n) where f (Matrix ((a,_), _)) = a