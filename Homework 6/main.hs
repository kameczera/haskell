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