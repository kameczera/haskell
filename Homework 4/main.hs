import Prelude

fun1 :: [Integer] -> Integer
fun1 [] = 1
fun1 (x:xs)
 | even x = (x - 2) * fun1 xs
 | otherwise = fun1 xs

fun1' :: [Integer] -> Integer
fun1' = product . map (\x -> x - 2) . filter (even)

fun2 :: Integer -> [Integer]
fun2 1 = []
fun2 n
 | even n = [n] ++ fun2 (n `div` 2)
 | otherwise = fun2 (3 * n + 1)

fun2' :: Integer -> [Integer]
fun2' n = filter (even) $ takeWhile (> 1) $ (iterate (\x -> if even x then x `div` 2 else 3 * x + 1) n)

data Tree a = Leaf
 | Node Integer (Tree a) a (Tree a)
 deriving (Show, Eq)

height :: Tree a -> Integer
height Leaf = -1
height (Node h _ _ _) = h

insertBalanced :: a -> Tree a -> Tree a
insertBalanced x Leaf = Node 0 Leaf x Leaf
insertBalanced x (Node h left val right)
 | height left <= height right = 
   let newLeft = insertBalanced x left
   in Node (1 + max (height newLeft) (height right)) newLeft val right
 | otherwise = 
   let newRight = insertBalanced x right
   in Node (1 + max (height left) (height newRight)) left val newRight

foldTree :: [a] -> Tree a
foldTree = foldr insertBalanced Leaf

xor :: [Bool] -> Bool
xor = foldl (\x y -> if x == True && y == True then False else True) False

map' :: (a -> b) -> [a] -> [b]
map' f = foldr (\x y -> (f x) : y ) []

myFoldl :: (a -> b -> a) -> a -> [b] -> a
myFoldl f base xs = foldr (\x g acc -> g (f acc x)) id xs base