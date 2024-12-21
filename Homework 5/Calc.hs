module Calc where

import ExprT
import Parser
import qualified StackVM as Stack

class Expr a where
    lit :: Integer -> a
    add :: a -> a -> a
    mul :: a -> a -> a

instance Expr Integer where
    lit = id
    add = (+)
    mul = (*)

instance Expr Bool where
    lit = (> 0)
    add = (||)
    mul = (&&)

newtype MinMax = MinMax Integer deriving (Eq, Show)

instance Expr MinMax where
    lit = MinMax
    add (MinMax x) (MinMax y) = MinMax (max x y)
    mul (MinMax x) (MinMax y) = MinMax (min x y)

newtype Mod7 = Mod7 Integer deriving (Eq, Show)

instance Expr Mod7 where
    lit x = Mod7 (x `mod` 7)
    add (Mod7 x) (Mod7 y) = Mod7 ((x + y) `mod` 7)
    mul (Mod7 x) (Mod7 y) = Mod7 ((x * y) `mod` 7)

testExp :: Expr a => Maybe a
testExp = parseExp lit add mul "(3 * -4) + 5"

testInteger :: Maybe Integer
testInteger = testExp

testBool :: Maybe Bool
testBool = testExp

testMM :: Maybe MinMax
testMM = testExp

testSat :: Maybe Mod7
testSat = testExp

instance Expr Stack.Program where
  lit n = [Stack.PushI n]
  add l1 l2 = l1 ++ l2 ++ [Stack.Add]
  mul l1 l2 = l1 ++ l2 ++ [Stack.Mul]

compile :: String -> Maybe Stack.Program
compile = parseExp lit add mul