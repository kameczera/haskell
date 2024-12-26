module Party where

import Data.Tree (Tree(..))
import Employee

glCons :: Employee -> GuestList -> GuestList
glCons employee@(Emp {empName = _, empFun = fun}) (GL employeeList totalFun) = (GL (employeeList ++ [employee]) (totalFun + fun))

moreFun :: GuestList -> GuestList -> GuestList
moreFun gl1@(GL _ totalFun1) gl2@(GL _ totalFun2) = if totalFun1 > totalFun2 then gl1 else gl2

treeFold :: (a -> b -> b) -> b -> Tree a -> b
treeFold f initial (Node root childrens) =
  foldr (\child acc -> treeFold f acc child) (f root initial) childrens

instance Monoid GuestList where
  mempty = GL [] 0
  mappend = (<>)

instance Semigroup GuestList where
  (<>) (GL employeeList1 totalFun1) (GL employeeList2 totalFun2) = (GL (employeeList1 ++ employeeList2) (totalFun1 + totalFun2))
