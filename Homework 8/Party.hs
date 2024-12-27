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

combineGLs :: Employee -> [GuestList] -> GuestList
combineGLs emp gls =
  let justEmp = GL [emp] (empFun emp)
  in case gls of
    [] -> justEmp
    _  -> moreFun justEmp (foldr mappend mempty gls)

nextLevel :: Employee -> [(GuestList, GuestList)] -> (GuestList, GuestList)
nextLevel emp gls = (glCons emp mempty, mostFunGL gls)
    where
        funGLs []           = []
        funGLs ((e,g):gls') = moreFun e g : funGLs gls'
        mostFunGL gls'      = foldr moreFun mempty (funGLs gls')

maxFun :: Tree Employee -> GuestList
maxFun tree = moreFun (fst result) (snd result)
  where
    maxFun' (Node emp [])   = (glCons emp mempty, mempty)
    maxFun' (Node emp frst) = nextLevel emp (map maxFun' frst)
    result = maxFun' tree

main :: IO ()
main = do str <- readFile "./company.txt" 
          let gl = maxFun (read str)
              getFun (GL _ fun) = fun
              getEmps (GL emps _) = emps
              names = map empName (getEmps gl)
          putStrLn ("Total fun: " ++ show (getFun gl))
          mapM_ putStrLn names