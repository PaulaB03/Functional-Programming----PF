import Data.Sequence (Seq())
import Prelude hiding (lookup)
-- Exercitiul 1
data Expr = Const Int --integer constant
          | Expr :+: Expr -- addition
          | Expr :*: Expr -- multiplication
          deriving Eq 

data Operation = Add | Mult deriving (Eq, Show)

data Tree = Lf Int -- leaf
          | Node Operation Tree Tree -- branch
          deriving (Eq, Show)

-- Exercitiul 1.1
instance Show Expr where
  show (Const x) = show x
  show (x :+: y) = "(" ++ show x ++ " + " ++ show y ++ ")"
  show (x :*: y) = show x ++ " * " ++ show y

exp1 = ((Const 2 :*: Const 3) :+: (Const 0 :*: Const 5))
exp2 = (Const 2 :*: (Const 3 :+: Const 4))
exp3 = (Const 4 :+: (Const 3 :*: Const 3))
exp4 = (((Const 1 :*: Const 2) :*: (Const 3 :+: Const 1)) :*: Const 2)


-- Exercitiul 1.2
evalExp :: Expr -> Int
evalExp (Const x) = x
evalExp (x :+: y) = evalExp x + evalExp y
evalExp (x :*: y) = evalExp x * evalExp y


-- Exercitiul 1.3
evalArb :: Tree -> Int
evalArb (Lf x) = x
evalArb (Node Add x y) = evalArb x + evalArb y
evalArb (Node Mult x y) = evalArb x * evalArb y

arb1 = Node Add (Node Mult (Lf 2) (Lf 3)) (Node Mult (Lf 0)(Lf 5))
arb2 = Node Mult (Lf 2) (Node Add (Lf 3)(Lf 4))
arb3 = Node Add (Lf 4) (Node Mult (Lf 3)(Lf 3))
arb4 = Node Mult (Node Mult (Node Mult (Lf 1) (Lf 2)) (Node Add (Lf 3)(Lf 1))) (Lf 2)


-- Exercitiul 1.4
expToArb :: Expr -> Tree 
expToArb (Const x) = Lf x
expToArb (x :+: y) = Node Add (expToArb x) (expToArb y)
expToArb (x :*: y) = Node Mult (expToArb x) (expToArb y)


-- Exercitiul 2
class Collection c where
  empty :: c key value
  singleton :: key -> value -> c key value
  insert :: Ord key => key -> value -> c key value -> c key value
  lookup :: Ord key => key -> c key value -> Maybe value
  delete :: Ord key => key -> c key value -> c key value
  keys :: c key value -> [key]
  values :: c key value -> [value]
  toList :: c key value -> [(keu, value)]
  fromList :: Ord key => [(key, value)] -> c key value

  -- Exercitiul 2.1
  keys x = [key | (key, _) <- toList x]
  values x = [val | (_, val) <- toList x]
  fromList [] = empty
  fromList ((k,v) : t) = insert k v $ fromList t


-- Exercitiul 2.2
newtype PairList k v = PairList {getPairList :: [(k, v)]}

instance Collection PairList where
  empty = PairList[]
  singleton k v = PairList [(k, v)]


-- Exercitiul 2.3
data SearchTree key value 
  = Empty
  | BNode
      (SearchTree key value)  -- elemente cu cheia mai mica
      key                     -- cheia elementului
      (Maybe value)           -- valoarea elementului
      (SearchTree key value)  -- elemente cu cheia mai mare

instance Collection SearchTree where
  empty = Empty
  singleton k v = BNode Empty k (Just v) Empty

  insert k v st = insert' k v $ delete k st
    where
      insert' k v Empty = singleton k v
      insert' k v (BNode left stk stv right)
        | k == stk = BNode left k (Just v) right
        | k < stk = BNode (insert k v left) stk stv right
        | otherwise = BNode left stk stv (insert k v right)

  lookup _ Empty = Nothing
  lookup k (BNode left stk stv right)
    | k == stk = stv
    | k < stk = lookup k left
    | otherwise = lookup k right
    
  delete k (Empty) = Empty
  delete k (BNode left stk stv right)
    | k == stk = BNode left stk Nothing right
    | k < stk = BNode (delete k left) stk stv right
    | otherwise = BNode left stk stv (delete k right)

  -- toList Empty = []
  -- toList (BNode left stk stv right) =
  --   let current = case stv of
  --                   Nothing -> []
  --                   Just x  -> [(stk, x)]
  --   in  (toList left) ++ current ++ (toList right)