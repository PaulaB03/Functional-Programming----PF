import Data.Monoid

-------------------------------------------------------
-- setup pentru tree (din curs)

data BinaryTree a = Leaf a | Node ( BinaryTree a ) ( BinaryTree a )
    deriving Show

foldTree :: ( a -> b -> b ) -> b -> BinaryTree a -> b
foldTree f i ( Leaf x ) = f x i
foldTree f i (Node l r ) = foldTree f ( foldTree f i r ) l

instance Foldable BinaryTree where
    foldr = foldTree

-------------------------------------------------------
-- exemple pentru testare

none = Nothing
one = Just 3
tree = Node(Node( Leaf 1) ( Leaf 2) ) (Node ( Leaf 3) ( Leaf 4) )

-------------------------------------------------------

-- monoizi din curs:
-- Sum, Product, Any, All, Min, Max

-------------------------------------------------------

elem1 :: (Foldable t, Eq a) => a -> t a -> Bool
elem1 x xs = foldr (\h t -> x == h || t) False xs

elem2 :: (Foldable t, Eq a) => a -> t a -> Bool
elem2 x = foldr ((||) . (== x)) False

elem3 :: (Foldable t, Eq a) => a -> t a -> Bool
elem3 x xs = getAny $ foldMap (\a -> Any (a == x)) xs

elem4 :: (Foldable t, Eq a) => a -> t a -> Bool
elem4 x = getAny . foldMap (Any . (== x))

-------------------------------------------------------

null1 :: (Foldable t) => t a -> Bool
null1 = foldr (\_ t -> False && t) True

null2 :: (Foldable t) => t a -> Bool
null2 xs = getAll $ foldMap (\_ -> All False) xs

null3 :: (Foldable t) => t a -> Bool
null3 = getAll . foldMap (All . (const False))

-------------------------------------------------------

length1 :: (Foldable t) => t a -> Int
length1 = foldr (\_ t -> 1 + t) 0

length2 :: (Foldable t) => t a -> Int
length2 xs = getSum $ foldMap (\_ -> Sum 1) xs

length3 :: (Foldable t) => t a -> Int
length3 = getSum . foldMap (Sum . (const 1))

-------------------------------------------------------

toList1 :: (Foldable t) => t a -> [a]
toList1 = foldr (\h t -> [h] ++ t) []

toList2 :: (Foldable t) => t a -> [a]
toList2 = foldMap (\x -> [x])

toList3 :: (Foldable t) => t a -> [a]
toList3 = foldMap (:[])

-------------------------------------------------------
-- Hint: folosiți foldMap

fold1 :: (Foldable t, Monoid m) => t m -> m
fold1 = foldMap id

-------------------------------------------------------
exConst = foldMap Sum (Constant 3)
exConst2 = foldMap Any (Constant False)

data Constant a b = Constant b
instance Foldable (Constant a) where
    foldMap f (Constant b) = f b

-------------------------------------------------------
exTwo = foldMap Sum (Two 1 2)

data Two a b = Two a b
instance Foldable (Two a) where
    foldMap f (Two a b) = f b

-------------------------------------------------------
exThree = foldMap Sum (Three 1 2 3)

data Three a b c = Three a b c
instance Foldable (Three a b) where
    foldMap f (Three a b c) = f c

-------------------------------------------------------
exThree' = foldMap Sum (Three' 1 2 3)

data Three' a b = Three' a b b
instance Foldable (Three' a) where
    foldMap f (Three' a b1 b2) = f b1 <> f b2

-------------------------------------------------------
exFour' = foldMap Sum (Four' 4 5 6 7)

data Four' a b = Four' a b b b
instance Foldable (Four' a) where
    foldMap f (Four' a b1 b2 b3) = f b1 <> f b2 <> f b3

-------------------------------------------------------
exGoat = foldMap Sum (MoreGoats (OneGoat 4) NoGoat (MoreGoats (OneGoat 3) (OneGoat 6) NoGoat))
exGoat2 = foldr (*) 1 (MoreGoats (OneGoat 4) NoGoat (MoreGoats (OneGoat 3) (OneGoat 6) NoGoat))

data GoatLord a = NoGoat | OneGoat a | MoreGoats (GoatLord a) (GoatLord a) (GoatLord a)
instance Foldable GoatLord where
    foldMap f NoGoat = mempty
    foldMap f (OneGoat a) = f a
    foldMap f (MoreGoats a b c) = foldMap f a <> foldMap f b <> foldMap f c
