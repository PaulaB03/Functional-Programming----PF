-- Exercitiul 1
data Punct = Pt [Int]

data Arb = Vid | F Int | N Arb Arb
          deriving Show

class ToFromArt a where 
    toArb :: a -> Arb
    fromArb :: Arb -> a

-- a
instance Show Punct where
    show (Pt []) = "()"
    show (Pt l) = "(" ++ parse l ++ ")"
        where   
            parse [] = ""
            parse [x] = show x
            parse (x:xs) = show x ++ ", " ++ parse xs

{- sau:
    show (Pt []) = "()"
    show (Pt (h : t)) = "(" ++ show h ++ (foldr (\x -> ", " ++ show x) "" t) ++ ")"
-}

-- b
instance ToFromArt Punct where
    toArb (Pt []) = Vid
    toArb (Pt [x]) = F x
    -- toArb (Pt [x]) = N (F x) Vid -- afisara ca in model
    toArb (Pt (x:xs)) = N (F x) (toArb (Pt xs))

    fromArb (Vid) = Pt []
    fromArb (F x) = Pt [x]
    fromArb (N st dr) = Pt (p1 ++ p2)
        where Pt p1 = fromArb st
              Pt p2 = fromArb dr

-- toArb (Pt [1,2,3])
-- N (F 1) (N (F 2) (N (F 3) Vid))
-- fromArb $ N (F 1) (N (F 2) (N (F 3) Vid)) :: Punct
-- (1,2,3)


-- Exercitiul 2
data Geo a = Square a | Rectangle a a | Circle a
    deriving Show

class GeoOps g where
    perimeter :: (Floating a) => g a -> a
    area :: (Floating a) => g a -> a

-- a
instance GeoOps Geo where
    perimeter (Square l) = 4 * l 
    perimeter (Rectangle l1 l2) = 2 * (l1 +l2)
    perimeter (Circle r) =  2 * pi * r

    area (Square l) = l * l
    area (Rectangle l1 l2) = l1 * l2
    area (Circle r) = pi * r * r -- r^2

-- b
instance (Floating a, Eq a) => Eq (Geo a) where
    fig1 == fig2 = perimeter fig1 == perimeter fig2
