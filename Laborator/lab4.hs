-- Exercitiul 1
factori :: Int -> [Int]
factori n = [x | x <- [1..abs(n)], mod n x == 0]


-- Exercitiul 2
prim :: Int -> Bool
prim n = if length (factori n) == 2 then True else False


-- Exercitiul 3
numerePrime :: Int -> [Int]
numerePrime n = [x | x <- [2..n], prim x]


-- Exercitiul 4
myzip3 :: [a] -> [b] -> [c] -> [(a, b, c)]
myzip3 l1 l2 l3 = [(x, y, z) | (x, (y, z)) <- zip l1 (zip l2 l3)]


-- Echivalent
-- length(prim n)
-- length $ prim n
-- (length.prim) n
-- length.prim $ n


-- Exercitiul 5
firstEl :: [(a,b)] -> [a]
firstEl =  map fst 
--firstEl = map (\(x, y) -> x)


-- Exercitiul 6
sumList :: [[Int]] -> [Int]
sumList = map sum


-- Exercitiul 7
prel2 :: [Int] -> [Int]
prel2 = map (\x -> if odd x then x * 2 else div x 2)


-- Exercitiul 8
hasLetter :: Char -> [String] -> [String]
hasLetter c str = filter (elem c) str


-- Exercitiul 9
squareOdd :: [Int] -> [Int]
squareOdd = map (^2) . filter odd


-- Exercitiul 10
squareOddPos :: [Int] -> [Int]
squareOddPos = map (\x -> snd x ^ 2) . filter (odd . fst) . zip [1..]


-- Exercitiul 11
numaiVocale :: [String] -> [String]
numaiVocale = map (filter (`elem` "aeiouAEIOU"))


-- Exercitiul 12
mymap :: (a -> b) -> [a] -> [b]
mymap _ [] = []
mymap f (h:t) = f h : mymap f t

myfilter :: (a -> Bool) -> [a] -> [a]
myfilter _ [] = []
myfilter f (h:t)
    | f h = h : myfilter f t
    | otherwise = myfilter f t