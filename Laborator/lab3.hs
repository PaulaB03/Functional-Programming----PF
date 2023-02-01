import Data.Char

-- Exerictiul 1
vocale :: String -> Integer
vocale "" = 0
vocale x = 
    if elem (head x) "aeiouAEIOU" then 1 + vocale (tail x)
    else vocale (tail x)

nrVocale :: [String] -> Integer
nrVocale [] = 0
nrVocale (h:t)
    | h == reverse h = vocale h + nrVocale t
    | otherwise = nrVocale t


-- Exercitiul 2
f :: Int -> [Int] -> [Int]
f a [] = []
f a (h:t) 
    | even h = h : a : f a t
    | otherwise = h : f a t


semiPareComp :: [Int] -> [Int]
semiPareComp l = [div x 2 | x <- l, even x]


-- Exercitiul 3
divizori :: Int -> [Int]
divizori n = [d | d <- [1..n], mod n d == 0]


-- Exercitiul 4
listaDiv :: [Int] -> [[Int]]
listaDiv ls = [divizori x | x <- ls]

-- listaDiv [] = []
-- listaDiv (h:t) = divizori h : listaDiv t


-- Exercitiul 5
inIntervalRec :: Int -> Int -> [Int] -> [Int]
inIntervalRec a b [] = []
inIntervalRec a b (h:t)
    | a <= h && h <= b = h : inIntervalRec a b t
    | otherwise = inIntervalRec a b t

inIntervalComp :: Int -> Int -> [Int] -> [Int]
inIntervalComp a b ls = [x | x <- ls, a <= x && x <= b]


-- Exercitiul 6
pozitiveRec :: [Int] -> Int
pozitiveRec [] = 0
pozitiveRec (h:t)
    | h > 0 = 1 + pozitiveRec t
    | otherwise = pozitiveRec t

pozitiveComp :: [Int] -> Int
pozitiveComp ls = sum [1 | x <- ls, x > 0]


-- Exercitiul 7
pozitiiImpare :: Int -> [Int] -> [Int]
pozitiiImpare n [] = []
pozitiiImpare n (h:t)
    | odd h = n : pozitiiImpare (n+1) t
    | otherwise = pozitiiImpare (n+1) t

pozitiiImpareRec :: [Int] -> [Int]
pozitiiImpareRec [] = []
pozitiiImpareRec ls = pozitiiImpare 0 ls

pozitiiImpareComp :: [Int] -> [Int]
pozitiiImpareComp ls = [poz | (poz, x) <- zip [0..] ls, odd x]


-- Exercitiul 8
multDigitRec :: [Char] -> Int
multDigitRec "" = 1
multDigitRec (h:t)
    | isDigit h = (digitToInt h) * multDigitRec t
    | otherwise = multDigitRec t

multDigitComp :: [Char] -> Int
multDigitComp str = product [digitToInt x | x <- str, isDigit x]