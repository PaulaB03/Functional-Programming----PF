import Data.ByteString (foldr')
-- Exercitiul 1
sumSquareOdd :: [Int] -> Int
sumSquareOdd l = foldr (+) 0 (map (^2) (filter odd l))


-- Exercitiul 2
checkTrue :: [Bool] -> Bool
checkTrue = foldr (&&) True


-- Exercitiul 3
allVerifies :: (Int -> Bool) -> [Int] -> Bool
allVerifies f l = foldr (&&) True (map f l)


-- Exercitiul 4
anyVerifies :: (Int -> Bool) -> [Int] -> Bool
anyVerifies f l = foldr (||) False (map f l)


-- Exercitiul 5
mapFoldr :: (a -> b) -> [a] -> [b]
mapFoldr f = foldr (\h t -> f h : t) []

filterFoldr :: (a -> Bool) -> [a] -> [a]
filterFoldr f = foldr (\h t -> if f h then h : t else t) []


-- Exercitiul 6
listToInt :: [Integer] -> Integer
listToInt = foldl (\h t -> h * 10 + t) 0


-- Exercitiul 7
-- a)
rmChar :: Char -> String -> String
rmChar c = filter (/= c)
-- rmChar c str = filter (\cr -> cr /= c) str

-- b)
rmCharRec :: String -> String -> String
rmCharRec "" str = str
rmCharRec (h:t) str = rmCharRec t (rmChar h str)

-- c)
rmCharFold :: String -> String -> String
rmCharFold chrs str = foldr rmChar str chrs
-- rmCharsFold ch s = foldr (\x xs -> rmChar x xs) s ch