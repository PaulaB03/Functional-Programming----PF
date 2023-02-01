-- Exercitiul 1
poly2 :: Double -> Double -> Double -> Double -> Double
poly2 a b c x = a * x ^ 2  + b * x + c


-- Exercitiul 2
eeny :: Integer -> String
eeny x = if even x then "eeny" else "meeny"


-- Exercitiul 3
fizzbuzz :: Integer -> String
fizzbuzz x 
    | mod x 15 == 0 = "FizzBuzz"
    | mod x 3 == 0 = "Fizz"
    | mod x 5 == 0 = "Buzz"
    | otherwise = ""


fibonacciCazuri :: Integer -> Integer
fibonacciCazuri n
    | n < 2 = n
    | otherwise = fibonacciCazuri (n-1) + fibonacciCazuri (n-2)

fibonacciEcuational :: Integer -> Integer
fibonacciEcuational 0 = 0
fibonacciEcuational 1 = 1
fibonacciEcuational n = fibonacciEcuational (n-1) + fibonacciEcuational (n-2)


-- Exercitiul 4
tribonacciCazuri :: Integer -> Integer 
tribonacciCazuri n
    | n < 3 = 1
    | n == 3 = 2 
    | otherwise = tribonacciCazuri (n-1) + tribonacciCazuri (n-2) + tribonacciCazuri (n-3)

tribonacciEcuational :: Integer -> Integer
tribonacciEcuational 1 = 1
tribonacciEcuational 2 = 1
tribonacciEcuational 3 = 2
tribonacciEcuational n = tribonacciEcuational (n-1) + tribonacciEcuational (n-2) + tribonacciEcuational (n-3)


-- Exercitiul 5
binomial :: Integer -> Integer -> Integer
binomial n 0 = 1
binomial 0 k = 0
binomial n k = binomial (n-1) k + binomial (n-1) (k-1)


-- Exercitiul 6
-- a)
verifL :: [Int] -> Bool
verifL ls = even (length ls)

-- b)
takeFinal :: [Int] -> Int -> [Int]
takeFinal ls n 
    | n >= length ls = ls
    | otherwise = takeFinal (tail ls) n

-- c)
remove :: [Int] -> Int -> [Int]
remove ls n = take (n-1) ls ++ drop n ls


semiPareRec :: [Int] -> [Int]
semiPareRec [] = []
semiPareRec (h:t)
    | even h = div h 2 : semiPareRec t
    | otherwise = semiPareRec t


-- Exercitiul 7
-- a)
myreplicate :: Integer -> Integer -> [Integer]
myreplicate 0 v = []
myreplicate n v = v : myreplicate (n-1) v

-- b)
sumImp :: [Int] -> Int
sumImp [] = 0
sumImp (h:t)
    | odd h = h + sumImp t
    | otherwise = sumImp t

-- c)
totalLen :: [String] -> Int
totalLen [] = 0
totalLen (h:t)
    | head h == 'A' = length h + totalLen t
    | otherwise = totalLen t