import Data.List

myInt = 5555555555555555555555555555555555555555555555555555555555555555555555555555

double :: Integer -> Integer
double x = x + x

maxim :: Integer -> Integer -> Integer
maxim x y = if (x > y) then x else y

maxim3 :: Integer -> Integer -> Integer -> Integer
maxim3 x y z = let u = maxim x y in (maxim u z)


-- Exercitiul 6
pow2 :: Integer -> Integer -> Integer
pow2 x y = x ^ 2 + y ^ 2

even_odd :: Integer -> String
even_odd x = if odd x then "impar" else "par"

factorial :: Integer -> Integer
factorial 1 = 1
factorial x = x * factorial (x-1)

double2 :: Integer -> Integer -> Bool
double2 x y = if (x > 2 * y) then True else False