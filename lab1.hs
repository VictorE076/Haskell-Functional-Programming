import Data.List

myInt = 31415926535897932384626433832795028841971693993751058209749445923

double :: Integer -> Integer
double x = x+x
triple x = double x + x

maxim :: Integer -> Integer -> Integer
maxim x y = if (x > y)
               then x
          else y

max3 :: Integer -> Integer -> Integer -> Integer
max3 x y z = let
             u = maxim x y
             in (maxim  u z)

max4 :: Integer -> Integer -> Integer -> Integer -> Integer
max4 x y z t = let
               a = max3 x y z
               in (maxim a t)

division :: Integer -> Integer -> Integer
division x y = x `div` y

--Ex
--a
sumpp :: Integer -> Integer -> Integer
sumpp x y = x*x + y*y

--b
strg :: Integer -> [Char]
strg x = if(x `mod` 2 == 0)
          then "par"
          else "impar"

--c
factorial :: Integer -> Integer
factorial 0 = 1
factorial n = n * factorial (n-1)
--
--factorial n = if(n == 0)
               --then 1 
               --else n * factorial (n-1)

--d
bgdbl :: Integer -> Integer -> Bool
bgdbl x y = if(x > double y)
               then True
               else False

--e
maxNum :: Ord a => [a] -> a -- any type (Ord a)
maxNum [x] = x                -- only 1 element
maxNum (x:y:xs) = maxNum ((if(x > y) then x else y):xs) 