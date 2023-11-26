--Ex.1:
poly :: Double -> Double -> Double -> Double -> Double
poly a b c x = a*x*x+b*x+c

--Ex.2:
eeny :: Integer -> String
eeny x = if(even x) then "eeny"
            else "meeny"

--Ex.3:

--V.1:
{-
fizzbuzz :: Integer -> String
fizzbuzz x = if(mod x 15 == 0) then "FizzBuzz"
                else if(mod x 5 == 0) then "Buzz"
                else if(mod x 3 == 0) then "Fizz"
                else ""
-}
--V.2:
fizzbuzz :: Integer -> String
fizzbuzz x
    | mod x 15 == 0 = "FizzBuzz"
    | mod x 5 == 0 = "Buzz"
    | mod x 3 == 0 = "Fizz"
    | otherwise = ""

--Ex.4:
fibonacciCazuri :: Integer -> Integer
fibonacciCazuri n
    | n < 2     = n
    | otherwise = fibonacciCazuri (n - 1) + fibonacciCazuri (n - 2)
    
fibonacciEcuational :: Integer -> Integer
fibonacciEcuational 0 = 0
fibonacciEcuational 1 = 1
fibonacciEcuational n =
    fibonacciEcuational (n - 1) + fibonacciEcuational (n - 2)
    

--Definitie cu sabloane 
tribonacci :: Integer -> Integer
tribonacci 1 = 1
tribonacci 2 = 1
tribonacci 3 = 2
tribonacci n = 
    tribonacci (n - 1) + tribonacci (n - 2) + tribonacci (n - 3)

--Ex.5:
binomial :: Integer -> Integer -> Integer
binomial _ 0 = 1
binomial 0 _ = 0
binomial n k = binomial (n - 1) k + binomial (n - 1) (k - 1)

--Ex.6:
-- !!! Functii utile: head, tail, take, drop, length.

--a.:
verifL :: [Int] -> Bool
verifL lista = if(even (length lista)) then True
                else False

--b.:
{-
takefinal :: [Int] -> Int -> [Int]
takefinal lst n = if(length lst <= n) then lst
                    else drop (length lst - n) lst
-}
-- Pt. liste sau siruri de caractere
takefinal :: [a] -> Int -> [a]
takefinal lst n = if(length lst <= n) then lst
                    else drop (length lst - n) lst

--c.:
remove :: [Int] -> Int -> [Int]
remove lst n = (take (n - 1) lst) ++ (drop (n) lst)

--Ex.7:
-- semiPareRec [0,2,1,7,8,56,17,18] == [0,1,4,28,9]
semiPareRec :: [Int] -> [Int]
semiPareRec [] = []
semiPareRec (h:t)
 | even h    = h `div` 2 : t'
 | otherwise = t'
 where t' = semiPareRec t -- t' is like an alias for "semiPareRec t" function!

--a.:
myreplicate :: Int -> a -> [a]
myreplicate 1 v = [v]
myreplicate n v = v : myreplicate (n - 1) v  -- head : tail (in a list)

--b.:
sumImp :: [Int] -> Int
sumImp [] = 0
sumImp (x:xs) = if(odd x) then x + sumImp xs
                    else sumImp xs

--c.:
totalLen :: [String] -> Int
totalLen [] = 0
totalLen (x:xs) = if(head x == 'A') then length x + totalLen xs
                    else totalLen xs

--