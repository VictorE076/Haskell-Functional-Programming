import Data.Char

--1:
nrVocale :: [String] -> Int
nrVocale [] = 0
nrVocale (x:xs) = if(x == reverse x) then nrVocale xs + nrVocaleAux x
                    else nrVocale xs

nrVocaleAux :: String -> Int
nrVocaleAux [] = 0
nrVocaleAux (x:xs) = if(elem x "aeiouAEIOU") then 1 + nrVocaleAux xs
                    else nrVocaleAux xs
-- nrVocale ["sos", "civic", "palton", "desen", "aerisirea"] == 9


--2:
f :: Int -> [Int] -> [Int]
f _ [] = []
f z (x:xs) = if(even x) then x:z:f z xs
            else x:f z xs
-- f 3 [1,2,3,4,5,6] = [1,2,3,3,4,3,5,6,3]

--3:
semiPareComp :: [Int] -> [Int]
semiPareComp l = [ x `div` 2 | x <- l, even x ]
---
divizori :: Int -> [Int]
divizori n = [d | d <- [1..n], mod n d == 0]

-- divizori 4 == [1,2,4]

--4:
listadiv :: [Int] -> [[Int]]
listadiv lst = [divizori x | x <- lst]

-- listadiv [1,4,6,8] == [[1],[1,2,4],[1,2,3,6],[1,2,4,8]]

--5:
-- inInterval 5 10 [1..15] == [5,6,7,8,9,10]
-- inInterval 5 10 [1,3,5,2,8,-1] == [5,8]

---
--a.:
inIntervalRec :: Int -> Int -> [Int] -> [Int]
inIntervalRec _ _ [] = []
inIntervalRec a b (x:xs) = if(a <= x && x <= b) then x:inIntervalRec a b xs
                            else inIntervalRec a b xs

--b.:
inIntervalComp :: Int -> Int -> [Int] -> [Int]
inIntervalComp a b lst = [x | x <- lst, a <= x && x <= b]

--6:
-- pozitive [0,1,-3,-2,8,-1,6] == 3

--a.:
pozitiveRec :: [Int] -> Int
pozitiveRec [] = 0
pozitiveRec (x:xs) = if(x > 0) then 1 + pozitiveRec xs
                        else pozitiveRec xs

--b.:
pozitiveComp :: [Int] -> Int
pozitiveComp lst = length [x | x <- lst, x > 0]

--7:
-- pozitiiImpare [0,1,-3,-2,8,-1,6,1] == [1,2,5,7]

--a.:
pozitiiImpareRec :: [Int] -> [Int]
pozitiiImpareRec lst = pozitiiAux 0 lst

pozitiiAux :: Int -> [Int] -> [Int]
pozitiiAux _ [] = []
pozitiiAux i (x:xs) = if(odd x) then i:pozitiiAux (i+1) xs
                        else pozitiiAux (i+1) xs

--b.:
pozitiiImpareComp :: [Int] -> [Int]
pozitiiImpareComp lst = [i | (i, x) <- zip [0..] lst, odd x]

--8:
-- multDigits "The time is 4:25" == 40
-- multDigits "No digits here!" == 1

--a.:
multDigitsRec :: String -> Int
multDigitsRec "" = 1
multDigitsRec (x:xs) = if(isDigit x) then digitToInt x * multDigitsRec xs
                        else multDigitsRec xs
--b.:
multDigitsComp :: String -> Int
multDigitsComp str = product [digitToInt c | c <- str, isDigit c]
