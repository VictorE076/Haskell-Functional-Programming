-- Ex.1:
sumP :: [Int] -> Int
sumP ls = foldr (+) 0 (map (^2) (filter (\x -> mod x 2 == 1) ls)) 

-- Ex.2:
allT :: [Bool] -> Bool
allT ls = foldr (&&) True ls

-- Ex.3:
allVerifies :: (Int -> Bool) -> [Int] -> Bool
allVerifies f ls = foldr (\x acc -> acc && f x) True ls -- "acc" este acumulator in lambda function

-- Ex.4:
anyVerifies :: (Int -> Bool) -> [Int] -> Bool
anyVerifies f ls = foldr (\x acc -> acc || f x) False ls

-- Ex.5:
mapFoldr :: (a -> b) -> [a] -> [b]
mapFoldr f ls = foldr (\x acc -> f x : acc) [] ls

filterFoldr :: (a -> Bool) -> [a] -> [a]
filterFoldr f ls = foldr (\x acc -> if(f x) 
                                then x : acc
                                else acc) [] ls


-- Ex.6:

-- !!! "foldl" are acumulatorul in stanga
-- !!! "foldr" are acumulatorul in dreapta

listToInt :: [Integer] -> Integer
listToInt ls = foldl (\acc x -> acc * 10 + x) 0 ls 

-- Ex.7:

-- a.:
rmChar :: Char -> String -> String
rmChar c s = filter (/= c) s -- '/=' este '!=' in C++

-- b.:
rmCharsRec :: String -> String -> String
rmCharsRec [] s2 = s2
rmCharsRec (x:s1) s2 = rmChar x (rmCharsRec s1 s2)  

--c.:
rmCharsFold :: String -> String -> String
rmCharsFold s1 s2 = foldr rmChar s2 s1 
    
-- Ex.8:
myReverse :: [Int] -> [Int]
myReverse ls = foldr (\x acc -> acc ++ [x]) [] ls

-- Ex.9:
myElem :: Int -> [Int] -> Bool
myElem a ls = foldr (\x acc -> acc || (a == x)) False ls

-- Ex.10:
myUnzip :: [(a, b)] -> ([a], [b])
myUnzip ls = foldr (\(a, b) (a_acc, b_acc) -> (a : a_acc, b : b_acc)) ([], []) ls

-- Ex.11:
union :: [Int] -> [Int] -> [Int]
union xs ys = foldr (f) e ys
    where 
        f y r = if(elem y xs) then r
                    else r ++ [y]
        e = xs

-- !!! Verifica daca fiecare element din a 2-a lista apartine primei liste
-- Daca DA, nu face nimic, daca NU, il adauga la acumulator
-- Initial, acumulatorul e prima lista

-- Ex.12:
intersect :: [Int] -> [Int] -> [Int]
intersect xs ys = foldr (f) e ys
    where
        f y r = if(elem y xs) then y:r
                    else r
        e = []

-- !!! Acumulatorul e initial lista vida.
-- Verifica pe rand daca fiecare element din a 2-a lista apartine primei liste.
-- Daca DA, il adauga la lista, daca NU, acumulatorul ramane neschimbat.
