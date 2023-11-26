{-
[x^2 |x <- [1..10], x `rem` 3 == 2]  ('rem' = mod) - [4, 25, 64]
[(x,y) | x <- [1..5], y <- [x..(x+2)]] - [(1, 1), (1, 2), (1, 3), (2, 2), (2, 3), (2, 4), ..., (5, 5), (5, 6), (5, 7)]
[(x,y) | x <- [1..3], let k = x^2, y <- [1..k]] - [(1, 1), (2, 1), (2, 2), (2, 3), (2, 4), (3, 1), ..., (3, 9)]
[x | x <- "Facultatea de Matematica si Informatica", elem x ['A'..'Z']] - "FMI"
[[x..y] | x <- [1..5], y <- [1..5], x < y] - [[1, 2], [1, 2, 3], [1, 2, 3, 4], [1, 2, 3, 4, 5], [2, 3], ..., [4, 5]]
-}

--Ex.2:
factori :: Int -> [Int]
factori n = [d | d <-[1..n], mod n d == 0]

--Ex.3:
prim :: Int -> Bool
prim n = length(factori n) == 2

--Ex.4:
numerePrime :: Int -> [Int]
numerePrime n = [x | x <- [2..n], prim x]

--Ex.5:
myzip3 :: [Int] -> [Int] -> [Int] -> [(Int, Int, Int)]
myzip3 l1 l2 l3 = [(a, b, c) | (a, (b, c)) <- zip l1 (zip l2 l3)]

--Ex.6:
firstEl :: [(a, b)] -> [a]
firstEl ls = map (fst) ls -- 'fst' first element in a tuple

--Ex.7:
sumList :: [[Int]] -> [Int]
sumList ls = map sum ls -- 'sum' suma elementelor unei liste de intregi

--Ex.8:
prel2 :: [Int] -> [Int]
prel2 ls = map (\x -> if even x then div x 2 else 2 * x) ls

--Ex.9:
func9 :: Char -> [String] -> [String]
func9 c ls = filter (elem c) ls 

--Ex.10:
func10 :: [Int] -> [Int]
func10 ls = map (\a -> a * a) (filter odd ls)

--Ex.11:
func11 :: [Int] -> [Int]
func11 ls = map (\x -> x * x) [x | (x, y) <- zip ls [0..], odd y]

--Ex.12:
numaiVocale :: [String] -> [String]
numaiVocale ls = map (\x -> filter (\a -> elem a "aeiouAEIOU") x) ls

--Ex.13:
mymap :: (a -> b) -> [a] -> [b]
mymap _ [] = []
mymap f (x:xs) = f x : mymap f xs 

myfilter :: (a -> Bool) -> [a] -> [a]
myfilter _ [] = []
myfilter f (x:xs) = if (f x == True) then x : myfilter f xs -- !!! concat x la lista "myfilter f xs"
                    else myfilter f xs

ordonataNat :: [Int] -> Bool
ordonataNat [] = True
ordonataNat [x] = True
ordonataNat (x:xs) = undefined
ordonata :: [a] -> (a -> a -> Bool) -> Bool
ordonata = undefined
