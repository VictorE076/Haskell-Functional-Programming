import Data.Char
{- Comentariu 
pe mai multe 
linii! -}

-- Comentariu pe o singura linie!

--ff :: Int -> Int
--ff x = x * 2 

-- Exercitii pentru partial!
--Ex.1:

{-
f :: Int -> Int
f X = X + Y
    where Y = X + 1
-}

-- Raspuns: b. eroare (Variabile si functii doar cu litere mici, iar constructorii de date sunt doar cu litere mari)

--Ex.2:

{-
filter (\(x, y) -> x == y) [('a', 'a'), ('b', 'c'), ('d', 'a')]
-}

-- Raspuns: d. [(Char, Char)] (tipul functiei)

--Ex.3:

{-
map minimum [[1..10], [10..20], [20..30]]
-}

-- Raspuns: a. [1, 10, 20] (Lista mare va contine valoarea minima a fiecarei liste mici din interiorul celei mari)

--Ex.4:

{-
l1 = [2, 6..]
l2 = [10, 20..]
l3 = zip l1 l2
x = head $ tail l3
-}

-- Raspuns: a. (6, 20) (l3 = zip l1 l2 = [(2, 10), (6, 20)..])

--Ex.5:

{-
f :: Integer -> Integer
f x 
    | x > 0 = x + 1
    | otherwise = x - 1

f 0 = 0
-}

-- Raspuns: d. -1 (f x este evaluat intai deoarece ordinea conteaza, deci f 0 = 0 - 1 = -1)

--Ex.6:

{-

-}

-- Raspuns: 

--Ex.7:

{-
g :: Int -> Int 
g y = let f y = y + 1 in f y
-}

-- Raspuns: a. 6 (g 5 = 5 + 1 = 6)

--Ex.8:

{-
x = 2
ff y = x + y
        where x = 3
-}

-- Raspuns: d. 7 (ff 4 = 3 + 4, x-ul este local in ff !!!)

--Ex.9:

-- Raspuns:

--Ex.10:

-- Raspuns: c. filter isSpace (Tipul functiei este [Char] -> [Char], string-ul care lipseste + tipul returnat de functia filter)

--Ex.11:

-- Raspuns: c. filter isUpper xs (selecteaza literele mari dintr-un sir de caractere)

--Ex.12:

-- Raspuns: 

--Ex.13:

{-
f = foldl (\i j -> 1 + i) 0
-}

-- Raspuns: c. definitie incorecta (lipseste al treilea parametru din functia foldl)

--Ex.14:

{-
l1 = [1, 2, 3, 4, 5]
l2 = [2, 3..]
l3 = ['a'..'f']
l4 = zip (zip l1 l2) l3
-}

-- Raspuns: b. [((1, 2), 'a'), ((2, 3), 'b'), ((3, 4), 'c'), ((4, 5), 'd'), ((5, 6), 'e')] (valoarea lui l4)

--Ex.15:

-- Raspuns: b. foldr (&&) True [1 > 2, 3 > 2, 5 == 5] (produce valoarea "False", 1 > 2 da "False")

--Ex.16:

{-
f :: (String -> Int) -> (Int -> String) -> String -> Int 
f = undefined
-}

-- Raspuns: b. Nu se poate aplica functia uncurry peste peste f (format nepotrivit !!!)


-- Lab.6:
data Fruct
  = Mar String Bool
  | Portocala String Int

ionatanFaraVierme = Mar "Ionatan" False
goldenCuVierme = Mar "Golden Delicious" True
portocalaSicilia10 = Portocala "Sanguinello" 10
listaFructe = [Mar "Ionatan" False,
                Portocala "Sanguinello" 10,
                Portocala "Valencia" 22,
                Mar "Golden Delicious" True,
                Portocala "Sanguinello" 15,
                Portocala "Moro" 12,
                Portocala "Tarocco" 3,
                Portocala "Moro" 12,
                Portocala "Valencia" 2,
                Mar "Golden Delicious" False,
                Mar "Golden" False,
                Mar "Golden" True]

--a.:
ePortocalaDeSicilia :: Fruct -> Bool
ePortocalaDeSicilia = undefined
test_ePortocalaDeSicilia1 =
    ePortocalaDeSicilia (Portocala "Moro" 12) == True
test_ePortocalaDeSicilia2 =
    ePortocalaDeSicilia (Mar "Ionatan" True) == False
--b:
nrFeliiSicilia :: [Fruct] -> Int
nrFeliiSicilia = undefined

test_nrFeliiSicilia = nrFeliiSicilia listaFructe == 52

--c.:
nrMereViermi :: [Fruct] -> Int
nrMereViermi = undefined
--nrMereViermi = length[b | Mar s b <- Fruct, b]

test_nrMereViermi = nrMereViermi listaFructe == 2

type NumeA = String
type Rasa = String
data Animal = Pisica NumeA | Caine NumeA Rasa
    deriving Show


--2.:
--a.:
vorbeste :: Animal -> String
vorbeste (Pisica _) = "Meow!"
vorbeste (Caine _ _) = "Woof!"

--b.:
rasa :: Animal -> Maybe String
rasa (Caine _ r) = Just r
rasa (Caine _ "") = Nothing 

data Linie = L [Int]
   deriving Show
data Matrice = M [Linie]
   deriving Show

--3.:
--a.:
verifica :: Matrice -> Int -> Bool
verifica (M lista) n = foldr (&&) True (map(\(L l) -> sum l == n) lista)
test_verif1 = verifica (M[L[1,2,3], L[4,5], L[2,3,6,8], L[8,5,3]]) 10 == False
test_verif2 = verifica (M[L[2,20,3], L[4,21], L[2,3,6,8,6], L[8,5,3,9]]) 25 == True

--b.:
{-
doarPozN :: Matrice -> Int -> Bool
doarPozN (M lista) n = foldr (&&) True (map p l)
                    where l = filter(\(L e1) -> length l1 == n) lista
                     p (L l) = l1 == filter (> 0) l1

testPoz1 = doarPozN (M [L[1,2,3], L[4,5], L[2,3,6,8], L[8,5,3]]) 3 == True

testPoz2 = doarPozN (M [L[1,2,-3], L[4,5], L[2,3,6,8], L[8,5,3]]) 3 == False
-}
--c.:
-- !!! Constructorul este mereu intre paranteze!
corect :: Matrice -> Bool
corect (M []) = True
corect (M [x]) = True
corect (M ((L l1):(L l2):list)) = length l1 == length l2 && corect (M ((L l2):list))

testcorect1 = corect (M[L[1,2,3], L[4,5], L[2,3,6,8], L[8,5,3]]) == False
testcorect2 = corect (M[L[1,2,3], L[4,5,8], L[3,6,8], L[8,5,3]]) == True
