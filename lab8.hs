class Collection c where
  empty :: c key value
  singleton :: key -> value -> c key value
  insert
      :: Ord key
      => key -> value -> c key value -> c key value
  clookup :: Ord key => key -> c key value -> Maybe value
  delete :: Ord key => key -> c key value -> c key value
  keys :: c key value -> [key]
  keys c = [fst p | p <- toList c]

  values :: c key value -> [value]
  values v = [snd p | p <- toList v]
  
  toList :: c key value -> [(key, value)]
  
  fromList :: Ord key => [(key,value)] -> c key value
  fromList [] = empty
  fromList ((k, v):xs) = insert k v (fromList xs)

newtype PairList k v
  = PairList { getPairList :: [(k, v)] }

instance Collection PairList where 
    empty = PairList []
    singleton k v = PairList [(k, v)]
    insert k v (PairList l) = PairList $ (k, v):filter((/= k).fst) l
    clookup k = lookup k . getPairList -- getPairList intoarce toate tuplurile
    delete k (PairList l) = PairList $ filter((/=k).fst) l
    --toList (PairList l) = getPairList l

data SearchTree key value
  = Empty
  | BNode
      (SearchTree key value) -- elemente cu cheia mai mica
      key                    -- cheia elementului
      (Maybe value)          -- valoarea elementului
      (SearchTree key value) -- elemente cu cheia mai mare
instance Collection SearchTree where
    empty = Empty
    {-
    insert k v = go
        where go Empty = singleton k v
        go (BNode t1 k1 v1 t2)
            | k == k1 = BNode t1 k (Just v) t2
            | k > k1 = BNode t1 k1 v1 (go t2)
            | otherwise = BNode (go t1) k1 v1 t2
    -} 

data Punct = Pt [Int]

data Arb = Vid | F Int | N Arb Arb
          deriving Show

class ToFromArb a where
 	    toArb :: a -> Arb
	    fromArb :: Arb -> a
-- Pt [1,2,3]
-- (1, 2, 3)

-- Pt []
-- ()

-- toArb (Pt [1,2,3])
-- N (F 1) (N (F 2) (N (F 3) Vid))
-- fromArb $ N (F 1) (N (F 2) (N (F 3) Vid)) :: Punct
--  (1,2,3)
data Geo a = Square a | Rectangle a a | Circle a
    deriving Show

class GeoOps g where
  perimeter :: (Floating a) => g a -> a
  area :: (Floating a) =>  g a -> a

-- ghci> pi
-- 3.141592653589793

--6.:
instance GeoOps Geo where -- !!! Conteaza ordinea fuctiilor
    perimeter (Circle a) = 2 * pi * a
    perimeter (Square a) = 4 * a
    perimeter (Rectangle a b) = 2 * (a + b)

    area (Circle a) = pi * a * a
    area (Square a) = a * a
    area (Rectangle a b) = a * b

--7.:
--instance Geo Eq where
