data Expr = Const Int -- integer constant
          | Expr :+: Expr -- addition
          | Expr :*: Expr -- multiplication
           deriving Eq

data Operation = Add | Mult deriving (Eq, Show)

data Tree = Lf Int -- leaf
          | Node Operation Tree Tree -- branch
           deriving (Eq, Show)
           
instance Show Expr where
  show (Const x) = show x
  show (e1 :+: e2) = "(" ++ show e1 ++ " + "++ show e2 ++ ")"
  show (e1 :*: e2) = "(" ++ show e1 ++ " * "++ show e2 ++ ")"           

evalExp :: Expr -> Int
evalExp = undefined

exp1 = ((Const 2 :*: Const 3) :+: (Const 0 :*: Const 5))
exp2 = (Const 2 :*: (Const 3 :+: Const 4))
exp3 = (Const 4 :+: (Const 3 :*: Const 3))
exp4 = (((Const 1 :*: Const 2) :*: (Const 3 :+: Const 1)) :*: Const 2)
test11 = evalExp exp1 == 6
test12 = evalExp exp2 == 14
test13 = evalExp exp3 == 13
test14 = evalExp exp4 == 16



-- Ex.2:
evalArb :: Tree -> Int
evalArb (Lf l) = l
evalArb (Node Add t1 t2) = evalArb t1 + evalArb t2
evalArb (Node Mult t1 t2) = evalArb t1 * evalArb t2

arb1 = Node Add (Node Mult (Lf 2) (Lf 3)) (Node Mult (Lf 0)(Lf 5))
arb2 = Node Mult (Lf 2) (Node Add (Lf 3)(Lf 4))
arb3 = Node Add (Lf 4) (Node Mult (Lf 3)(Lf 3))
arb4 = Node Mult (Node Mult (Node Mult (Lf 1) (Lf 2)) (Node Add (Lf 3)(Lf 1))) (Lf 2)

test21 = evalArb arb1 == 6
test22 = evalArb arb2 == 14
test23 = evalArb arb3 == 13
test24 = evalArb arb4 == 16

--Ex.3:

expToArb :: Expr -> Tree
expToArb (Const a) = Lf a
expToArb (e1 :+: e2) = Node Add (expToArb e1) (expToArb e2)
expToArb (e1 :*: e2) = Node Mult (expToArb e1) (expToArb e2) 

-- Ex.4:
data IntSearchTree value
  = Empty
  | BNode
      (IntSearchTree value)     -- elemente cu cheia mai mica
      Int                       -- cheia elementului
      (Maybe value)             -- valoarea elementului
      (IntSearchTree value)     -- elemente cu cheia mai mare



{-
lookup' :: Int -> IntSearchTree value -> Maybe value
lookup' _ Empty = Nothing 
lookup' key (BNode lt ckey)
     | Key == ckey = valoarea
     | Key < ckey = lookup' key lt
-}

--Ex.5:


keys ::  IntSearchTree value -> [Int]
keys Empty = []
keys (BNode lt ckey val rt) = keys lt ++ [ckey] ++ keys rt


--Ex.6:
values :: IntSearchTree value -> [Maybe value]
values Empty = []
values (BNode lt ckey val rt) = values lt ++ [val] ++ values rt


--Ex.7:
insert :: Int -> value -> IntSearchTree value -> IntSearchTree value
insert key val Empty = BNode Empty key (Just val) Empty
insert key vall (BNode lt ckey val rt) = if (key < ckey) then BNode (insert key vall lt) ckey val rt
                                                          else if (key > ckey) 
                                                            then BNode lt ckey val (insert key vall rt) 
                                                          else BNode lt ckey (Just vall) rt      


--Ex.8:
delete :: Int -> IntSearchTree value -> IntSearchTree value
delete key Empty = BNode Empty key Nothing Empty
delete key (BNode lt ckey val rt) = if (key < ckey) then BNode (delete key lt) ckey val rt
                                    else if (key > ckey) then BNode lt ckey val (delete key rt)
                                    else BNode lt ckey Nothing rt

--Ex.9:
toList :: IntSearchTree value -> [(Int, value)]
toList Empty = []
toList (BNode lt ckey (Just val) rt) = toList lt ++ [(ckey, val)] ++ toList rt
toList (BNode lt ckey Nothing rt) = toList lt ++ toList rt 


--Ex.10:
fromList :: [(Int,value)] -> IntSearchTree value 
fromList = undefined

printTree :: IntSearchTree value -> String
printTree = undefined

-- balance :: IntSearchTree value -> IntSearchTree value
-- balance = undefined