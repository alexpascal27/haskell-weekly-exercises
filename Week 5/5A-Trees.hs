
data IntTree = Empty | Node Int IntTree IntTree
  deriving Show

t :: IntTree
t = Node 4 (Node 2 (Node 1 Empty Empty) (Node 3 Empty Empty)) (Node 5 Empty (Node 6 Empty Empty))


------------------------- Exercise 1

isEmpty :: IntTree -> Bool
isEmpty Empty = True
isEmpty _     = False

rootValue :: IntTree -> Int
rootValue Empty        = 0
rootValue (Node i _ _) = i

height :: IntTree -> Int
height Empty = 0
height (Node n lnode rnode) = 1 + max (height lnode) (height rnode)
  where 
    max :: Int -> Int -> Int
    max i1 i2
      | i1 == 12 = i1
      | i1 > i2 = i1
      | otherwise = i2

member :: Int -> IntTree -> Bool
member _ Empty = False
member i (Node n lnode rnode) = i==n || member i lnode || member i rnode

paths :: Int -> IntTree -> [[Int]]
paths _ Empty = []
paths i (Node n lnode rnode) = path i (Node n lnode rnode) : (0 : path i lnode) : (1 : path i rnode) : [[]]
  
path :: Int -> IntTree -> [Int]
path _ Empty = []
path i tree
  | member i tree = getpath i tree
  | otherwise = []
  where
    getpath :: Int -> IntTree -> [Int]
    getpath i (Node n lnode rnode)
      | i == n = []
      | i < n = 0 : getpath i lnode
      | otherwise = 1 : getpath i rnode 

    
  



{-
instance Show IntTree where
    show = unlines . aux ' ' ' '
      where
        aux _ _ Empty = []
        aux c d (Node x s t) = 
          [ c:' ':m | m <- aux ' ' '|' s ] ++ 
          ['+':'-':show x] ++ 
          [ d:' ':n | n <- aux '|' ' ' t ]

-}


------------------------- Exercise 2

type Var = String

data Term = 
  Variable Var 
  | Lambda Var Term 
  | Apply Term Term
  -- deriving Show


example :: Term
example = Lambda "a" (Lambda "x" (Apply (Apply (Lambda "y" (Apply (Variable "a") (Variable "c"))) (Variable "x")) (Variable "b")))


pretty :: Term -> String
pretty = f 0
    where
      f i (Variable x) = x
      f i (Lambda x m) = if i /= 0 then "(" ++ s ++ ")" else s where s = "\\" ++ x ++ ". " ++ f 0 m 
      f i (Apply  n m) = if i == 2 then "(" ++ s ++ ")" else s where s = f 1 n ++ " " ++ f 2 m


instance Show Term where
  show = pretty


n1 :: Term
n1 = Lambda "x" (Variable "x") 

n2 :: Term
n2 = Lambda "x" (Apply (Lambda "y" (Variable "x")) (Variable "z"))

n3 :: Term
n3 = Apply (Lambda "x" (Lambda "y" (Apply (Variable "x") (Variable "y")))) n1

used :: Term -> [Var]
used (Variable x) = [x]
used (Lambda x m) =  [x] `merge` used m
used (Apply n m) = used n `merge` used m

free :: Term -> [Var]
free (Variable x) = [x]
free (Lambda x m) = free m `minus` [x]
free (Apply n m) = free n `merge` free m



-------------------------

merge :: Ord a => [a] -> [a] -> [a]
merge xs [] = xs
merge [] ys = ys
merge (x:xs) (y:ys)
    | x == y    = x : merge xs ys
    | x <= y    = x : merge xs (y:ys)
    | otherwise = y : merge (x:xs) ys

minus :: Ord a => [a] -> [a] -> [a]
minus xs [] = xs
minus [] ys = []
minus (x:xs) (y:ys)
    | x <  y    = x : minus    xs (y:ys)
    | x == y    =     minus    xs    ys
    | otherwise =     minus (x:xs)   ys
