
data Tree a = Empty | Node a (Tree a) (Tree a)

t :: Num a =>  (Tree a)
t = Node 4 (Node 2 (Node 1 Empty Empty) (Node 3 Empty Empty)) (Node 5 Empty (Node 6 Empty Empty))

t1 :: Num a => (Tree a)
t1 = Node 4 (Node 2 (Node 1 Empty Empty) (Node 3 Empty Empty)) (Node 5 Empty (Node 0 Empty Empty))

isEmpty :: Tree a -> Bool
isEmpty Empty = True
isEmpty _     = False

------------------------- Exercise 1

member :: Ord a => a -> Tree a -> Bool
member _ Empty = False
member i (Node n lnode rnode)
  | i==n = True 
  | i < n = member i lnode
  | otherwise = member i rnode

largest :: Ord a => Tree a -> a
largest Empty            = error "largest: empty tree"
largest (Node x l Empty) = x
largest (Node x l r)     = largest r

ordered :: Ord a => Tree a -> Bool
ordered Empty = True 
ordered (Node x Empty Empty) = True
ordered (Node x Empty (Node rx rl rr)) = rx > x && ordered(Node rx rl rr)
ordered (Node x (Node lx ll lr) Empty) = lx < x && ordered(Node lx ll lr)
ordered (Node x (Node lx ll lr) (Node rx rl rr)) = lx < x && rx > x && lx < rx && ordered(Node lx ll lr) && ordered(Node rx rl rr)

deleteLargest :: Tree a -> Tree a
deleteLargest Empty            = Empty
-- found largest
deleteLargest (Node x l Empty) = l
deleteLargest (Node x l r)     = Node x l (deleteLargest r)

delete :: Ord a => a -> Tree a -> Tree a
delete _ Empty = Empty
delete y (Node x l r)
    -- go left
    | y < x     = Node x (delete y l) r
    -- go right
    | y > x     = Node x l (delete y r)
    -- same as below but easy delete
    | isEmpty l = r
    -- harder delete
    | otherwise = Node (largest l) (deleteLargest l) r


------------------------- Exercise 2



instance Show a => Show (Tree a) where
    show = unlines . aux ' ' ' '
      where
        aux _ _ Empty = []
        aux c d (Node x s t) = 
          [ c:' ':m | m <- aux ' ' '|' s ] ++ 
          ['+':'-':show x] ++ 
          [ d:' ':n | n <- aux '|' ' ' t ]




------------------------- Lambda-calculus

type Var = String

data Term =
    Variable Var
  | Lambda   Var  Term
  | Apply    Term Term

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

-------------------------

used :: Term -> [Var]
used (Variable x) = [x]
used (Lambda x n) = [x] `merge` used n
used (Apply  n m) = used n `merge` used m

free :: Term -> [Var]
free (Variable x) = [x]
free (Lambda x n) = free n `minus` [x]
free (Apply  n m) = free n `merge` free m


------------------------- Exercise 3

numeral :: Int -> Term
numeral x
  | x < 0 = error "negative input"
  | otherwise = Lambda "f" (Lambda "x" (churchNumeral x)) 
    where 
      churchNumeral :: Int -> Term
      churchNumeral x
        | x == 0 = Variable "x"
        -- x>0
        | otherwise = Apply (Variable "f") (churchNumeral (x-1))


------------------------- Exercise 4

alphabet :: [String]
alphabet = ["a","b", "c", "d", "e", "f", "g", "h", "i", "j", "k", "l", "m", "n", "o", "p", "q", "r", "s", "t", "u", "v", "w", "x", "y", "z"]

variables :: [Var]
variables = alphabeticalCount 0 0
  where 
    alphabeticalCount :: Int -> Int -> [Var]
    alphabeticalCount letter n
      | n == 0 && letter < 25 = alphabet!!letter : alphabeticalCount (letter+1) n
      | n == 0 && letter == 25 = alphabet!!letter : alphabeticalCount 0 (n+1)
      | letter == 25 = (alphabet!!letter ++ show n) : alphabeticalCount 0 (n+1)
      | otherwise = ((alphabet!!letter) ++ show n) : alphabeticalCount (letter+1) n

fresh :: [Var] -> Var
fresh [] = ""
fresh xs = fresh' xs variables
  where 
  fresh' (x : xs) (y :ys)
    | x == y = fresh' xs ys
    | otherwise = y

-- var to replace -> -- new var -> term that we need to change -> new term
rename :: Var -> Var -> Term -> Term
rename x y (Variable z)
    | z == x    = Variable y
    | otherwise = Variable z
rename x y (Lambda z n)
    | z == x    = Lambda z n
    | otherwise = Lambda z (rename x y n)
rename x y (Apply n m) = Apply (rename x y n) (rename x y m) 

substitute :: Var -> Term -> Term -> Term
substitute x n (Variable y)
    | x == y    = n
    | otherwise = Variable y
substitute x n (Lambda y m)
    | x == y    = Lambda y m
    | otherwise = Lambda z (substitute x n (rename y z m))
    where z = fresh (used n `merge` used m `merge` [x,y])
substitute x n (Apply m p) = Apply (substitute x n m) (substitute x n p)
