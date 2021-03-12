

ladies    = ["Mary","Kitty","Lydia","Elizabeth","Jane"]
gentlemen = ["Charles","Fitzwilliam","George","William"]
couples   = [("Elizabeth","Fitzwilliam"),("Charlotte","William"),("Lydia","George"),("Jane","Charles")]

------------------------- Exercise 1


ditch :: Int -> [a] -> [a]
ditch _ [] = []
ditch n (x:xs)
  | n < 0 = error "negative input"
  | n > 0 = ditch (n-1) xs
  |otherwise = x:xs

at :: [a] -> Int -> a
at [] _ = error "empty list"
at (x:xs) n
  | n < 0 = error "negative input"
  | n > 0 = at xs (n-1)
  | otherwise = x


------------------------- Exercise 2

find :: Eq a => a -> [(a,b)] -> b
find _ [] = error "empty list"
find key (x:xs)
  | key == fst x = snd x
  | otherwise = find key xs

which :: Eq a => a -> [a] -> Int
which = aux 0
  where
    aux :: Eq a => Int -> a -> [a] -> Int
    aux _ _ [] = error "empty list"
    aux i object (x : xs)
      -- have we found the object
      | object == x = i
      -- not yet found it
      | otherwise = aux (i+1) object xs


-- 2A
member :: Eq a => [a] -> a -> Bool
member    []  _ = False
member (x:xs) y
    | x == y    = True
    | otherwise = member xs y

remove :: Eq a => [a] -> a -> [a]
remove [] _ = []
remove (x:xs) y 
    | x == y = xs
    | otherwise = x : remove xs y


before :: Ord a => a -> a -> Bool
before x y
    | x < y = True
    | otherwise = False

sorted :: Ord a => [a] -> Bool
sorted [] = True 
sorted [x] = True
sorted (x:xs:xss) = before x xs && sorted (xs:xss)

------------------------- Exercise 3

merge :: Ord a => [a] -> [a] -> [a]
merge xs [] = xs
merge [] ys = ys
merge (x:xs) (y:ys)
  | before x y = x : merge xs (y:ys) 
  | before y x = y : merge (x:xs) ys
  | otherwise = x: (y: merge xs ys)


minus :: Ord a => [a] -> [a] -> [a]
minus x [] = x
minus [] x  = error "can't remove from empty list"
minus xs (y:ys)
  | member xs y = minus (remove xs y) ys
  | otherwise  = minus xs ys

msort :: Ord a => [a] -> [a]
msort [] = []
msort xs
  | length xs == 1 = xs
  | otherwise =  merge (msort (fst div xs) msort (snd div xs))
