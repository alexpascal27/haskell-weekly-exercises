
------------------------- Exercise 1

{-
doubles :: [Int] -> [Int]
doubles    []  = [] 
doubles (x:xs) = 2*x : doubles xs

odds :: [Int] -> [Int]
odds [] = []
odds (x:xs)
    | odd x     = x : odds xs
    | otherwise =     odds xs

doubleodds :: [Int] -> [Int]
doubleodds [] = []
doubleodds (x:xs)
    | odd x     = 2*x : doubleodds xs
    | otherwise =       doubleodds xs

-}

double :: Int -> Int
double x = 2 * x

doubles :: [Int] -> [Int]
doubles = map double

odds :: [Int] -> [Int]
odds = filter odd

doubleodds :: [Int] -> [Int]
doubleodds = map double . odds


------------------------- Exercise 2

shorts :: [String] -> [String]
shorts = filter f
  where
    f x = length x <= 5

squarePositives :: [Int] -> [Int]
squarePositives = map f . filter p
  where
    f x = x * x
    p x = 0 < x

oddLengthSums :: [[Int]] -> [Int]
oddLengthSums = map sum . filter p
  where
    p xs = odd (length xs)

{-
shorts = filter ((<= 5) . length)

squarePositives = map (\x -> x*x) . filter (0<)

oddLengthSums = map sum . filter (odd . length)
-}

------------------------- Exercise 3

remove :: Eq a => [a] -> a -> [a]
remove xs y = filter p xs
  where
    p x = y /= x

removeAll :: Eq a => [a] -> [a] -> [a]
removeAll xs ys = filter p xs
  where
    p x = not (elem x ys)

numbered :: [a] -> [(Int,a)]
numbered xs = zip [1..] xs

everyother :: [a] -> [a]
everyother xs = map snd (filter p (zip [1..] xs))
  where
    p (i,_) = odd i

same :: Eq a => [a] -> [a] -> [Int]
same xs ys = map fst (filter snd (zip [1..] (zipWith (==) xs ys)))

{-
remove xs y = filter (/=y) xs

removeAll xs ys = filter (not . (`elem` ys)) xs

everyother = map snd . filter (odd . fst) . numbered

same xs = map fst . filter snd . zip [1..] . zipWith (==) xs
-}
