
------------------------- Exercise 1

doublesrecursion :: [Int] -> [Int]
doublesrecursion [] = []
doublesrecursion (x : xs) = (2 * x) : doublesrecursion xs   

doubles :: [Int] -> [Int]
doubles [] = []
doubles xs = map (* 2) xs 

oddsrecursion :: [Int] -> [Int]
oddsrecursion [] = []
oddsrecursion (x : xs)
    -- even (not odd)
    | even x = oddsrecursion xs
    | otherwise = x : oddsrecursion xs


odds :: [Int] -> [Int]
odds [] = []
odds xs = filter odd xs

doubleodds :: [Int] -> [Int]
doubleodds [] = []
doubleodds xs = doubles (odds xs)


------------------------- Exercise 2

shorts :: [String] -> [String]
shorts [] = []
shorts xs = filter islengthlessthan6 xs
    where 
        islengthlessthan6 :: String -> Bool
        islengthlessthan6 x = length x < 6

squarePositives :: [Int] -> [Int]
squarePositives [] = []
squarePositives xs = map (^ 2) (filter ispositive xs)
    where 
        ispositive :: Int -> Bool
        ispositive x = x > 0

oddLengthSums :: [[Int]] -> [Int]
oddLengthSums [] = []
oddLengthSums xs =  map sum (filter oddlength xs)
    where
        oddlength :: [Int] -> Bool
        oddlength [] = False 
        oddlength xs = odd (length xs)


------------------------- Exercise 3

remove :: Eq a => [a] -> a -> [a]
remove [] _ = []
remove xs y = filter (noty y) xs 
    where 
        noty :: Eq a => a -> a -> Bool
        noty x y = not(x == y)

removeAll :: Eq a => [a] -> [a] -> [a]
removeAll xs [] = xs
removeAll [] _ = []
removeAll xs (y : ys) = removeAll (remove xs y) ys

numbered :: [a] -> [(Int,a)]
numbered [] = []
numbered xs = zip [1..] xs

everyother :: [a] -> [a]
everyother [] = []
everyother xs = map snd (filter p (zip [1..] xs))
    where
        p(x , _) =  odd x

same :: Eq a => [a] -> [a] -> [Int]
same xs ys = map fst (filter p (zip [1..] (zipWith (==) xs ys)))
    where
        p(_, y) = y
