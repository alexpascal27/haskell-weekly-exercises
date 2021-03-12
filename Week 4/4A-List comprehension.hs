

------------------------- Exercise 1

doubles :: [Int] -> [Int]
doubles [] = []
doubles xs = [(* 2) x | x <- xs]

odds :: [Int] -> [Int]
odds [] = []
odds xs = [x | x <- xs, odd x]

doubleodds :: [Int] -> [Int]
doubleodds [] = []
doubleodds xs = [(* 2) x | x <- xs, odd x]

shorts :: [String] -> [String]
shorts [] = []
shorts xs = [x | x <- xs, islengthlessthan6 x]
    where 
            islengthlessthan6 :: String -> Bool
            islengthlessthan6 x = length x < 6

squarePositives :: [Int] -> [Int]
squarePositives [] = []
squarePositives xs = [(^ 2) x | x <- xs, ispositive x]
    where 
        ispositive :: Int -> Bool
        ispositive x = x > 0

oddLengthSums :: [[Int]] -> [Int]
oddLengthSums [] = []
oddLengthSums xs = [sum x| x <- xs, oddlength x]
    where
        oddlength :: [Int] -> Bool
        oddlength [] = False 
        oddlength xs = odd (length xs)

remove :: Eq a => [a] -> a -> [a]
remove [] _ = []
remove xs y = [x | x <- xs, (noty y) x]
    where 
        noty :: Eq a => a -> a -> Bool
        noty x y = x /= y

removeAll :: Eq a => [a] -> [a] -> [a]
removeAll xs [] = xs
removeAll [] _ = []
removeAll xs (y: ys) = removeAll (remove xs y) ys 

everyother :: [a] -> [a]
everyother [] = []
everyother xs = [snd x | x <- zip [1..] xs, p x]
    where
        p(x , _) =  odd x

same :: Eq a => [a] -> [a] -> [Int]
same xs ys =  [fst x | x <- (zip [1..] (zipWith (==) xs ys)), p x]
    where
        p(_, y) = y


------------------------- Exercise 2

pairs :: [a] -> [b] -> [(a,b)]
pairs = undefined

selfpairs :: [a] -> [(a,a)]
selfpairs = undefined

pyts :: Int -> [(Int,Int,Int)]
pyts = undefined

