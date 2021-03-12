

------------------------- Exercise 1

square :: Int -> Int
square x = x*x

pythagoras :: Int -> Int -> Int -> Bool
pythagoras a b c = square a + square b == square c


------------------------- Exercise 2

factorial :: Int -> Int
factorial x
    | x <= 1    = 1
    | otherwise = x * factorial (x-1)

euclid :: Int -> Int -> Int
euclid x y
    | x < 0 = error "entered negative x number"
    | y < 0 = error "entered negative y number"
    | x == 0 = error "entered zero x number"
    | y == 0 = error "entered zero y number"
    | x == y = x
    | x <  y = euclid x (y-x)
    | x >  y = euclid y (x-y)

power :: Int -> Int -> Int
power x y
    | y < 0 = error "negative power"
    | y == 0 = 1
    | y == 1 = x
    | otherwise = x * power x (y-1)


------------------------- Exercise 3

range :: Int -> Int -> [Int]
range n m
    | n > m = error "start of range is bigger than end of range"
    | n == m = [m]
    | m > n =  n : range (n+1) m

times :: [Int] -> Int
times [] = 1
times (x : xs) = x * times xs

--note: you will need to create your own pattern-matching

fact :: Int -> Int
fact x
    | x < 0 = error "netagive input number"
    | x == 0 = 1
    | otherwise = times (range 1 x)