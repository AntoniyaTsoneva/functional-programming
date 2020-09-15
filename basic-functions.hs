------------------Learning Haskell-------------------------

------------------List-Comprehension-----------------------
setEvens = [x*2 | x <- [1..10]]
setEvensAboveTen = [x*2 | x <- [1..10], x*2 >= 12]

boomBangs xs = [ if x < 10 then "BOOM!" else "BANG!" | x <- xs, odd x]

production = [ x*y | x <- [2,5,10], y <- [8,10,11]] 

removeNonUppercase list = [ c | c <- list, c `elem` ['A'..'Z']]

triangles = [ (a,b,c) | c <- [1..10], b <- [1..10], a <- [1..10] ]
rightTriangles = [ (a,b,c) | c <- [1..10], b <- [1..c], a <- [1..b], a^2 + b^2 == c^2]


-------------------Matching-Pattern------------------------
--factorial
fact' :: (Eq t, Num t) => t -> t
fact' 0 = 1
fact' n = n * fact' (n -1)


------------------------Pattern----------------------------
firstLetter' :: String -> String  
firstLetter' "" = "empty"  
firstLetter' all@(x:xs) = "The first letter of " ++ all ++ " is " ++ [x] 


-------------------------Guards----------------------------
--max
max' a b
   | a > b     = a
   | otherwise = b
   
--compare
compare' :: (Ord a) => a -> a -> Ordering  
compare' a b
   | a > b  = GT
   | a == b = EQ
   | a < b  = LT


---------------------Let-Bindings---------------------------
cylinder :: (RealFloat a) => a -> a -> a  
cylinder r h = 
    let sideArea = 2 * pi * r * h  
        topArea = pi * r ^2  
    in  sideArea + 2 * topArea 


---------------------Case-Expressions------------------------
describeList :: [a] -> String  
describeList xs = "The list is " ++ what xs  
    where what []  = "empty."  
          what [x] = "a singleton list."  
          what xs  = "a longer list."  


---------------------Curried-Functions----------------------
divideByTen :: (Floating a) => a -> a  
divideByTen = (/10) 
--divideByTen 100  ~  100/10  ~  (/10) 200


---------------------High-order-Functions-------------------
applyTwice :: (a -> a) -> a -> a
applyTwice f x = f (f x)

--zipWith
zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c] 
zipWith' _ [] _ = []
zipWith' _ _ [] = []
zipWith' f (x:xs) (y:ys) = (f x y): zipWith' f xs ys

--flip
flip' :: (a -> b -> c) -> b -> a -> c  
flip' f y x = f x y 


----------------------Lambdas-\-----------------------------

--flip with Lambda
flip'' f = \x y -> f y x

----------------------Function-Application------------------

-- $ function has the lowest precedence
-- sqrt (3 + 4 + 9)  === sqrt $ 3 + 4 + 9

----------------------Function-Composition------------------

getNegativeNumbers = map (negate . abs) 

---------------------Basic-Functions------------------------
--sum
sum' :: Num t => [t] -> t
sum' [] = 0
sum' (x:xs) = x + sum' xs

--length
length' :: Num t => [t1] -> t
length' [] = 0
length' (_:xs) = 1 + lenght' xs

--head
head' :: [a] -> a
head' [] = error "error: empty list."
head' (x:_) = x

--tail
tail' :: [a] -> [a]
tail' [] = error "error: empty list."
tail' (_:xs) = xs

--maximum
maximum' []  = error "error: empty list."
maximum' [x] = x
maximum' (x:xs) = max' x (maximum' xs)

--replicate
replicate' :: (Num i, Ord i) => i -> a -> [a]  
replicate' n x
        | n <= 0 = []
        | otherwise = x:replicate' (n-1) x

--take
take' _ [] = []
take' n _
    | n <= 0 = []
take' n (x:xs) = x:take' (n-1) xs

--reverse
reverse' [] = []
reverse' (x:xs) = (reverse' xs) ++ [x]

--repeat
repeat' elem = elem:repeat' elem

--zip
zip' [] _ = []
zip' _ [] = []
zip' (x:xs) (y:ys) = (x,y):zip' xs ys

--elem
elem' x [] = False
elem' x (y:ys)
    | x == y = True
    | x /= y = elem' x ys

--quicksort with list comprehension
quickSort' :: (Ord a) => [a] -> [a]
quickSort' [] = []
quickSort' (x:xs) = 
    let smaller = quickSort' [ a | a <- xs, a <= x] 
        bigger  = quickSort' [ a | a <- xs, a > x]  
    in smaller ++ [x] ++ bigger

--[5,1,9,4,6,7,3]
--         [1,4,3]    ++ [5]   ++   [9,6,7]
-- [] ++ [1] ++ [4,3] ++ [5] ++ [6,7] ++ [9] ++ []
--       [3] ++ [4] ++ []       [] ++ [6] ++ [7]
-- [] ++ [3] ++ []                     [] ++ [7] ++ []



--map
map' _ [] = []
map' f (x:xs) = f x : map' f xs
-- map (+3) [1,5,3,1,6]  === [x+3 | x <- [1,5,3,1,6]]

--filter
filter' _ [] = []
filter' p (x:xs)
    | p x == True = x : filter' p xs
    | otherwise   = filter' p xs


--qicksort with filter
quickSort'' [] = []
quickSort'' (x:xs) = 
    let smaller' = quickSort'' (filter' (<=x) xs)
        bigger'  = quickSort'' (filter' (>x) xs)
    in smaller' ++ [x] ++ bigger'



--takeWhile
takeWhile' _ [] = []
takeWhile' p (x:xs)
    | p x == True = x: takeWhile' p xs
    | otherwise   = []


sum'' :: (Num a) => [a] -> a  
sum'' = foldl (+) 0 
--sum  xs = foldl (\acc x -> acc + x) 0 xs

elem'' y ys = foldl (\acc x -> if x == y then True else acc) False ys 

map'' f xs = foldr (\x acc -> f x :acc ) [] xs

-- reverse'' :: [a] -> [a]  
-- reverse'' = foldl (\x acc -> x:acc) []