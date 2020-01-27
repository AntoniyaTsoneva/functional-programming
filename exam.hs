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

          
---------------------Basic-Functions------------------------
--sum
sum' :: Num t => [t] -> t
sum' [] = 0
sum' (x:xs) = x + sum' xs

--lenght
lenght' :: Num t => [t1] -> t
lenght' [] = 0
lenght' (_:xs) = 1 + lenght' xs

--head
head' :: [a] -> a
head' [] = error "error: empty list"
head' (x:_) = x

--tail
tail' :: [a] -> [a]
tail' [] = error "error: empty list"
tail' (_:xs) = xs

--maximum
maximum' []  = error "error"
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
