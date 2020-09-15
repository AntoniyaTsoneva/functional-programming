import Data.List (minimumBy, maximumBy, nub)
import Data.Ord (comparing)

--2018/2019

-- Задача 1. Да се напише функция middle-digit, която намира средната цифра от записа на подадено естествено число n.
-- Ако n е с четен брой цифри, функцията връща -1.
-- Пример: (middle-digit 452) → 5
-- Пример: (middle-digit 4712) → -1

countDigits n 
    | n == 0    = 0
    | otherwise = 1 + (countDigits (n `div` 10))

getPosition cnt = cnt `div` 2

middleDigit n  
    | n < 10                       = n
    | cntDigits `mod` 2 == 0       = -1
    | otherwise                    = (n `div` (10^(getPosition cntDigits))) `mod` 10
    where cntDigits = countDigits n


-- Задача 2. Да се дефинира функция next-look-and-say, която по даден списък y намира списъка x, получен от прочитането y.
-- Пример: (next-look-and-say ‘(1 1 2 3 3)) → ‘(2 1 1 2 2 3)

removeFirstSame [n] = []
removeFirstSame (x:xs)
    | x == (head xs) = removeFirstSame xs
    | otherwise      = xs
 
countFirstSame [n] = 1
countFirstSame (x:xs)
    | x == (head xs) = 1 + (countFirstSame xs)
    | otherwise = 1
 
nextSay []  = []
nextSay [x] = 1 : [x]
nextSay x   = countFirstSame x : (head x) :(nextSay (removeFirstSame x))

-- Задача 5. Да се генерира поток sumsOfSquares от тези числа, 
-- които са сума от квадратите на две положителни цели числа.

-- 1 4 9 16 25 36 49 64 81 100 ....
-- 5 13 25 ....
---sumsOfSquares

sumOfSquares1 :: [Int]
sumOfSquares1 = nub [ a^2 + b^2 | a<-[1..], b<-[1..a] ]


----------------------------------------------------------------------------------------------
-- Задача 4. (12 т.) Видеоклип се представя с име (низ) и дължина (брой секунди). 
-- Да се напише функция averageVideo, която по непразен списък от видеоклипове намира името на този, 
-- който е с дължина най-близка до средната дължина на всички видеоклипове в списъка, без да я надхвърля.
-- Пример: averageVideo [("lolcat", 15), ("dogewow", 35), ("omgseethis", 28)] → "lolcat"

data Video = Video String Int deriving (Show)

average [] = 0
average (x:xs) = (snd x) + (average xs)

sortByLength [] = []
sortByLength (x:xs) =
    let bigger  = sortByLength [a | a <- xs, (snd a) >= (snd x)]
        smaller = sortByLength [a | a <- xs, (snd a) < (snd x)]
    in bigger ++ [x] ++ smaller

findNearest [] _ = error "empty list"
findNearest (x:xs) avg
    | (snd x) > avg   = findNearest xs avg
    | otherwise = x


averageVideo [] = error "empty list"
averageVideo xs = fst (findNearest (sortByLength xs) ((average xs) `div` (length xs)))

----------------------------------------------------------------------------------------------

-- Задача 4. Чифт обувки се представят с модел (низ) и номер (цяло число). 
-- Да се напише функция bestRange, която по даден непразен списък от чифтове намира модел обувки, 
-- от който има максимален брой различни номера.
-- Пример: bestRange [("boots", 38), ("sandals", 41), ("boots", 38), ("sandals", 43)] → "sandals"

data Shoe = Shoe String Int deriving (Show)

sortShoes [] _        = []
sortShoes (x:xs) func =
    let smaller = sortShoes [ a | a <- xs, (func a) <= (func x)] func
        bigger  = sortShoes [ a| a <- xs,  (func a) > (func x)] func
    in smaller ++ [x] ++ bigger


extractEqualShoes [x] = [x]    
extractEqualShoes (x:xs)
    | (fst x) == fst (head xs) =  [x] ++ (extractEqualShoes xs)
    | otherwise                = [x]


removeFirstSameShoes [] = []
removeFirstSameShoes (x:y:xs)
    | fst x == fst y = removeFirstSameShoes (y:xs)
    | otherwise      = (y:xs)

removeDuplicates [x] = [x]
removeDuplicates (x:xs)
    | snd x == snd (head xs) = removeDuplicates xs
    | otherwise              = x : removeDuplicates xs

bestRange [] = []
bestRange xs = length (removeDuplicates (extractEqualShoes sortedShoes)) : bestRange (removeFirstSame sortedShoes)
    where sortedShoes = sortShoes xs fst

bestRange' shoes = maximumBy (comparing getSizes) allNames
  where allNames = nub (map fst shoes)
        getSizes n = length (nub (filter ((==n) . fst) shoes))

shoes = [("boots", 38), ("sandals", 41), ("boots", 38), ("sandals", 43)]

----------------------------------------------------------------------------------------------------

-- Задача 2. Да се напише функция intervalTree, която преобразува двоично дърво от числа в ново дърво със 
-- същата структура, в което стойността във всеки възел е заменена с наредена двойка, представляваща 
-- най-малкия интервал, съдържащ всички стойности в съответното поддърво.

data Tree a = Empty | Node a (Tree a) (Tree a) deriving (Show)

leftTree Empty = Empty
leftTree (Node value left _) = left

rigthTree Empty = Empty
rigthTree (Node value _ right) = right

value Empty = error "no value"
value (Node value _ _) = value

-- getInterval =

-- getInterval Empty = []
-- getInterval (Node value left right) =
--     let l = intervalTree value (intervalTree left) right
--         r = intervalTree value left (intervalTree right)
--     in 


----------------------------------------------------------------------------------------------------
-- Задача 1. (10 т.) Да се напише функция findColumns, която по дадена матрица от числа намира броя на 
-- колоните, за които е вярно, че всичките им елементи се срещат в някой от редовете на матрицата.
-- Пример: findColumns [[1,4,3],[4,5,6],[7,4,9]] → 1

mLength [] = 0
mLength (x:xs) = 1 + mLength(xs)


containsElems [] _ = True
containsElems (x:xs) row 
    | x `elem` row = containsElems xs row
    | otherwise    = False

getColumn [] _ = []
getColumn (x:xs) j = (x !! j) : (getColumn xs j)

colHelper [] _ = 0
colHelper (x:xs) col
    | containsElems col x = 1 + (colHelper xs col)
    | otherwise = colHelper xs col

containsAllElems x col
    | containsElems col x = True
    | otherwise           = False

findColumns matrix = allCols matrix 0
    where allCols matrix j
            | j >= length (head matrix) = 0
            | otherwise = (colHelper matrix (getColumn matrix j)) + (allCols matrix (j + 1))
    
matrix = [[1,4,3],[4,5,6],[7,4,9]]


------------------------------------------------------------------------------------------------------------

-- Задача 4. (15 т.) Даден е граф g, представен със списък от наследници и връх u в графа. 
-- Да се напише функция maxPath, която намира най-дългия ацикличен път в g, започващ от върха u.
-- Пример: maxPath [[1,2,4],[2,3],[3,2],[4]] 1 → [1,2,3]

type Graph = [[Int]]
vertices :: Graph -> [Int]
vertices = map head
neighbours :: Int -> Graph -> [Int]
neighbours v g = tail $ head [ l | l<-g, (head l) == v ]

testGraph2 = [[1,2],[2,3],[3,1,4],[4,2]]

--maxLength = maximumBy (comparing length) 

--allPaths g = 

----------------------------------------------------------------------------------------------------

member _ [] = False
member x (y:ys)
    | x == y = True
    | otherwise = member x ys

--unionN xs ys = nub (xs ++ ys)

-- Problem: does not sort the list
-- union [] x = x
-- union (x:xs) ys 
--     | member x ys = union xs ys
--     | otherwise   = x : union xs ys

union xs ys = xs ++ union' ys xs
    where   union' [] _ = []
            union' (y:ys') first 
                | member y first = union' ys' first
                | otherwise      = y : union' ys' (y:first)

intersection [] _ = []
intersection (x:xs) ys
    | member x ys = x : intersection xs ys
    | otherwise      = intersection xs ys

difference [] _ = []
difference (x:xs) ys 
    | member x ys = difference xs ys
    | otherwise   = x : difference xs ys

-- Задача 3. (15 т.) Растение се описва с наредена тройка от име (низ), минимална и максимална температура, 
-- в която вирее (цели числа градуси). Да се напише функция garden, която по списък от растения намира интервал 
-- от стойности на температура, в който максимален брой растения могат да виреят, заедно с имената на тези растения.
-- Пример: garden [("peas",5,25),("beans",3,15),("cocoa",20,30)] → ((20,25),["peas","cocoa"])

data Plant = Plant String Int Int
getName (n,_,_) = n
getMin (_,min,_) = min
getMax (_,_,max) = max

garden plants = maximumBy (comparing (length . snd)) [ (intervals, getNames intervals plants) | intervals <- allIntervals]
    where   allIntervals = [ (minT, maxT) | minT <- (map getMin plants), maxT <- (map getMax plants), minT <= maxT]
            getNames intervals plants = [getName plant | plant <- plants, growsIn plant intervals]
            growsIn (_, plMin, plMax) (minT, maxT) = minT >= plMin && maxT <= plMax 

---------------------------------------------------------------------------------------------------------------------

-- Задача 3. (15 т.) Спектакъл се описва с наредена тройка от име (низ), начален кръгъл час (от 0 до 23) 
-- и брой минути, които продължава (от 1 до 300). Да се напише функция showtime която по списък от спектакли 
-- в един и същ ден намира в кой едночасов интервал протичат максимален брой спектакли, колко минути продължава 
-- засичането и кои са имената засичащите се спектакли.
-- Пример: showtime[("Cats",21,130),("Rent",19,100),("Hair",22,90)] → ((22,70),["Cats","Hair"])

data BigShow = String Int Int deriving (Show)
getShowName (name,_,_) = name
getStart (_,start,_) = start
getduration (_,_,duration) = duration


-- Задача 1. (10 т.) Да се напише функция, generateExponents, която по дадени различни естествени числа k и l, 
-- генерира безкрайния поток от всички числа от вида xkyl, без повторения и подредени в нарастващ ред.
-- Пример: generateExponents 2 3 → [1, 4, 8, 9, 16, 25, 27, ... ]

generateExponents k l = [x^k * y^l | x <- [1..], y <- [1..x]]

-- Задача 4. Лекарство се задава със наредена двойка от име (низ) и списък от активни съставки,
--  зададени като наредени двойки от име (низ) и количество в мг (цяло число). Казваме, че лекарството A 
--  е заместител на лекарството B, ако A има точно същите активни съставки като B в същата пропорция.
-- (4 т.) Да се реализира функция isSubstitute, която по две дадени лекарства проверява дали едното е заместител на другото.
-- (6 т.) Да се реализира функция bestSubstitutes, която по лекарство A и списък от лекарства L намира името на 
-- "най-добрия" заместител на A в L, чиито активни съставки са най-близки по количество до тези на A, без да ги 
-- надхвърлят, или празният низ, ако такъв няма.
-- (8 т.) Да се реализира функция groupSubstitutes, по даден списък от лекарства ги групира по “заместителство”,
--  т.е. връща списък от списъци от лекарства, където всички лекарства в даден списък са заместители един на друг.
-- Пример: l = [("A",[("p",6),("q",9)]),("B",[("p",2),("q",3)]),("C",[("p",3)])]
-- isSubstitute (l!!0) (l!!1) → True    bestSubstitute (l!!0) (tail l) → "B"
-- groupSubstitutes l → [[("A",...),("B",...)],[("C",...)]]

gcdList [x] = x
gcdList (x:xs) = gcd x (gcdList xs)

-- isProportion a b 
--     | gcdList (getIngridients a)

-- isSubstitute a b
--     | (getIngridients a) != (getIngridients b) = False
--     | isProportion (sortShoes fst a) (sortShoes fst b) = True

-- data Ingredient = Ingridient String Int
-- data Drug = Drug String [Ingredient]

-- getIngridients (_,ingridients) = ingridients
-- getDrugName (name, _) = name




