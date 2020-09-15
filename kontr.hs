-- Задача 3. (8 т.) Покупка се означава с наредена тройка от име на магазин (низ), категория (низ) и цена (дробно число).
-- Да се реализира функция, която по даден списък от покупки връща списък от тройки, съдържащи категория, обща цена на покупките в тази категория и името на магазина, в който общата цена на покупките в тази категория е максимална.
-- Всяка категория да се среща в точно една тройка от резултата.

(|>) :: a -> (a -> b) -> b
x |> f = f x

maxBy :: Ord b => (a -> b) -> [a] -> a
maxBy _ [] = error "Nothing to compare"
maxBy f (h:t) = snd $ foldl comparator (f h, h) t
  where comparator (maxValue, maxX) x
          | value < maxValue = (value, x)
          | otherwise        = (maxValue, maxX)
            where value = f x

unique :: Eq a => [a] -> [a]
unique = reverse . foldl keepOnce []
  where keepOnce seen x
          | x `elem` seen = seen
          | otherwise     = x : seen

type Store = String
type Category = String
type Price = Double
type Purchase = (Store, Category, Price)

getStore :: Purchase -> Store
getStore (_, store, _) = store

getCategory :: Purchase -> Category
getCategory (category, _, _) = category

getPrice :: Purchase -> Price
getPrice (_, _, price) = price

mostExpensive :: [Purchase] -> [(Category, Price, Store)]
mostExpensive purchases = [(
    category,
    getTotalPrice ((== category) . getCategory),
    mostExpensiveStore category
  ) | category <- categories]
  where categories = purchases |> map getCategory |> unique
        getTotalPrice p = purchases |> filter p |> map getPrice |> sum
        mostExpensiveStore category = maxBy (totalPriceFor category) stores
          where stores = purchases |> map getStore |> unique
                totalPriceFor category store = getTotalPrice (\(c, s, _) -> c == category && s == store)

main :: IO ()
main = print ":)"


type Vertex = Int
type Graph = [(Vertex, [Vertex])]

vertices :: Graph -> [Vertex]
vertices = concatMap snd

children :: Vertex -> Graph -> [Vertex]
children v g = snd $ head $ filter ((== v) . fst) g

isEdge :: Vertex -> Vertex -> Graph -> Bool
isEdge u v g = u `elem` children v g

parents :: Vertex -> Graph -> [Vertex]
parents v g = [u | u <- vertices g, isEdge u v g]

-- “Семейство” наричаме множество от възли F такова, че за всеки възел u ∈ F е вярно, че във F са всичките му деца и нито един негов родител или всичките му родители и нито едно негово дете.
-- а) (6 т.) Да се реализира функция isFamily, която проверява дали дадено множество от възли е семейство в даден граф;

intersect :: Eq a => [a] -> [a] -> [a]
a `intersect` b = [x | x <- a, x `elem` b]

contains :: Eq a => [a] -> [a] -> Bool
set `contains` subset = all (`elem` set) subset

isDisjointWith :: Eq a => [a] -> [a] -> Bool
a `isDisjointWith` b = null $ a `intersect` b

isFamily :: Graph -> [Vertex] -> Bool
isFamily g set = all isFromFamily set
  where isFromFamily v = onlyChildren || onlyParents
          where onlyChildren = set `contains` allChildren && set `isDisjointWith` allParents
                onlyParents = set `contains` allParents && set `isDisjointWith` allChildren
                allChildren = children v g
                allParents = parents v g

-- б) (10 т.) Да се реализира функция minIncluding, която по даден възел u намира минимално множество от възли, което е семейство и съдържа u (ако такова семейство има).

tails :: [a] -> [[a]]
tails [] = []
tails l = l : tails (tail l)

inits :: [a] -> [[a]]
inits [] = []
inits l = l : inits (init l)

subsequences :: [a] -> [[a]]
subsequences l = [] : [suffix | prefix <- inits l, suffix <- tails prefix]

minBy :: Ord b => (a -> b) -> [a] -> a
minBy _ [] = error "Nothing to compare"
minBy f (h:t) = snd $ foldl comparator (f h, h) t
  where comparator (minValue, minX) x
          | value < minValue = (value, x)
          | otherwise        = (minValue, minX)
            where value = f x

minIncluding :: Graph -> Vertex -> [Vertex]
minIncluding g u = minBy length [s | s <- subsequences $ vertices g,
                                     u `elem` s,
                                     isFamily g s]