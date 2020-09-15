import Data.List

data Database = Database String Int deriving (Show)
data Server = Server String Int [Database]

getDatabaseName(name, _) = name
getDatabaseSize (_, size) = size

getServerName (name, _, _) = name
getServerCapacity (_, capacity, _) = capacity
getServerDatabase (_,_, database) = database

sumSizes [] = 0 
sumSizes (x:xs) = (getDatabaseSize x) + (sumSizes xs)

getServersCoefficients servers = [(
    server,
    coeff server
    ) | server <- servers]
    where coeff server = (sumSizes (getServerDatabase server)) `div` (getServerCapacity server)

sortBy' func (x:xs) =
    let bigger  = [a | a <- xs, (func a) >= (func x) ]
        smaller = [ a | a <- xs, (func a) < (func x)]
    in bigger ++ [x] ++ smaller

maximumBy' _ [] = ()
maximumBy' func (x:xs) = max (func x) (maximumBy' func xs)

getMaxDatabase list = head $ sortBy' snd list

mostUtilized l = getServerName $ fst $ head $ sortBy' snd $ getServersCoefficients l

createMaxDatabases' _ [] _ = []
createMaxDatabases' cap (x:xs) currCap
    | currCap + (getDatabaseSize newDb) >= cap = []
    | otherwise = newDb : (createMaxDatabases' cap xs (currCap + (getDatabaseSize newDb)))
    where newDb = getMaxDatabase (getServerDatabase x)

createMaxDatabases _ [] = []
createMaxDatabases cap (x:xs) = createMaxDatabases' cap (x:xs) 0

addNew s c list = (s,c, db) : list
    where   db = createMaxDatabases c list

databases1 = [("db1", 10),("db2", 20)]
databases2 = [("db1", 10),("db9", 20),("db3", 30),("db4", 40)]
databases3 = [("db1", 10),("db8", 20),("db3", 30),("db4", 40)]
databases4 = [("db1", 10)]
allServers = [("server1", 10, databases1),("server2", 20, databases2),("server3", 30, databases3),("server4", 40, databases4)]
        