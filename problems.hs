--------------Problems--------------------

--largestDivisible
largestDivisible = head (filter p [100000,99999..])
    where p x = x `mod` 3829 == 0 


--sum of all odds squares that are smaller than 10,000
sumOfAllOdds = sum (takeWhile (<10000) (filter odd (map (^2) [1..])))


--chain
chain 1 = [1]
chain x 
    | even x = x:chain (x `div` 2)
    | odd x  = x:chain (x * 3 + 1)


numberOfLongChains = length (filter isLonger (map chain [1..100])) 
    where isLonger x = length x > 15

numberOfLongChains' = length (filter (\x -> length x > 15) (map chain [1..100]))

listOfFunctions = map (*) [0..]
-- ghci> (listOfFunctions !! 4) 5
-- ghci> 20


