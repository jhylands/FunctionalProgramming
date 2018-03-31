change :: [Int] -> Int -> [[Int]]
change coins value = concat map (y coins value) coins

y :: [Int]->Int->Int->[[Int]]
y coins value x = change coins (value-x)


--changeOfaCoin coin value = change coins value-coin
