change :: [Int] -> Int -> [[Int]]
change [] _ = []
change _ 0 = [[]]
change coins value 
    |value>0 = concat (map z coins) 
    |otherwise = [[]]
    where z = (givenThisCoin coins value) 

--z :: Int->[[Int]]
--zomp ::[Int]->int->(Int->[[Int]])
--zomp = givenThisCoin coins value




givenThisCoin :: [Int]->Int->Int->[[Int]]
givenThisCoin coins value thisCoin 
    |value-thisCoin>=0 = map (thisCoin:) (change coins (value-thisCoin))


{-
[1,2] 3 

1: [1,2] 2
2: [1,2] 1
1:1: [1,2] 1
2:1 [1,2] 0
1:1:1 [1,2] 0
[[2,1],[1,1,1]]
-}
--changeOfaCoin coin value = change coins value-coin
