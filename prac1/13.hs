change :: [Int] -> Int -> [[Int]]
change [] _ = []
change _ 0 = [[]]
change coins value 
    |value>0 = concat (removeBadLeads (map z coins))
    |otherwise = [[]]
    where z = (givenThisCoin coins value) 

--we need some kind of order
removeBadLeads :: [([[Int]],Bool)]->[[[Int]]]
removeBadLeads [] = []
removeBadLeads ((x,y):s) = if y then x:rest  else rest where rest = removeBadLeads s


givenThisCoin :: [Int]->Int->Int->([[Int]],Bool)
givenThisCoin coins value thisCoin 
    |value-thisCoin>=0 = (map (thisCoin:) (change [c|c<-coins,c<=thisCoin] (value-thisCoin)),True)
    |otherwise = (undefined, False)


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
