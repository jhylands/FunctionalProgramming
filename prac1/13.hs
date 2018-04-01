change :: [Int] -> Int -> [[Int]]
change [] _ = []
change _ 0 = [[]]
change coins value 
    |value>0 = concat (map z vcoins)
    |otherwise = undefined
    where 
    z = (givenThisCoin vcoins value) 
    vcoins = [c|c<-coins,c<=value]

givenThisCoin :: [Int]->Int->Int->[[Int]]
givenThisCoin coins value thisCoin 
    |value-thisCoin>=0 = map (thisCoin:) theOthers
    where
        theOthers = change [c|c<-coins,c<=thisCoin] (value-thisCoin)

--    |otherwise = (undefined, False)

--we need some kind of order
--removeBadLeads :: [([[Int]],Bool)]->[[[Int]]]
--removeBadLeads [] = []
--removeBadLeads ((x,y):s) = if y then x:rest  else rest where rest = removeBadLeads s

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
