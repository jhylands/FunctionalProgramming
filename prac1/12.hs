perms :: [a]->[[a]]
perms [] = []
perms [x] = [[x]]
perms (x:xs) =  concat (map (allinto x) (perms xs))


-- silver platter 
--perms [0] =     [[0]]
--perms [0,1] =   [[0,1],[1,0]]
--perms [0,1,2] = [[0,1,2],[0,2,1],[2,0,1],[1,0,2],[1,2,0],[2,1,0]]

allinto :: a->[a]->[[a]]
allinto i [] = [[i]]
allinto i (x:xs) = (i:x:xs) : (map (x:) rest) where rest = allinto i xs

infront :: a->[a]->[a]
infront i xs = i:xs



-- for every element in ys we want to create an element for each posiion x could be in
--where 
--	ys = perms xs


