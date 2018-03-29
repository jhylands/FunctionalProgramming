data Tri a = Tri [[a]]
instance Show a => Show (Tri a) where
  show (Tri a) =  unlines (pad (map triUnwords a))
triUnwords xs = unwords (map show xs)

pad :: [String]->[String]
pad [] = []
pad (x:xs) = (replicate (length xs) ' ' ++ x) : (pad xs)

trol :: Tri a -> Tri a
trol (Tri xs) = Tri (listTrol xs)

listTrol :: [[a]] -> [[a]]
listTrol [] = []
listTrol as = map reverse (listTrans as)

listTrans :: [[a]]->[[a]]
listTrans [] = []
listTrans as =  map safeHead as : listTrans (map tail as)
--	     |length (head as) > 0 = map head as : listTrans (map tail as)
--	     |anyHeadBetter as =  
--	     |otherwise = []

--is there any head that has a length larger than 0
--rather than just the first
--anyHeadBetter :: [[a]] -> Bool
--anyHeadBetter as = foldr (+) (map (length . head) as) > 0 


safeHead :: [a]->a
  
safeHead xs = head xs
--try to transpose

-- map head as : listTrans (map tail as)

--ben Example
data Ben a = Ben a
instance Show a => Show (Ben a) where 
  show (Ben i) = show i

