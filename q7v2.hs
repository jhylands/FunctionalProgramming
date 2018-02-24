import Prelude hiding (max)
data Tri a = Tri [[a]]
instance Show a => Show (Tri a) where
  show (Tri a) =  unlines (pad (map triUnwords a))
triUnwords xs = unwords (map show xs)

-- the question is asking for a transpose reverse transpose



pad :: [String]->[String]
pad [] = []
pad (x:xs) = (replicate (length xs) ' ' ++ x) : (pad xs)

trol :: Tri a -> Tri a
trol (Tri xs) = Tri (listTrol xs)

listTrol :: [[a]] -> [[a]]
listTrol [] = []
listTrol as = reverse (listTrans as)

listTrans :: [[a]]->[[a]]
listTrans [] = []
listTrans as = mapHead as : listTrans (mapTail as)

-- the problem is in that we are mapping 
mapHead :: [[a]] -> [a]
mapHead [] = []
mapHead (x:xs) =  if length x >0 then head x : mapHead xs  else mapHead xs 

mapTail :: [[a]] -> [[a]]
mapTail (x:[]) = if length x > 1 then tail x :[] else []
mapTail (x:xs) = if length x >0 then tail x : mapTail xs else mapTail xs
--	     |anyHeadBetter as =  
--	     |otherwise = []

--is there any head that has a length larger than 0
--rather than just the first
--anyHeadBetter :: [[a]] -> Bool
--anyHeadBetter as = foldr (+) (map (length . head) as) > 0 

--fillSquare :: [[a]] -> [[(Maybe a)]]
--fillSquare xs 	= map (arrPad mx) xs where  mx = maximum (map length xs)

--sparceSqaure:: [[(Maybe a)]] -> [[a]]
--sparceSqaure (x:xs) = map 

--j2a :: [Maybe a] -> [a]
--j2a [(Maybe a):as] = [a]
--j2a [Nothing] = []
--j2a [] =[]
--data Pad = Pass | James Int
--instance Show Pad => Show Pad where
--  show Pass = "Pad"
-- -- show (James x) = show x

--arrPad :: Int->[a] -> [(Maybe a)]
--arrPad mx xs = (map Just xs) ++(replicate (mx-len) Nothing)  where len = length xs




--max :: Int->Int->Int
--max x y = if x>y then x else y
--ben Example
data Ben a = Ben a
instance Show a => Show (Ben a) where 
  show (Ben i) = show i

