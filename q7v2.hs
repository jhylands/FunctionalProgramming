import Prelude hiding (max)
data Tri a = Tri [[a]]
instance Show a => Show (Tri a) where
  show (Tri a) =  unlines (pad (map triUnwords a))
triUnwords xs = unwords (map show xs)

-- the question is asking for a transpose reverse transpose

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
mapTail (x:xs) = if length x > 0 then tail x : mapTail xs else mapTail xs


--ben Example
data Ben a = Ben a
instance Show a => Show (Ben a) where 
  show (Ben i) = show i

tror = trol . trol
tror xs =   reverse (listTrans ( reverse (listTrans as))
tror xs = reverse (map reverse (listTrans (map reverse (listTrans (reverse xs)))))
tror xs = reverse (map reverse ((listTrans . listTrans) xs))

