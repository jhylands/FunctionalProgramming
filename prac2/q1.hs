
data Tri a = Tri [[a]]
instance Show a => Show (Tri a) where
  show (Tri a) =  unlines (pad (map triUnwords a))
triUnwords xs = unwords (map show xs)

pascal :: Integer->Tri Integer
pascal n  = Tri (listPascal n)
listPascal :: Integer->[[Integer]]
listPascal 1 = [[1]]
listPascal n = lp ++ [1:addUp (last lp) ++ [1]]
    where 
    lp =  listPascal (n-1)

addUp :: [Integer]->[Integer]
addUp [] = []
addUp [x] = []
addUp (x:y:xs) = x+y : addUp (y:xs)

pad :: [String]->[String]
pad [] = []
pad (x:xs) = (replicate (length xs) ' ' ++ x) : pad xs
