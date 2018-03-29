
data Tri a = Tri [[a]]
instance Show a => Show (Tri a) where
  show (Tri a) =  unlines (pad (map triUnwords a))
triUnwords xs = unwords (map show xs)

--pascal :: Tri Integer
listPascal :: Integer->[[Integer]] 
listPascal 1 = [[1]]
listPascal n = let lp =  listPascal (n-1)  in lp: [1:addUp (end lp) :[1]]

addUp :: [Integer]->Integer
addUp [] = []
addUp [x] = []
addUp [x:y:xs] = [x+y] : addUp (y:xs)

end :: [a]->a
end [x] = x
end [x:xs] = end xs



pad :: [String]->[String]
pad [] = []
pad (x:xs) = (replicate (length xs) ' ' ++ x) : (pad xs)
