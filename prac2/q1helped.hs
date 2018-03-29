data Tri a = Tri [[a]]
instance Show a => Show (Tri a) where
  show (Tri a) =  unlines (pad (map triUnwords a))
triUnwords xs = unwords (map show xs)
 
pascal = concat pascalR
pascalR = [1]:map padListThenAddUp pascalR

padListThenAddUp :: [Integer]->[Integer]
padListThenAddUp x = addUp (0:x++[0])

addUp :: [Integer]->[Integer]
addUp [] = []
addUp [x] = []
addUp (x:y:xs) = (x+y) : addUp (y:xs)

pad :: [String]->[String]
pad [] = []
pad (x:xs) = (replicate (length xs) ' ' ++ x) : (pad xs)
