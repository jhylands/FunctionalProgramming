hamming :: [Integer]
hamming  = h 2 `merge` h 3 `merge` h 5
    where
    h n = map (n*) (1:hamming)

merge :: Ord a => [a] -> [a] -> [a]
merge (x:xs) (y:ys)  =  case compare x y of
                        LT -> x : merge xs (y:ys)
                        EQ -> x : merge xs ys
                        GT -> y : merge (x:xs) ys

ishamming :: Integer->Bool
ishamming x = True
-- A nieve way to do the above function is devide by 2,3,5 as that will 
-- result true for all the hammin numbers
-- the problem is that this will also result true for some non-hamming numbers


--canDiv :: Integer -> Bool
--canDiv x y = x % y == 0

