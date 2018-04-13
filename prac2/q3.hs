primes :: [Integer]
primes = 2:3: findNext2 primes 1 

findNext2 :: [Integer]->Integer->[Integer]
findNext2 ps from = if foldr ((&&) . (\n->from `mod` n >0))  True ps 
then from : findNext2 
else findNext2 ps (from+2)


nPrimes :: Int -> [Integer]
nPrimes 1 = [2]
nPrimes 2 = [2,3]
nPrimes n = findNext nm (last nm) where nm = nPrimes (n-1) 

findNext :: [Integer]->Integer->[Integer]
findNext ps from = if foldr ((&&) .(\n->from `mod` n >0))  True ps 
then ps++[from ]
else findNext ps (from+2)
