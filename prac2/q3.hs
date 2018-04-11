--primes :: [Integer]
--primes = 2:[x|x<-[3,5..],prime x primes]

prime :: Integer->[Integer]->Bool
prime x (p:ps) =fromIntegral p>sqrt (fromIntegral x) || x `mod` p /=0 && prime x ps
--prime x = not (foldr1 (||) (map (\p->(x/p)==0) [p|\<-primes,p<sqrt(x)]))

-- a prime is prime if it is not devisable by any prime smaller than its sqrt

primes :: [Integer]
primes =[] ++ [x | x<-[2..], foldr ((&&) .(\n->x `mod` n/=0))  True primes]
