
hamming :: [Integer]
hamming = [x|x<-Integer,ishamming(x)]

ham :: [Integer] -> [Integer]
ham x = map (*) hamming 


ishamming :: Integer->Bool
ishamming x = 

--canDiv :: Integer -> Bool
--canDiv x y = x % y == 0

primes :: [Integer]
primes = 2:[x|x<-[3,5..],prime x primes]

prime :: Integer->[Integer]->Bool
prime x (p:ps) = if (fromIntegral p)>sqrt (fromIntegral x) then True else x `mod` p /=0 && prime x ps
--prime x = not (foldr1 (||) (map (\p->(x/p)==0) [p|\<-primes,p<sqrt(x)]))





