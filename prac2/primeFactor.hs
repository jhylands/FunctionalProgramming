fromP :: [Integers]-> Integer
fromP xs =  sum$zipWith (*) xs primes

