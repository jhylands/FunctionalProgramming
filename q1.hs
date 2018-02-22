import Prelude hiding(take,drop)

take,drop :: Int->[a]->[a]
take 0 _ = []
take n (x:xs) = x:take (n-1) xs

drop 0 xs =  xs
drop n (x:xs) = drop (n-1) xs
