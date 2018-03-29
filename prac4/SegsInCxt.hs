segsInCxt :: [a] -> [([a],[a],[a])]
segsInCxt [x] = [([],[x],[])]
segsInCxt (x:xs) = let old = segsInCxt xs in ( middle x (head old) ): addMiddle x old ++ addfirst x old

middle :: a->([a],[a],[a])->([a],[a],[a])
middle n ([],[x],y) = ([],[n],x:y)

addMiddle :: a->[([a],[a],[a])]->[([a],[a],[a])]
addMiddle n (([],x,y):ss) = ([],n:x,y): addMiddle n ss
addMiddle n xs = xs

addfirst :: a->[([a],[a],[a])]->[([a],[a],[a])]
addfirst n [] = []
addfirst n ((x,y,z):ss) = (n:x,y,z): addfirst n ss 


