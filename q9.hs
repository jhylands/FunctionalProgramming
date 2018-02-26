prefixes, suffixes :: [a]->[[a]]
suffixes (x:[]) = [x:[]]
suffixes (x:xs) = (x:(head r)) : r where 
				r = suffixes(xs) 


--prefixes x = suffixes (reverse x)

prefixes (x:[]) = [x:[]]
prefixes (x:xs) = [x]: (map (x:) (prefixes xs))

