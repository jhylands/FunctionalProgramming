

countTo :: Int->[Int]
countTo  = bfs [0] 

bfs :: [Int]->Int->[Int]
bfs (x:xs) n = if x==n then [] else x : bfs (xs ++ branch x) n


branch :: Int->[Int]
branch n = [(2*n)+1,2*(n+1)]
