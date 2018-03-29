import ListQ
import Prelude hiding (head,tail)
countTo :: QueueSpec q => Int->(q Int)
countTo  = bfs (queue [0]) 

bfs :: QueueSpec q => (q Int)->Int->(q Int)
bfs q n = if (head q)==n then empty else  aQueue where
        aQueue = bfs (foldl snoc (tail q) (branch (head q))) n


appendQueues q1 q2 = if isEmpty q2 then q1 
                                   else appendQueues (snoc q1 (head q2)) (tail q2)


branch :: Int->[Int]
branch n = [(2*n)+1,2*(n+1)]
