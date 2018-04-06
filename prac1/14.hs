import Control.Monad

promptLine :: String ->IO String
promptLine output = putStrLn output >> getLine

main :: IO()
main = do
    fileName <- promptLine "file:"
    fileLines <- liftM lines (readFile fileName)
    sum <- liftM resultSum (sequence (itterateThough fileLines repeatAfterMe))
    printResultSum sum

resultSum :: [(Int,Int)]->(Int,Int)
resultSum [] = (0,0)
resultSum ((x,y):xys) = (x+xs,y+ys) where (xs,ys) = resultSum xys

printResultSum :: (Int,Int) -> IO()
printResultSum (x,y) = putStrLn (show x ++ " : " ++ show y)
--This is a function which takes a line, and gets the user to copy that line
--returning the number of words typed and the number of errors
repeatAfterMe :: String->IO (Int, Int)
repeatAfterMe x = putStrLn x >> getLine >>= return . checkWords x

checkWords :: String->String->(Int,Int)
checkWords x ln = (length wds,countFalse$zipWith (==) x ln)   
    where wds = words ln
--works like an ordered map function
itterateThough :: [a]->(a->b)->[b]
itterateThough [] _ = []
itterateThough (x:xs) f = f x : itterateThough xs f

countFalse :: [Bool]->Int
countFalse [] = 0
countFalse (x:xs)  |x = countFalse xs
                    |otherwise = 1+countFalse xs

{-
check :: [a]->[a]->Int
check _ [] = 0
check [] _ = check _ []
check w:ws r:rs = if w==r then 1+ rst else rst where rst = check ws rs
-}
--    putStrLn$show (length (words theline))
-- >> getLine >> length$words ln
--take command line input

--doPureStuff :: [String] -> IO String
--doPureStuff ln = 

--check :: IO String->String
--check user_input = do user_input  
    
--main = getLine >>= lines$readFile >>= putStrLn$head
--        putStrLn show (sum$map repeatAfterMe file)

