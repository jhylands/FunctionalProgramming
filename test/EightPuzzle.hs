import Data.Char (isDigit)
import Data.List (sort, transpose, permutations, isInfixOf,intercalate)
import System.IO (hFlush, stdout)
import System.Random (randomIO)
--import Test.FitSpec hiding (rows)

-- This program is about sliding-block puzzles in a
-- a 3x3 grid with eight tiles and one free space.
-- If the tiles are assigned digits 1 to 8 then the
-- goal is to reach
-- +---+---+---+
-- | 1 | 2 | 3 |
-- +---+---+---+
-- | 4 | 5 | 6 |
-- +---+---+---+
-- | 7 | 8 |   |
-- +---+---+---+
-- from some initial configuration of tiles. 

data Eight  =  E String

-- data invariant for Eight values; the second
-- conjunct ensures a solvable configuration
invariant :: Eight -> Bool
invariant (E s)  =  sort s == " 12345678" &&
                    even (falls (filter isDigit s))

isGoal :: Eight -> Bool
isGoal (E s)  =  s == "12345678 "

falls :: Ord a => [a] -> Int
falls [x,y] = if x>y then 1 else 0
falls (x:y:s) = if x>y then rest+1 else rest where rest = falls (y:s)

rows, cols :: Eight -> [String]
rows (E s)  =  chop 3 s
cols        =  transpose . rows

chop :: Int -> [a] -> [[a]]
chop _ []  =  []
chop n xs  =  take n xs : chop n (drop n xs)

unrows, uncols :: [String] -> Eight
unrows  =  E . concat
uncols  =  unrows . transpose

join :: [a]->[a]->[a]
join delim [x] = [x]
join delim (x:xs) = x: delim  ++ join delim xs

breaks :: Int->String->[String]
breaks n [] = []
breaks n s  |length s >= n = take n s : breaks n (drop n s)
           |otherwise = [s]
            

instance Show Eight where
  show (E e) = hr ++ "\n" ++ intercalate ("\n"++hr++"\n") pain  ++ ('\n':hr)
        where 
        hr = "+---+---+---+"
        pain = map f (breaks 3 e)
        f x = "| " ++ join " | " x ++ " |"

validMove :: String -> Eight -> Bool
validMove [t] e  =  or [adjacent t ' ' r | r <- rows e] ||
                    or [adjacent t ' ' c | c <- cols e]
validMove _   _  =  False

-- assumes each item occurs at most once
adjacent :: Eq a => a -> a -> [a] -> Bool
adjacent a b (x:y:etc)  |  x == a
                        =  y == b
                        |  x == b
                        =  y == a
                        |  otherwise
                        =  adjacent a b (y:etc)
adjacent _ _ _          =  False                


-- precondition: validMove [t] e
makeMove :: Char -> Eight -> Eight
makeMove t e  |  or [adjacent t ' ' r | r <- rows e]
              =  unrows [swapIfAdjacent t ' ' r | r <- rows e]
              |  or [adjacent t ' ' c | c <- cols e]
              =  uncols [swapIfAdjacent t ' ' c | c <- cols e]
              |  otherwise
              =  error "makeMove: not a valid move"

swapIfAdjacent :: Eq a => a -> a -> [a] -> [a]
swapIfAdjacent _ _ [] = []
swapIfAdjacent _ _ [x] = [x]
swapIfAdjacent x y (a:b:cde) |[a,b] `elem` [[n,m]|n<-[x,y],m<-[x,y]] = b:a:cde
                             |otherwise = a:swapIfAdjacent x y (b:cde)

{-           
properties :: (Word2 -> Word2 -> [Word2] -> [Word2]) -> [Property]
properties  =  error "Declare 'properties' of swapIfAdjacent.  See Q1(d)."
-}
interSolveRandom :: IO ()
interSolveRandom  =  do e <- randomEight
                        interSolve e
                        interSolveRandom

interSolve :: Eight -> IO ()
interSolve e    =  do putStr (show e)
                      if isGoal e then putStrLn "Goal!"
                      else do putStrLn $ "Goal is "++show (minSolve e)++" moves away."
                              t <- getValidMove e
                              interSolve (makeMove t e)

getValidMove :: Eight -> IO Char
getValidMove e  =  do putStr "Move? " ; hFlush stdout
                      input <- getLine
                      if validMove input e then return (Prelude.head input)
                      else do putStrLn "Not valid."
                              getValidMove e

randomEight :: IO Eight
randomEight  =  error "no"

minSolve :: Eight -> Int
minSolve  =  error "Declare a working 'minSolve'.  See Q1(f)."


