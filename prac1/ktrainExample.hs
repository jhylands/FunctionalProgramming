-- Ktrain: a solution to the final exercise on the FUNC
-- practical sheet following Lectures 1-3

import System.Environment
import System.Time

main :: IO ()
main  =  do [fname]   <- getArgs
            flines    <- readFile fname >>= return . lines
            ((w,e),t) <- timed (trainWith (0,0) flines)
            putStr    ("speed " ++
                       show (w * 60 `div` t) ++ "wpm, ")
            putStrLn  ("accuracy " ++
                       show (max 0 (w-e) * 100 `div` w) ++ "%")

timed :: IO a -> IO (a,Int)
timed action  =  do tstart <- getClockTime
                    x      <- action
                    tstop  <- getClockTime
                    let t   = tstop `diffSeconds` tstart
                    return (x,t)

-- difference of clock times less than a day apart
-- disregarding fractions of a second                    
diffSeconds :: ClockTime -> ClockTime -> Int
t1 `diffSeconds` t0  |  d < noTimeDiff{tdDay = 1}
                     =  tdHour d * 3600 + tdMin d * 60 + tdSec d
            where d  =  t1 `diffClockTimes` t0

-- (w,e) is an accumulating argument with w the number
-- of words typed and e the number of errors made
trainWith :: (Int,Int) -> [String] -> IO (Int,Int)
trainWith we    []      =  return we
trainWith (w,e) (x:xs)  =  do putStrLn x
                              y <- getLine
                              let (wy,ey) = check x y
                              let (w',e') = (w+wy,e+ey)
                              putStrLn (show w' ++" words, "++
                                        show e' ++" errors")
                              trainWith (w',e') xs

-- compares two lines of text, a given line and an
-- entered line, using words as the unit of comparison
-- and giving a (w,e) result --- see trainWith
check :: String -> String -> (Int,Int)
check x y    =  (length yws, delta xws yws)
  where xws  =  words x
        yws  =  words y

-- very inefficient in the worst case but
-- ok for short lists with few differences
delta :: Eq a => [a] -> [a] -> Int
delta []     ys      =  length ys
delta xs     []      =  length xs
delta (x:xs) (y:ys)  |  x == y
                     =  delta xs ys
                     |  otherwise
                     =  1 + minimum [delta (x:xs) ys,
                                     delta xs     ys,
                                     delta xs (y:ys)]


