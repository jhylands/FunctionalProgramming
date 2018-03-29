module QueueSpec where
import Data.List

class QueueSpec q where
  empty   :: q a
  snoc    :: q a -> a -> q a
  head    :: q a -> a
  tail    :: q a -> q a
  queue   :: [a] -> q a
  queue   =  foldl snoc empty
  items   :: q a -> [a]
  isEmpty :: q a -> Bool
  isEmpty =  null . items
showQueue :: (Show a, QueueSpec q) => q a -> String
showQueue queue    =  "<<" ++ concat (intersperse " " (map show (items queue))) ++ "<<"
