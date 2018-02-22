data Mat a = Mat [[a]]
instance Show a => Show (Mat a) where 
  show (Mat []) = ""
  show (Mat (a:as)) = (unwords (map show a)) ++ ('\n': show (Mat as) )

transpose :: Mat a -> Mat a
transpose (Mat xs) = Mat (listTrans xs)

listTrans :: [[a]]->[[a]]
listTrans as | length (head as) > 0
	     = map head as   : listTrans (map tail as)
listTrans _ = []

--matCons :: Mat a -> Mat a -> Mat a
--matCons (Mat a) (Mat b) = (Mat a:b)

--zip :: [[a]]->[[a]]
--zip a b = [a:b]:zip as bs
--zip a = map head a   : zip map tail a

--ben Example
data Ben a = Ben a
instance Show a => Show (Ben a) where 
  show (Ben i) = show i



