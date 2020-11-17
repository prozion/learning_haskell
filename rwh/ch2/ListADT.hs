data List a = Cons a (List a) | Nil deriving (Show)

fromList :: List a -> [a]
fromList (Cons x xs) = x:fromList(xs)
fromList Nil = []
