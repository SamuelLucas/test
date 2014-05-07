import Data.Foldable

data Tree a = Empty | Leaf a | Branch (Tree a) a (Tree a)

data Val = Op Char | Num Int

t1 = Branch (Leaf 'a') 'b' (Leaf 'c') 
-- b parent to a and c
t2 = Branch (Leaf (Num 4)) (Op '*') (Leaf (Num 5))
t3 = Branch (Leaf (Num 3)) (Op '+') t2

instance (Show a) => Show (Tree a) where
	show (Empty) = "<Em>"
	show (Leaf v) = "<<" ++ (show v) ++ ">>"
	show (Branch l v r) = "(Branch " ++ (show l) ++ " " ++
			(show v) ++ " " ++ (show r) ++ ") "

instance Show Val where
	show (Op c) = show c
	show (Num n) = show n

eval (Leaf (Num n)) = n
eval (Branch l (Op c) r) = (apply c) (eval l) (eval r)

apply '+' = (+)
apply '*' = (*)
apply '-' = (-)
