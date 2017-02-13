{-# OPTIONS_GHC -Wall -fno-warn-unused-imports #-}


module Hw02 where


data ArithExp =
    Num Int
  | Plus ArithExp ArithExp
  | Times ArithExp ArithExp
  | Neg ArithExp

instance Show ArithExp where
  show (Num a)= "Num " ++ show a
  show (Plus a b) = "Plus " ++ "(" ++ show a ++ ") " ++ "(" ++ show b ++ ")"
  show (Times a b) = "Times " ++ "(" ++ show a ++ ") " ++ "(" ++ show b ++ ")"
  show (Neg a) = "Neg " ++ "(" ++ show a ++ ")"

instance Eq ArithExp where
  (Num a) == (Num b) = (a == b)
  (Plus a b) == (Plus c d) = (a == c) && (b == d)
  (Times a b) == (Times c d) = (a == c) && (b == d)
  (Neg a) == (Neg b) = (a == b)
  _ == _ = False

eval :: ArithExp -> Int
eval (Num a) = a
eval (Plus a b) = eval a + eval b
eval (Times a b) = eval a * eval b
eval (Neg a) = - (eval a)

data ArithExp' =
    Num' Int
  | Plus' ArithExp' ArithExp'
  | Sub' ArithExp' ArithExp'
  | Times' ArithExp' ArithExp'
  | Neg' ArithExp'
  deriving Show

eval' :: ArithExp' -> Int
eval' = eval . translate

translate :: ArithExp' -> ArithExp
translate (Num' a) = Num a
translate (Plus' a b) = Plus (translate a)  (translate b)
translate (Sub' a b) = Plus (translate a) (Neg (translate b))
translate (Times' a b) = Times (translate a) (translate b)
translate (Neg' a) = Neg (translate a)

instance Eq ArithExp' where
  e1 == e2 = (eval' e1) == (eval' e2)

instance Ord ArithExp' where
  compare e1 e2 | (eval' e1) < (eval' e2) = LT
  compare e1 e2 | (eval' e1) == (eval' e2) = EQ
  compare e1 e2 | (eval' e1) > (eval' e2) = GT

class Setlike f where
  emp :: f a

  singleton :: a -> f a

  union :: Ord a => f a -> f a -> f a
  union = fold insert

  insert :: Ord a => a -> f a -> f a
  insert = union . singleton

  suc :: Ord a => f a -> a

  deleteH :: Ord a => f a -> f a
 
  delete :: Ord a => a -> f a -> f a
  delete x s = fold (\y s' -> if x == y then s' else insert y s') emp s

  isEmpty :: f a -> Bool
  isEmpty = (==0) . size

  size :: f a -> Int
  size = fold (\_ count -> count + 1) 0

  isIn :: Ord a => a -> f a -> Bool
  isIn x s = maybe False (const True) $ getElem x s

  getElem :: Ord a => a -> f a -> Maybe a

  fold :: (a -> b -> b) -> b -> f a -> b

  toAscList :: f a -> [a] -- must return the list sorted ascending
  toAscList = fold (:) []


instance Setlike ([]) where
 emp = [] 

 singleton a = [a] 

 union [] [] = []
 union (x:xs) [] = (x:xs)
 union [] (y:ys) = (y:ys)
 union (x:xs) (y:ys) = if elem y (x:xs) then union (x:xs) ys
						  else union ((x:xs) ++ [y]) ys
 fold f a b = foldr f a b					  
 getElem _ [] = Nothing
 getElem a (x:xs) = if (a == x) then Just x
		               else getElem a xs
	

evensUpToTen :: [Int]
evensUpToTen = foldr insert emp [0,2,4,6,8]

 
data BST a = Empty | Node (BST a) a (BST a)

instance Setlike BST where
	emp = Empty

	singleton a = Node (Empty) a (Empty)

	insert a Empty = Node Empty a Empty
	insert a (Node l x r) 
		| x==a  = Node l x r
		| x < a = Node l x (insert a r)
		| x > a = Node (insert a l) x r

	fold _ a Empty                = a
	fold f a (Node Empty x Empty) = f x a
	fold f a (Node Empty x r)     = f x (fold f a r)
	fold f a (Node l x Empty)     = f x (fold f a l)
	fold f a (Node l x r)         = f x (fold f (fold f a r) l)


	suc (Node Empty x _) = x
	suc (Node l _ _)     = suc l

	deleteH (Node Empty _ r)     = r
	deleteH (Node l _ Empty)     = l
	deleteH (Node l _ r) = (Node l y r)
		where 
			y = suc r

	getElem _ Empty = Nothing
	getElem a (Node l x r)
		| a == x = (Just a)
		| x > a  = getElem a l
		| x < a  = getElem a r

	delete _ Empty        = Empty
	delete a (Node l x r)
		| x == a = deleteH (Node l x r)
		| a > x  = Node (delete a l) x r
		| a < x  = Node l x (delete a r)



instance Ord a => Eq (BST a) where
  Empty == Empty                               = True
  (Node Empty x Empty) == (Node Empty y Empty) = (x == y)
  (Node Empty x r) == (Node Empty a b)         = (x == a) && (r == b)
  (Node l x Empty) == (Node a b Empty)         = (x == b) && (l == a)
  (Node l x r) == (Node a b c)                 = (l == a) && (x == b) && (r == c)
  _ == _                                       = False


instance Show a => Show (BST a) where
  show Empty                = "Empty"
  show (Node Empty x Empty) = "(Node Empty " ++ show x ++ " Empty)"
  show (Node l x Empty)     = "(Node " ++ show l ++ " " ++ show x ++ " Empty)"
  show (Node Empty x r)     = "(Node " ++ "Empty " ++ show x ++ " " ++ show r ++ ")"
  show (Node l x r)         = "(Node " ++ show l ++ " " ++ show x ++ " " ++ show r ++ ")"


fromList :: (Setlike f, Ord a) => [a] -> f a
fromList []     = emp
fromList (x:xs) =  union (insert x emp) (fromList xs)

difference :: (Setlike f, Ord a) => f a -> f a -> f a
difference xs ys  = fromList (differenceH (toAscList xs) (toAscList ys))

differenceH :: (Eq a) => [a] -> [a] -> [a]
differenceH [] [] = []
differenceH [] _  = []
differenceH xs []  = xs
differenceH (x:xs) (y:ys) = if (elem x (y:ys)) then differenceH xs (y:ys)
							  else x : (differenceH xs (y:ys))
subset :: (Setlike f, Ord a) => f a -> f a -> Bool
subset xs ys = subsetH (toAscList xs) (toAscList ys)

subsetH :: (Ord a) => [a] -> [a] -> Bool
subsetH [] [] = True
subsetH [] _ = True
subsetH _ [] = False
subsetH (x:xs) (y:ys) = if (elem x (y:ys)) then subsetH xs (y:ys)
						  else False

newtype KV k v = KV { kv :: (k,v) }

instance Eq k => Eq (KV k v) where
  (KV kv1) == (KV kv2) = fst kv1 == fst kv2

instance Ord k => Ord (KV k v) where
  compare (KV kv1) (KV kv2) = compare (fst kv1) (fst kv2)

instance (Show k, Show v) => Show (KV k v) where
  show (KV (k,v)) = show k ++ " |-> " ++ show v

type Map f k v = f (KV k v)
type ListMap k v = Map [] k v
type TreeMap k v = Map BST k v

emptyMap :: Setlike f => Map f k v
emptyMap = emp

lookup' :: (Setlike f, Ord k) => k -> Map f k v -> Maybe v
lookup' k m = case (getElem (KV (k, undefined)) m) of
					(Just (KV (_,v))) -> (Just v)
					Nothing           -> Nothing



extend :: (Setlike f, Ord k) => k -> v -> Map f k v -> Map f k v
extend k v m = insert (KV (k, v)) m

remove :: (Setlike f, Ord k) => k -> Map f k v -> Map f k v
remove k m = undefined

toAssocList :: Setlike f => Map f k v -> [(k,v)]
toAssocList m = fold (\x y-> (toAssocListH x):y) [] m

toAssocListH :: KV a b -> (a,b)
toAssocListH (KV (k,v)) = (k,v)


data RoseTree a = Leaf a | Branch [RoseTree a] deriving (Eq, Show)

instance Functor RoseTree where
  fmap f (Leaf a)        = Leaf (f a)           
  fmap f (Branch (ls)) = Branch (foldl (\x y -> x ++ [(fmap f y)]) [] (ls))

instance Functor BST where
	fmap _ Empty        = Empty
	fmap f (Node l x r) = Node (fmap f l) (f x) (fmap f r)