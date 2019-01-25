module Chapter3
    ( module Chapter3
    )
where


--------------------------------------------------
-- LIST STYLE DATA STRUCTURE
--------------------------------------------------

data MyList a = Empty
    | Cons a (MyList a)
    deriving (Show, Eq)

mySum :: (Num a) => MyList a -> a
mySum Empty       = 0
mySum (Cons x xs) = x + mySum xs

myProduct :: (Num a) => MyList a -> a
myProduct Empty       = 1
myProduct (Cons x xs) = x * myProduct xs

myTail :: MyList a -> MyList a
myTail Empty       = Empty
myTail (Cons x xs) = xs

mySetHead :: MyList a -> a -> MyList a
mySetHead Empty       y = Empty
mySetHead (Cons x xs) y = Cons y xs

myDrop :: MyList a -> Int -> MyList a
myDrop Empty       _ = Empty
myDrop xs          0 = xs
myDrop (Cons x xs) n = myDrop xs (n - 1)

myDropWhile :: MyList a -> (a -> Bool) -> MyList a
myDropWhile Empty f = Empty
myDropWhile (Cons x xs) f | f x       = myDropWhile xs f
                          | otherwise = Cons x xs

myInit :: MyList a -> MyList a
myInit Empty                   = Empty
myInit (Cons x (Cons y Empty)) = Cons x Empty
myInit (Cons x xs            ) = Cons x (myInit xs)

myFoldRight :: MyList a -> b -> (a -> b -> b) -> b
myFoldRight Empty       b _ = b
myFoldRight (Cons a as) b f = f a (myFoldRight as b f)

myLength :: MyList a -> Int
myLength Empty = 0
myLength as    = myFoldRight as 0 (\a b -> b + 1)

myFoldLeft :: MyList a -> b -> (b -> a -> b) -> b
myFoldLeft Empty       b _ = b
myFoldLeft (Cons a as) b f = myFoldLeft as (f b a) f

myReverse :: MyList a -> MyList a
myReverse xs = myFoldLeft xs Empty (\b a -> Cons a b)


--------------------------------------------------
-- BINARY TREE STYLE DATA STRUCTURE
--------------------------------------------------

data MyTree a = Leaf a | Branch (MyTree a) (MyTree a) | DeadLeaf
    deriving (Show, Eq)

mySize :: MyTree a -> Int
mySize (Leaf _) = 1
mySize (Branch left right) = mySize(left) + mySize(right)

myMax :: (Ord a) => MyTree a -> a
myMax (Leaf a) = a
myMax (Branch left right) = max (myMax left) (myMax right)

myDepth :: MyTree a -> Int
myDepth (Leaf _) = 0
myDepth (Branch left right) = max ((myDepth left) + 1) ((myDepth right) + 1)

myMap :: MyTree a -> (a -> b) -> MyTree b
myMap (Leaf a) f = Leaf (f a)
myMap (Branch left right) f = Branch (myMap left f) (myMap right f)