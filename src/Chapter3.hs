module Chapter3
    ( module Chapter3
    )
where

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
myDropWhile (Cons x xs) f
    | f(x) = myDropWhile xs f
    | otherwise = Cons x xs

myInit :: MyList a -> MyList a
myInit Empty = Empty
myInit (Cons x (Cons y Empty)) = Cons x Empty
myInit (Cons x xs) = Cons x (myInit xs)
