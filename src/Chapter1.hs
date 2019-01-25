module Chapter1 (
    module Chapter1
) where

fib :: Int -> Int
fib n = f n 0 1
    where
        f :: Int -> Int -> Int -> Int
        f 1 prev _ = prev
        f n prev cur = f (n-1) cur (prev + cur)

isSorted :: [a] -> (a -> a -> Bool) -> Bool
isSorted [] _ = True
isSorted [x] _ = True
isSorted (x:y:ys) f = (f x y) && isSorted (y:ys) f