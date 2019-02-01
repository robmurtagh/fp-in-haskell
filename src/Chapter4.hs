module Chapter4
    ( module Chapter4
    )
where

--------------------------------------------------
-- 'MAYBE' STYLE DATA STRUCTURE
--------------------------------------------------

data MyOption a = Some a | None
    deriving (Show, Eq)

myMap :: MyOption a -> (a -> b) -> MyOption b
myMap None     _ = None
myMap (Some a) f = Some (f a)

myFlatMap :: MyOption a -> (a -> MyOption b) -> MyOption b
myFlatMap None     _ = None
myFlatMap (Some a) f = f a

myGetOrElse :: MyOption a -> a -> a
myGetOrElse None     dflt = dflt
myGetOrElse (Some a) _    = a

myOrElse :: MyOption a -> MyOption a -> MyOption a
myOrElse None     dflt = dflt
myOrElse (Some a) _    = Some a

myFilter :: MyOption a -> (a -> Bool) -> MyOption a
myFilter None _ = None
myFilter (Some a) f | f a       = Some a
                    | otherwise = None

myMean :: (Fractional a) => [a] -> MyOption a
myMean [] = None
myMean xs = Some $ (foldl (+) 0 xs) / (foldl (\b a -> b + 1) 0 xs)

myVariance :: (Fractional a) => [a] -> MyOption a
myVariance [] = None
myVariance xs =
    myFlatMap (myMean xs) (\m -> myMean (map (\x -> (x - m) ^ 2) xs))

-- If either return None, this returns None
myMap2 :: MyOption a -> MyOption b -> (a -> b -> c) -> MyOption c
myMap2 a b f = myFlatMap a (\aa -> myMap b (\bb -> f aa bb))

mySequence :: [MyOption a] -> MyOption [a]
mySequence [] = Some []
mySequence xs = foldr (\b a -> myMap2 b a (:)) (Some []) xs

myTraverse :: [a] -> (a -> MyOption b) -> MyOption [b]
myTraverse []       _ = Some []
myTraverse (x : xs) f = myMap2 (f x) (myTraverse xs f) (:)
