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
myMap None _ = None
myMap (Some a) f = Some (f a)

myFlatMap :: MyOption a -> (a -> MyOption b) -> MyOption b
myFlatMap None _ = None
myFlatMap (Some a) f = f a
