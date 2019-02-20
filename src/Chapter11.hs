{-# LANGUAGE FlexibleInstances #-}

module Chapter11
    ( module Chapter11
    )
where

import           Data.Monoid             hiding ( Sum
                                                , Product
                                                , Endo
                                                )

-- FUNCTOR - CLASS FOR THINGS WHICH CAN BE MAPPED OVER
-- https://en.wikibooks.org/wiki/Haskell/The_Functor_class
-- fmap === (<$>)

class MyFunctor f where
    myFmap :: (a -> b) -> f a -> f b

-- Make List an instance of my custom Functor typeclass
instance MyFunctor [] where
    myFmap = map

-- APPLICATIVE FUNCTOR
-- https://en.wikibooks.org/wiki/Haskell/Applicative_functors
-- <*> = something like "ap" or "Apply" or "Tie fighter"

class (MyFunctor t) => MyApplicative t where
    myPure :: a -> t a
    myAp :: t (a -> b) -> t a -> t b

-- Make Maybe an instance of my custom Applicative typeclass

instance MyFunctor Maybe where
    myFmap _ Nothing = Nothing
    myFmap f (Just x) = Just (f x)

instance MyApplicative Maybe where
    myPure = Just

    myAp Nothing _ = Nothing
    myAp _ Nothing = Nothing
    myAp (Just f) (Just x) = Just (f x)

-- Make List an instance of my custom Applicative typeclass
instance MyApplicative [] where
    myPure x = [x]

    myAp [] _ = []
    myAp _ [] = []
    myAp fs xs = [f x | f <- fs, x <- xs]

-- MONADS
-- (>>=)  :: m a -> (a -> m b) -> m b

-- MONADIC MAYBE
-- https://en.wikibooks.org/wiki/Haskell/Understanding_monads/Maybe

lookup1 :: [(String, String)]
lookup1 = [("A", "AA"), ("B", "BB"), ("C", "CC"), ("D", "DD")]

lookup2 :: [(String, String)]
lookup2 = [("AA", "AAA"), ("BB", "BBB"), ("CC", "CCC"), ("DD", "DDD")]

lookup3 :: [(String, String)]
lookup3 = [("AAA", "AAAA"), ("BBB", "BBBB"), ("CCC", "CCCC"), ("DDD", "DDDD")]

myLookup :: (Eq a) => a -> [(a, b)] -> Maybe b
myLookup _ [] = Nothing
myLookup key ((x, y) : xys) | key == x  = Just y
                            | otherwise = myLookup key xys

myTripleLookup :: String -> Maybe String
myTripleLookup field1 =
    myLookup field1 lookup1
        >>= (\field2 -> myLookup field2 lookup2)
        >>= (\field3 -> myLookup field3 lookup3)

myDoTripleLookup :: String -> Maybe String
myDoTripleLookup field1 = do
    field2 <- myLookup field1 lookup1
    field3 <- myLookup field2 lookup2
    myLookup field3 lookup3

-- MONADIC LISTS
-- https://en.wikibooks.org/wiki/Haskell/Understanding_monads/List
-- xs >>= f = [y | x <- xs, y <- f x]

myGenerator :: [String] -> Int -> [String]
myGenerator xs n = (xs >>= replicate n)
