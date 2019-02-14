{-# LANGUAGE FlexibleInstances #-}

module Chapter10
    ( module Chapter10
    )
where

import           Data.Monoid             hiding ( Sum
                                                , Product
                                                , Endo
                                                )

-- MONOID - ASSOCIATIVE OPERATION HAVING AN IDENTITY ELEMENT
-- https://en.wikibooks.org/wiki/Haskell/Monoids
-- "integer numbers form a monoid under addition with 0 as identity element"
-- mappend === (<>)

-- My own Monoid typeclass, with a slightly different interface
class MyMonoid a where
    op :: a -> a -> a
    zero :: a

-- Make lists instance of my own Monoid typeclass
instance MyMonoid [a] where
    op x y = x ++ y
    zero = []

-- Monoid under addition
newtype Sum a = Sum a
    deriving (Show,Eq)

instance Num a => Monoid (Sum a) where
    -- mappend will automatically be set to <> from Semigroup
    mempty = Sum 0

instance Num a => Semigroup (Sum a) where
    Sum a <> Sum b = Sum (a + b)

-- Monoid under addition
newtype Product a = Product a
    deriving (Show,Eq)

instance Num a => Monoid (Product a) where
    mempty = Product 0

instance Num a => Semigroup (Product a) where
    Product a <> Product b = Product (a * b)

-- An endofunction is a function having the same argument and return type
newtype EndoFunc a = EndoFunc { appEndo :: a -> a } -- this record syntax gives a handy way of extracting the value
-- data EndoFunc a = EndoFunc (a -> a)

instance Semigroup (EndoFunc a) where
    EndoFunc f <> EndoFunc g = EndoFunc (f . g)

instance Monoid (EndoFunc a) where
    mempty = EndoFunc id
