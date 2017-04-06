module Applicatives where

import Control.Applicative
import Data.Monoid
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

data MyList a = Nil | Cons a (MyList a) deriving (Eq, Show)

instance Monoid (MyList a) where
    mempty = Nil
    mappend Nil l = l
    mappend l Nil = l
    mappend (Cons x y) (Cons x' y') = Cons x (y `mappend` Cons x' y')

instance Functor MyList where
    fmap _ Nil = Nil
    fmap f (Cons x y) = Cons (f x) (f <$> y)

instance Applicative MyList where
    pure x        = Cons x Nil
    (<*>) Nil _   = Nil
    (<*>) _   Nil = Nil
    (<*>) (Cons f g) (Cons x y) = Cons (f x) (Cons f Nil <*> y) `mappend` (g <*> Cons x y)

instance (Arbitrary a) => Arbitrary (MyList a) where
     arbitrary = (Cons <$> arbitrary <*> oneof [arbitrary, return Nil])

instance (Eq a) => EqProp (MyList a) where (=-=) = eq

append :: MyList a -> MyList a -> MyList a
append Nil ys         = ys
append (Cons x xs) ys = Cons x $ xs `append` ys

fold :: (a -> b -> b) -> b -> MyList a -> b
fold _ b  Nil       = b
fold f b (Cons h t) = f h (fold f b t)

concat' :: MyList (MyList a) -> MyList a
concat' = fold append Nil

flatMap :: (a -> MyList b) -> MyList a -> MyList b
flatMap f as = concat' $ fmap f as

take' :: Int -> MyList a -> MyList a
take' 0 _           = Nil
take' _ Nil         = Nil
take' n (Cons v vs) = Cons v $ take' (n-1) vs

repeat' :: a -> MyList a
repeat' x = Cons x $ repeat' x

zipWith' :: (a-> b -> c) -> MyList a -> MyList b -> MyList c
zipWith' f (Cons x x') (Cons y y') = Cons (f x y) $ zipWith' f x' y'
zipWith' _ _      _      = Nil

newtype MyZipList a = MyZipList (MyList a) deriving (Eq, Show)

instance Eq a => EqProp (MyZipList a) where
    xs =-= ys = xs' `eq` ys'
        where xs' = let (MyZipList l) = xs
                    in take' 3000 l
              ys' = let (MyZipList l) = ys
                    in take' 3000 l

instance Functor MyZipList where
    fmap f (MyZipList xs) = MyZipList $ fmap f xs

instance Applicative MyZipList where
    pure x = MyZipList $ repeat' x
    (<*>) (MyZipList fs) (MyZipList xs) = MyZipList $ zipWith' ($) fs xs
    
instance (Arbitrary a) => Arbitrary (MyZipList a) where
    arbitrary = MyZipList <$> arbitrary

main :: IO ()
main = do
    quickBatch $ monoid $ (undefined :: MyList Int)
    quickBatch $ applicative $ (undefined :: MyList (String, Int, Double))
    quickBatch $ applicative $ (undefined :: MyZipList (String, Int, Double))