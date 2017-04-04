module Applicatives where

import Data.Monoid
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

data MyList a = Nil | Cons a (MyList a) deriving (Eq, Show)

instance Monoid (MyList a) where
    mempty = Nil
    mappend Nil (Cons x y) = Cons x y
    mappend (Cons x y) Nil = Cons x y
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
append Nil ys = ys
append (Cons x xs) ys = Cons x $ xs `append` ys

fold :: (a -> b -> b) -> b -> MyList a -> b
fold _ b Nil = b
fold f b (Cons h t) = f h (fold f b t)

concat' :: MyList (MyList a) -> MyList a
concat' = fold append Nil

flatMap :: (a -> MyList b) -> MyList a -> MyList b
flatMap f as = concat' $ fmap f as

main :: IO ()
main = do
    quickBatch $ monoid $ (undefined :: MyList Int)
    quickBatch $ applicative $ (undefined :: MyList (String, Int, Double))