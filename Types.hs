module Types where

import Control.Applicative
import Control.Monad ((>=>), liftM)
import Data.Monoid
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

-------------------------- MyList -------------------------
-----------------------------------------------------------
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
    pure x                      = Cons x Nil
    (<*>) (Cons f g) (Cons x y) = Cons (f x) (Cons f Nil <*> y) `mappend` (g <*> Cons x y)
    (<*>)     _       _         = Nil

instance Foldable MyList where
    foldMap _ Nil = mempty
    foldMap f (Cons x y) = f x `mappend` foldMap f y

instance Traversable MyList where
    traverse _ Nil = pure Nil
    traverse f (Cons x y) = Cons <$> f x <*> (traverse f y) 

instance (Arbitrary a) => Arbitrary (MyList a) where
     arbitrary = (Cons <$> arbitrary <*> oneof [arbitrary, return Nil])

instance (Eq a) => EqProp (MyList a) where (=-=) = eq

append' :: MyList a -> MyList a -> MyList a
append' Nil ys         = ys
append' (Cons x xs) ys = Cons x $ append' xs ys

fold' :: (a -> b -> b) -> b -> MyList a -> b
fold' _ b  Nil       = b
fold' f b (Cons h t) = f h (fold' f b t)

concat' :: MyList (MyList a) -> MyList a
concat' = fold' append' Nil

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
zipWith' _      _           _      = Nil

------------------------ MyZipList ---------------------------
--------------------------------------------------------------
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

--------------------------- MySum -------------------------
-----------------------------------------------------------
data MySum a b = MyFirst a | MySecond b deriving (Eq, Show)

instance Functor (MySum a) where
    fmap _ (MyFirst x)  = MyFirst x
    fmap f (MySecond x) = MySecond (f x)

instance Applicative (MySum a) where
    pure = MySecond
    (MyFirst a) <*> _  = MyFirst a
    (MySecond f) <*> v = f <$> v

instance Monad (MySum a) where
    return = pure
    (MyFirst x)  >>= _  = MyFirst x
    (MySecond x) >>= k  = k x

instance (EqProp a, EqProp b) => EqProp (MySum a b) where
    (=-=) (MyFirst x)  (MyFirst y)  = x =-= y
    (=-=) (MySecond x) (MySecond y) = x =-= y
    (=-=)      _            _       = property False

instance (Arbitrary a, Arbitrary b) => Arbitrary (MySum a b) where
     arbitrary = oneof [liftM MyFirst arbitrary, liftM MySecond arbitrary]
     shrink (MyFirst x)  = [MyFirst  x' | x' <- shrink x ]
     shrink (MySecond y) = [MySecond y' | y' <- shrink y]

------------------- Nope -------------------
--------------------------------------------
data Nope a = NopeDotJpg deriving (Eq, Show)

instance Functor Nope where
    fmap _ NopeDotJpg = NopeDotJpg

instance Applicative Nope where 
    pure   _   = NopeDotJpg
    (<*>) _  _ = NopeDotJpg

instance Monad Nope where
    return     = pure
    (>>=) _ _  = NopeDotJpg

instance (EqProp a) => EqProp (Nope a) where
    (=-=)  _     _  = property True

instance (Arbitrary a) => Arbitrary (Nope a) where
    arbitrary = pure NopeDotJpg

------------------------- MyIdentity -----------------------
------------------------------------------------------------
newtype MyIdentity a = MyIdentity a deriving (Eq, Ord, Show)

instance Functor MyIdentity where
    fmap f (MyIdentity x) = MyIdentity $ f x

instance Foldable MyIdentity where
    foldMap f (MyIdentity x) = f x

instance Traversable (MyIdentity) where
    traverse f (MyIdentity x) = MyIdentity <$> (f x)

instance (EqProp a) => EqProp (MyIdentity a) where
    (=-=)  _     _  = property True

instance (Arbitrary a) => Arbitrary (MyIdentity a) where
    arbitrary = do
        x <- arbitrary
        elements [MyIdentity x]

--------------------------------- Kleisli composition -------------------------------------
----------------- (>=>) :: Monad m => (a -> m b) -> (b -> m c) -> a -> m c ----------------
-------------------------------------------------------------------------------------------
sayHi :: String -> IO String
sayHi greeting = do
    putStrLn greeting
    getLine

readM :: Read a => String -> IO a
readM = return . read

getAge :: String -> IO Int
getAge = sayHi >=> readM

askForAge :: IO Int
askForAge = getAge "Hello! How old are you? "

--------------------------------- MyEither -------------------------------------
--------------------------------------------------------------------------------
data MyEither a b = MyLeft a | MyRight b deriving (Eq, Show, Ord)

instance Functor (MyEither a) where
    fmap _ (MyLeft a)  = MyLeft a
    fmap f (MyRight a) = MyRight $ f a

instance Applicative (MyEither e) where
    pure              = MyRight
    (MyLeft e) <*>  _ = MyLeft e
    (MyRight e) <*> r = fmap e r

instance Foldable (MyEither a) where
    foldMap _ (MyLeft _)  = mempty
    foldMap f (MyRight r) = f r

    foldr _ z (MyLeft _)  = z
    foldr f z (MyRight r) = f r z

    foldl _ z (MyLeft _)  = z
    foldl f z (MyRight r) = f z r

instance Traversable (MyEither e) where
    traverse _ (MyLeft x) = pure (MyLeft x)
    traverse f (MyRight r) = MyRight <$> f r 

instance (EqProp a, EqProp b) => EqProp (MyEither a b) where
    (=-=) (MyLeft x)  (MyLeft y)  = x =-= y
    (=-=) (MyRight x) (MyRight y) = x =-= y
    (=-=)      _            _       = property False

instance (Arbitrary a, Arbitrary b) => Arbitrary (MyEither a b) where
    arbitrary = do
        a <- arbitrary
        b <- arbitrary
        elements [MyLeft a, MyRight b]

---------------------------------- MyConstant -----------------------------------
---------------------------------------------------------------------------------
newtype MyConstant a b = MyConstant { getConstant :: a } deriving (Eq, Ord, Show)

instance Functor (MyConstant a) where
    fmap _ (MyConstant x) = MyConstant x

instance Foldable (MyConstant a) where
    foldMap f (MyConstant x) = mempty f

instance Traversable (MyConstant a) where
    traverse _ (MyConstant x) = pure (MyConstant x)

instance (EqProp a, EqProp b) => EqProp (MyConstant a b) where
     (=-=) (MyConstant x)  (MyConstant y)  = x =-= y

instance (Arbitrary a, Arbitrary b) => Arbitrary (MyConstant a b) where
      arbitrary = do
          x <- arbitrary
          elements [(MyConstant x)]

---------------------------------- MyMaybe --------------------------------
---------------------------------------------------------------------------
data MyMaybe a = Zero | One a deriving (Eq, Ord, Show)

instance Functor MyMaybe where
    fmap _ Zero = Zero
    fmap f (One x) = One $ f x

instance Foldable MyMaybe where
    foldMap _ Zero    = mempty
    foldMap f (One x) = f x

instance Traversable MyMaybe where
    traverse _ Zero    = pure Zero
    traverse f (One x) = One <$> f x

instance (EqProp a) => EqProp (MyMaybe a) where
    (=-=)  Zero     Zero     = property True
    (=-=)  (One x)  (One y)  = x =-= y

instance (Arbitrary a) => Arbitrary (MyMaybe a) where
    arbitrary = do
        x <- arbitrary
        elements [One x]

----------------------------------- MyTree --------------------------------
---------------------------------------------------------------------------
data MyTree a = Empty | Leaf a | Node (MyTree a) a (MyTree a) deriving (Eq, Show)

instance Functor MyTree where
    fmap _ Empty    = Empty
    fmap f (Leaf x) = Leaf (f x)
    fmap f (Node l v r) = Node (f <$> l) (f v) (f <$> r)

instance Foldable MyTree where
    foldMap _ Empty        = mempty
    foldMap f (Leaf x)     = f x
    foldMap f (Node l v r) = foldMap f l `mappend` f v `mappend` foldMap f r

instance Traversable MyTree where
    traverse _ Empty        = pure Empty
    traverse f (Leaf x)     = Leaf <$> f x
    traverse f (Node l v r) = Node <$> traverse f l <*> f v <*> traverse f r

instance (Arbitrary a) => Arbitrary (MyTree a) where
     arbitrary = (Node <$> oneof [arbitrary, Leaf <$> arbitrary, return Empty] 
                       <*> arbitrary <*> oneof [arbitrary, Leaf <$> arbitrary ,return Empty])

instance (Eq a) => EqProp (MyTree a) where (=-=) = eq

main :: IO ()
main = do
    putStrLn "Testing MyList..."
    quickBatch $ monoid      $ (undefined :: MyList (String, Int, Double))
    quickBatch $ applicative $ (undefined :: MyList (String, Int, Double))
    quickBatch $ traversable $ (undefined :: MyList (String, Int, String))
    putStrLn "\n\nTesting MyZipList..."
    quickBatch $ applicative $ (undefined :: MyZipList (String, Int, Double))
    putStrLn "\n\nTesting MySum..."
    quickBatch $ applicative $ (undefined :: MySum String (Int, Double, Char))
    quickBatch $ monad $ (undefined :: MySum String (Int, Double, Char))
    putStrLn "\n\nTesting Nope..."
    quickBatch $ applicative $ (undefined :: Nope (Int, Double, String))
    quickBatch $ monad $ (undefined :: Nope (String, Int, Double))
    putStrLn "\n\nTesting MyEither..."
    quickBatch $ traversable $ (undefined :: MyEither String (Double, Double, [Double]))
    putStrLn "\n\nTesting MyIdentity..."
    quickBatch $ traversable $ (undefined :: MyIdentity (Double, Double, [Double]))
    putStrLn "\n\nTesting MyConstant..."
    quickBatch $ traversable $ (undefined :: MyConstant Double (Double, Double, [Double]))
    putStrLn "\n\nTesting MyMaybe..."
    quickBatch $ traversable $ (undefined :: MyMaybe (Double, Double, [Double]))
    putStrLn "\n\nTesting MyTree"
    quickBatch $ traversable $ (undefined :: MyTree (String, Int, String))
    --putStrLn "\nTesting Kleisli composition"
    --askForAge
    --putStrLn "\nFinished testing Kleisli composition"
