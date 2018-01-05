module Foldable where

import Data.Foldable
import Data.Monoid

data Identity a = Identity a

instance Foldable Identity where
    foldr f z (Identity x) = f x z
    foldl f z (Identity x) = f z x
    foldMap f (Identity x) = f x

data Optional a = Nada | Yep a

instance Foldable Optional where
    foldr _ z Nada = z
    foldr f z (Yep x) = f x z

    foldl _ z Nada = z
    foldl f z (Yep x) = f z x

    foldMap _ Nada = mempty
    foldMap f (Yep a) = f a

sum' :: (Foldable t, Num a) => t a -> a
sum' = foldr (+) 0

product' :: (Foldable t, Num a) => t a -> a
product' = foldr (*) 1

elem' :: (Foldable t, Eq a) => a -> t a -> Bool
elem' x = foldr (\x' acc -> acc || x == x') False

minimum' :: (Foldable t, Ord a) => t a -> Maybe a
minimum' = foldr (compareWith (<)) Nothing

maximum' :: (Foldable t, Ord a) => t a -> Maybe a
maximum' = foldr (compareWith (>)) Nothing

compareWith :: (a -> a -> Bool) -> a -> Maybe a -> Maybe a
compareWith op x acc = case acc of Nothing -> Just x
                                   Just x'
                                        | op x x' -> Just x
                                        | otherwise -> Just x'

null' :: (Foldable t) => t a -> Bool
null' = foldr (\x acc -> False) True

length' :: (Foldable t) => t a -> Int
length' = foldr (\x acc -> acc + 1) 0

toList' :: (Foldable t) => t a -> [a]
toList' = foldr (\x acc -> x : acc) []

fold' :: (Foldable t, Monoid m) => t m -> m
fold' = foldMap id

foldMap' :: (Foldable t, Monoid m) => (a -> m) -> t a -> m
foldMap' am = foldr (\x acc -> mappend (am x) acc) mempty

data Constant a b = Constant a
data Two a b = Two a b
data Three a b c = Three a b c
data Three' a b = Three' a b b
data Four' a b = Four' a b b b

instance Foldable (Constant a) where
    foldr _ z _ = z
    foldMap _ _ = mempty

instance Foldable (Two a) where
    foldr f z (Two _ y) = f y z
    foldMap f (Two _ y) = f y

instance Foldable (Three a b) where
    foldr f z (Three _ _ y) = f y z
    foldMap f (Three _ _ y) = f y

instance Foldable (Three' a) where
    foldr f z (Three' _ x x') = f x $ f x' z
    foldMap f (Three' _ x x') = mappend (f x') (f x)

instance Foldable (Four' a) where
    foldr f z (Four' _ a b c) = f a $ f b $ f c z
    foldMap f (Four' _ a b c) = mappend (f a) $ mappend (f c) (f b)

filterF :: (Applicative f, Foldable f, Monoid (f a)) => (a -> Bool) -> f a -> f a
filterF p = foldMap (\x -> if p x then pure x else mempty)