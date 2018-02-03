{-# LANGUAGE FlexibleInstances #-}

newtype Mu f = InF {outF :: f (Mu f)}

data Sum b a = First a | Second b

instance Functor (Sum e) where
    fmap f (First a) = First (f a)
    fmap f (Second b) = Second b

data Company a c b = DeepBlue a c | Something b

instance Functor (Company e e') where
    fmap f (Something b) = Something (f b)
    fmap _ (DeepBlue a c) = DeepBlue a c

data More b a = L a b a | R b a b deriving (Eq, Show)

instance Functor (More x) where
    fmap f (L a b a') = L (f a) b (f a')
    fmap f (R b a b') = R b (f a) b'

data Quant a b = Finance | Desk a | Bloor b

instance Functor (Quant a) where
    fmap _ Finance = Finance
    fmap f (Desk a) = Desk a
    fmap f (Bloor b) = Bloor $ f b

data K a b = K a

instance Functor (K a) where
    fmap _ (K a) = K a

-- Type Constructor Flip has 3 type arguments
-- Each argument is a polymorphic type variable
-- Flip :: (* -> * -> *) -> * -> * -> *
-- Data Constructor Flip takes 1 argument
-- Flip :: f b a -> Flip f a b
-- View (f b a) as one value, i.e. we match on Flip x
-- f is a function that takes in two arguments
newtype Flip f a b =
    Flip (f b a)
    deriving (Eq, Show)

newtype K' a b = K' a deriving (Eq, Show)

instance Functor (Flip K' a) where
    fmap f (Flip (K' a)) = Flip (K' (f a))

data EvilGoateeConst a b = GoatyConst b

instance Functor (EvilGoateeConst a) where
    fmap f (GoatyConst a) = GoatyConst (f a)

data LiftItOut f a = LiftItOut (f a)

instance Functor f => Functor (LiftItOut f) where
    fmap g (LiftItOut x) = LiftItOut (fmap g x)

data Parappa f g a = DaWrappa (f a) (g a)

instance (Functor f, Functor g) => Functor (Parappa f g) where
    fmap h (DaWrappa fa ga) = DaWrappa (fmap h fa) (fmap h ga)

data IgnoreOne f g a b = IgnoringSomething (f a) (g b)

instance (Functor g) => Functor (IgnoreOne f g a) where
    fmap h (IgnoringSomething fa gb) = IgnoringSomething (fa) (fmap h gb)

data Notorious g o a t = Notorious (g o) (g a) (g t)

instance (Functor g) => Functor (Notorious g o a) where
    fmap f (Notorious go ga gt) = Notorious (go) (ga) (fmap f gt)

data List a = Nil | Cons a (List a)

instance Functor List where
    fmap _ Nil = Nil
    fmap f (Cons x xs) = Cons (f x) (fmap f xs)

data GoatLord a = NoGoat | OneGoat a | MoreGoats (GoatLord a) (GoatLord a) (GoatLord a)

instance Functor GoatLord where
    fmap _ NoGoat = NoGoat
    fmap f (OneGoat x) = OneGoat $ f x
    fmap f (MoreGoats x y z) = MoreGoats (fmap f x) (fmap f y) (fmap f z)

data TalkToMe a = Halt | Print String a | Read (String -> a)

instance Functor TalkToMe where
    fmap _ Halt = Halt
    fmap f (Print x y) = Print x (f y)
    fmap f (Read fn) = Read (fmap f fn)
