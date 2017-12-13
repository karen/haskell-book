import Data.Monoid

newtype Mem s a =
    Mem {
        runMem :: s -> (a,s)
    }

instance Monoid a => Monoid (Mem s a) where
    mempty = Mem $ \s -> (mempty, s)
    mappend Mem{runMem=f} Mem{runMem=g} = Mem $ comb f g

comb :: Monoid a => (s -> (a,s)) -> (s -> (a,s)) -> s -> (a,s)
comb f g x = (a1 <> a2, s2) where
    (a1, s1) = f x
    (a2, s2) = g s1

f' = Mem $ \s -> ("hi", s + 1)

main = do
    print $ runMem (f' <> mempty) 0
    print $ runMem (mempty <> f') 0
    print $ (runMem mempty 0 :: (String, Int))
    print $ runMem (f' <> mempty) 0 == runMem f' 0
    print $ runMem (mempty <> f') 0 == runMem f' 0
