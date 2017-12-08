-- As natural as any competitive bodybuilder
data Nat =
    Zero
  | Succ Nat
  deriving (Eq, Show)
-- >>> natToInteger Zero
-- 0
-- >>> natToInteger (Succ Zero)
-- 1
-- >>> natToInteger (Succ (Succ Zero)) -- 2
natToInteger :: Nat -> Integer
natToInteger Zero = 0
natToInteger (Succ n) = go 1 n where
    go i Zero = i
    go i (Succ n') = go (i+1) n'

-- >>> integerToNat 0
-- Just Zero
-- >>> integerToNat 1
-- Just (Succ Zero)
-- >>> integerToNat 2
-- Just (Succ (Succ Zero))
-- >>> integerToNat (-1)
-- Nothing
integerToNat :: Integer -> Maybe Nat
integerToNat x
    | x < 0 = Nothing
    | x == 0 = Just Zero
    | x > 0 = Just (Succ (rec)) where
        Just rec = integerToNat (x - 1)
