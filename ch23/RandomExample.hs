{-# LANGUAGE InstanceSigs #-}
module RandomExample where

import Control.Applicative (liftA3)
import Control.Monad (replicateM)
import Control.Monad.Trans.State
import System.Random

data Die =
    DieOne
  | DieTwo
  | DieThree
  | DieFour
  | DieFive
  | DieSix
  deriving (Eq, Show)

intToDie :: Int -> Die
intToDie n =
    case n of
        1 -> DieOne
        2 -> DieTwo
        3 -> DieThree
        4 -> DieFour
        5 -> DieFive
        6 -> DieSix
        x -> error $ "intToDie got non 1-6 integer: " ++ show x

rollDie :: State StdGen Die
rollDie = state $ do
    (n, s) <- randomR (1, 6)
    return (intToDie n, s)

rollDie' :: State StdGen Die
rollDie' = intToDie <$> state (randomR (1, 6))

rollDieThreeTimes :: State StdGen (Die, Die, Die)
rollDieThreeTimes = liftA3 (,,) rollDie rollDie rollDie

nDie :: Int -> State StdGen [Die]
nDie n = replicateM n rollDie

rollsToGetN :: Int -> StdGen -> Int
rollsToGetN n = go 0 0 n
    where go :: Int -> Int -> Int -> StdGen -> Int
          go sum count lim gen
            | sum >= lim = count
            | otherwise =
                let (die, nextGen) = randomR (1, 6) gen
                in go (sum + die) (count + 1) lim nextGen

rollsCountLogged :: Int -> StdGen -> (Int, [Die])
rollsCountLogged n = go 0 (0, []) n
    where go :: Int -> (Int, [Die]) -> Int -> StdGen -> (Int, [Die])
          go sum (count, rolls) lim gen
            | sum >= lim = (count, rolls)
            | otherwise =
                let (die, nextGen) = randomR (1, 6) gen
                in go (sum + die) (count + 1, intToDie die : rolls) lim nextGen

newtype Moi s a = Moi { runMoi :: s -> (a, s) }

instance Functor (Moi s) where
    fmap :: (a -> b) -> Moi s a -> Moi s b
    fmap f (Moi g) = Moi $ \x -> let (a, s) = g x
                                 in (f a, s)

instance Applicative (Moi s) where
    pure :: a -> Moi s a
    pure a = Moi $ \s -> (a, s)

    (<*>) :: Moi s (a -> b) -> Moi s a -> Moi s b
    (Moi f) <*> (Moi g) = Moi $ \x -> let (a, s) = g x
                                          (ab, s'') = f s
                                      in (ab a, s'')

instance Monad (Moi s) where
    return = pure

    (>>=) :: Moi s a -> (a -> Moi s b) -> Moi s b
    (Moi f) >>= g = Moi $ \s -> let (a, s') = f s
                                in runMoi (g a) s'