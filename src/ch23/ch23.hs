import Control.Monad.Trans.State

get' :: State s s
get' = state $ \s -> (s, s)

put' :: s -> State s ()
put' s = state $ \s -> ((), s)

exec :: State s a -> s -> s
exec sa s = snd (runState sa) s

eval :: State s a -> s -> a
eval sa s = fst (runState sa) s

modify'' :: (s -> s) -> State s ()
modify'' f = state $ \s -> ((), f s)
