import Criterion.Main

data Queue a =
    Queue { enqueue :: [a]
          , dequeue :: [a]
          } deriving (Eq, Show)

push :: a -> Queue a -> Queue a
push x (Queue enq deq) = Queue (x:enq) deq

push' :: a -> Queue a -> Queue a
push' x (Queue f r) = check (Queue f (x:r))

pop :: Queue a -> Maybe (a, Queue a)
pop (Queue [] []) = Nothing
pop (Queue enq []) = Just (x, Queue [] xs) where
    x:xs = reverse enq
pop (Queue enq (x:xs)) = Just (x, Queue enq xs)

pop' :: Queue a -> Maybe (a, Queue a)
pop' (Queue [] []) = Nothing
pop' (Queue (x:xs) r) = Just (x, check (Queue xs r))

check :: Queue a -> Queue a
check (Queue [] r) = Queue (reverse r) []
check q = q

alt :: Int -> [Int] -> [Int]
alt n q
    | n `mod` 2 == 0 = n:q
    | otherwise = removeLast q where
        removeLast [] = []
        removeLast [x] = []
        removeLast (x:xs) = x: removeLast xs

altWrapper :: Int -> [Int]
altWrapper i = go i []
    where go 0 xs = xs
          go n xs = go (n-1) $ alt n xs

dbl :: Int -> Queue Int
dbl i = go i (Queue [] [])
    where go 0 q = q
          go n q
            | n `mod` 2 == 0 = go (n-1) $ push n q
            | otherwise = case pop q of
                Nothing -> q
                Just (_, q') -> go (n-1) q'

dbl' :: Int -> Queue Int
dbl' i = go i (Queue [] [])
    where go 0 q = q
          go n q
            | n `mod` 2 == 0 = go (n-1) $ push' n q
            | otherwise = case pop' q of
                Nothing -> q
                Just (_, q') -> go (n-1) q'

main :: IO ()
main = defaultMain
    [ bench "Plain queue" $ whnf altWrapper 1000
    , bench "Queue (Double Lists 1)" $ whnf dbl 1000
    , bench "Queue (Double Lists 2)" $ whnf dbl' 1000]
