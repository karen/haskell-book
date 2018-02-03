import Control.Applicative (liftA3)

stops, vowels :: String
stops = "pbtdkg"
vowels = "aeiou"

-- or: (\s v s' -> (s, v, s'))
combos :: [a] -> [b] -> [c] -> [(a, b, c)]
combos = liftA3 ((,,))

-- fmap (,,) stops causes the function to be embedded within the functor, f (= [])
-- fmap (,,) stops <*> vowels <*> stops   -> [(_, _, _)]
-- f (b -> c -> d)      f b        f c    -> f d
-- where f = []
