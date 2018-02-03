myIterate :: (a -> a) -> a -> [a]
myIterate f x = x : myIterate f (f x)

myUnfoldr :: (b -> Maybe (a, b)) -> b -> [a]
myUnfoldr f x = case f x of
    Nothing -> []
    (Just (fs, sn)) -> fs : myUnfoldr f sn

betterIterate :: (a -> a) -> a -> [a]
betterIterate f x = myUnfoldr (\x -> Just (x, f x)) x

data BinaryTree a = Leaf | Node (BinaryTree a) a (BinaryTree a)
    deriving (Eq, Ord, Show)

unfold :: (a -> Maybe (a,b,a)) -> a -> BinaryTree b
unfold f x = case f x of
    Nothing -> Leaf
    (Just (l, curr, r)) -> Node (unfold f l) curr (unfold f r)

treeBuild :: Integer -> BinaryTree Integer
treeBuild n = unfold go 0 where
    go x = case x == n of
            True -> Nothing
            False -> Just (x + 1, x , x + 1)

-- treeBuild 3
-- Node (Node (Node Leaf 2 Leaf)
--             1
--             (Node Leaf 2 Leaf))
--      0
--      (Node (Node Leaf 2 Leaf)
--             1
--             (Node Leaf 2 Leaf))
