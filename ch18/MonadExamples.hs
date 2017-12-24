import Control.Monad(join)

sequencing :: IO ()
sequencing = do
    putStrLn "blah"
    putStrLn "foo"

sequencing' :: IO ()
sequencing' =
    putStrLn "blah" >>
    putStrLn "foo"

binding :: IO ()
binding = do
    name <- getLine
    putStrLn name

binding' :: IO ()
binding' =
    getLine >>= putStrLn

bindingAndSequencing :: IO ()
bindingAndSequencing = do
    putStrLn "Name: "
    name <- getLine
    putStrLn ("Hello, " ++ name)

bindingAndSequencing' :: IO ()
bindingAndSequencing' =
    putStrLn "Name: " >>
    getLine >>=
        \name -> putStrLn ("Hello, " ++ name)

twiceWhenEven :: [Integer] -> [Integer]
twiceWhenEven xs = do
    x <- xs
    if even x
        then [x*x, x*x]
        else []

f :: Maybe Integer
f = Just 1

g :: Maybe String
g = Nothing

h :: Maybe Integer
h = Just 10191

zed :: a -> b -> c -> (a,b,c)
zed = (,,)

-- can be rewritten using applicative
doSomething :: Maybe (Integer, String, Integer)
doSomething = do
    a <- f
    b <- g
    c <- h
    return (zed a b c)

doSomethingApplicative :: Maybe (Integer, String, Integer)
doSomethingApplicative = pure zed <*> f <*> g <*> h

-- returns a value with more monadic structure
-- we need `join`
zed' :: Monad m => a -> b -> c -> m (a, b, c)
zed' a b c = return (a,b,c)

doSomething' = do
    a <- f
    b <- g
    c <- h
    zed' a b c

foo :: (Maybe (Integer, String, Integer))
foo = join $ zed' <$> f <*> g <*> h
-- foo = join $ pure zed' <*> f <*> g <*> h