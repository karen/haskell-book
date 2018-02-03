import Control.Monad.Reader

foo :: Reader String String
foo = do
    env <- ask
    return ("foo: " ++ env)

bar :: Reader String String
bar = do
    env <- ask
    return ("bar: " ++ env)

foobar :: Reader String String
foobar = do
    f <- foo
    b <- bar
    return $ f ++ b

main = print . runReader foobar $ "env"
