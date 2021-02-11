# monad

- Functor

```haskell
class Functor f where
    fmap :: (a -> b) -> f a -> f b
```

- Applicative

```haskell
class (Functor f) => Applicative f where
    pure :: a -> f b
    (<*>) :: f (a -> b) -> f a -> f b
```

- Monad

```haskell
class Monad m where
    return :: a -> m a

    (>>=) :: m  -> (a -> m b) -> m b

    (>>) :: m a -> m b -> m b
    x >> y = x >>= \_ -> y

    fail :: String -> m a
    fail msg = error msg
```

## do 记法

```haskell
foo :: Maybe String
foo = Just 3 >>= (\x ->
      Just "!" >>= (\y ->
      Just (show x ++ y)))


foo :: Maybe String
foo = do
    x <- Just 3
    y <- Just "!"
    Just (show x ++ y)
```

## Monad 定律

1. 左单位元

```haskell
return x >>= f -- 等价于 f x
```

2. 右单位元

```haskell
m >>= return -- 和 m 没有差别
```

3. 结合律

```haskell
(m >>= f) >>= g -- 等价于 m >>= (\x -> f x >>= g)

(<=<) :: (Monad m) => (b -> m c) -> (a -> m b) -> (a -> m c)
f <==< g = (\x -> g x >>= f)
```
