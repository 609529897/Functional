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

## 带状态计算的优雅表示

State Monad

```haskell
-- import Control.Monad.State
newtype State s a = State { runState :: s -> (a, s) }

instance Monad (State s) where
    return x = State $ \s -> (x, s)
    (State h) >>= f = State $ \s -> let (a, newState) = hs
                                        (State g) = f a
                                    in g newState
```

## Error

```haskell
instance (Error e) => Monad (Either e) where
    return x = Right x
    Right x >>= f = f x
    Left err >>= f = Left err
    fail msg = Left (strMsg msg)
```


## Monad 式函数

### liftM: Monad 版 fmap

```haskell
liftM :: (Monad m) => (a -> b) -> m a -> m b

liftM f m = m >>= (\x -> return (f x))

-- liftM f m = do
--    x <- m
--    return (f x)
```

- ap 函数

```haskell
ap :: (Monad m) => m (a -> b) -> m a -> m b
ap mf m = do
    f <- mf
    x <- m
    return (f x)
```

### join 函数: 铺平嵌套 Monad

```haskell
join :: (Monad m) => m (m a) -> m a
```

### filterM

```haskell
filterM :: (Monad m) => (a -> m Bool) -> [a] -> m [a]
```

### foldM

```haskell
foldM :: (Monad m) => (a -> b -> m a) -> a -> [b] -> m a
```

## 组合 Monad 式的函数

-  `<=<`

- return

## 创建 Monad

