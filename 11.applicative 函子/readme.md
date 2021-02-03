# applicative 函子

## 函子

### IO 函子

```haskell
instance Functor IO where
  fmap f action = do
    result <- action
    return (f result)
```

### 函数作为函子

- 函数作为函子的话就成了 compose（组合）

```haskell
instance Functor ((->) r) where
  fmap = (.)
```

- lifting：可以把 fmap 和 参数柯里化 结合起来，把 fmap 理解成 lifting

- 参数柯里化可以理解为接受一个参数，返回一个函数

- 可以把 fmap 理解为接受一个参数是一个普通的值返回一个普通的值的函数，提升为接受一个函子值返回一个函子值的函数

```haskell
class Functor f where
fmap :: (a -> b) -> (f a -> f b)
```

## 函子定律 functor law

functor 都要求拥有一些性质和行为，必须可靠的表现为可以被映射的东西

一旦满足下面两个定律就可以认为对于映射会有相同的基本行为

我们可以知道当对函子值进行映射的时候不会发生和映射无关的事情

### 定律 1

在函子值上映射 id 函数，返回的函子值应该跟原先的值一样

```haskell
fmap id (Just 3) -- Just 3
```

### 定律 2

两个函数组合起来映射一个函子等于一个函子分别被上面的两个函数映射

> 分配律？

```haskell
fmap (f . g) = fmap f . fmap g

fmap (f . g) x = fmap f $ fmap g x
```

## 使用 applicative 函子

```haskell
class (Functor f) => Applicative f where
    pure :: a -> f b -- pure 接受的是最小上下文
    (<*>) :: f (a -> b) -> f a -> f b
```

Maybe Applicative

```haskell
instance Applicative Maybe where
    pure = Just
    Nothing <*> _ = Nothing
    (Just f) <*> something = fmap f something
```

```haskell
-- <$> fmap 的中缀形式
（<$>） :: (Functor f) => (a -> b) -> f a -> f b
f <$> x = fmap f x
```

一些 applicative 实例

- 列表

```haskell
instance Applicative [] where
    pure x = [x]
    fs <*> xs = [f x | f <- fs, x <- xs]
```

- IO applicative 函子

```haskell
instance Applicative IO where
    pure = return
    a <*> b = do
        f <- a
        x <- b
        return (f x)
```

- 函数作为 applicative

```haskell
instance Applicative ((->) r) where
    pure x = (\_ -> x)
    f <*> g = \x -> f x (g x)
```

- zip 列表

```haskell
instance Applicative ZipList where
  pure x = ZipList (repeat x)
  ZipList fs <*> ZipList xs = ZipList (zipWith (\f x -> f x) fs xs)
```

## Applicative 定律

- `pure f <*> x = fmap f x`

- `pure id <*> v = v`

- `pure (.) <*> u <*> v <*> w = u <*> (v <*> w)`

- `pure f <*> pure x = pure (f x)`

- `u <*> pure y = pure ($ y) <*> u`

## Applicative 实用函数

```haskell
liftA2 :: (Applicative f) => (a -> b -> c) -> f a -> f b -> f c
liftA2 f a b = f <$> a <*> b
```

接受 Applicative 值的列表，返回以列表为结果的 Applicative 值

```haskell
sequenceA :: (Applicative f) => [f a] -> f [a]
sequenceA [] = pure []
sequenceA (x:xs) = (:) <$> x <*> sequenceA xs
```

```haskell
sequenceA :: (Applicative f) => [f a] -> f [a]
sequenceA = foldr (liftA2 (:)) (pure [])
```