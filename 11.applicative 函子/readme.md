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

