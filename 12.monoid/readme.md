# monoid

## newtype

按照已有类型创建新类型，比 data 更快

```haskell
newtype ZipList a = ZipList { getZipList :: [a] }
```

用 newtype 创建类型类的实例

```haskell
newtype Pair b a = Pair { getPair :: (a, b) }

instance Functor (Pair c) where
    fmap f (Pair (x, y)) = Pair (f x, y) 
```

type, newtype, data 三者对比

type: 类型别名，更加具有描述性

newtype: 使用现有类型创建新类型，而且这两个类型是完全不同的类型，可以想象成是是只有一个值构造器和一个字段的 data 声明

data: 自定义类型，创建自己的类型

## monoid

结合律：当结合的顺序发生改变时结果并不会改变

```haskell
(3 * 2) * (8 * 2)

3 * (2 * (8 * 5))
```

```haskell
"la" ++ ("di" ++ "da")

("la" ++ "di") ++ "da"
```

monoid 是由满足结合律的二元函数和单位元组成。一个值被称为某个函数的单位元，表示这个值和其他任何参数做运算时，结果总是那个参数

比如 `*` 里的 1 和 `++` 里的 `[]`

```haskell
class Monoid m where
  mempty :: m
  mappend :: m -> m -> m
  mconcat :: [m] -> m
  mconcat = foldr mappend mempty
```

定律

```haskell
mempty `mappend` x = x
x `mappend` mempty = x
(x `mappend` y) `mappend` z = x `mappend` (y `mappend` z)
```

## 一些 monoid

