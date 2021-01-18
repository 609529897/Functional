# 自定义类型和类型类

使用关键字 `data` 进行自定义类型

```haskell
-- 类型名和值构造器名必须大写
-- False , True 值构造器
data Bool = False | True
```

## 成型

```haskell
-- Circle 值构造器, 本质上就是函数
data Shape = Circle Float Float Float | Rectangle Float Float Float
    deriving (Show) -- 自动放置到 Show 类型类下，Show 类型类的实例类型可以把自身用字符串的形式打印出来

area :: Shape -> Float
area (Circle _ _ r) = pi * r ^ 2
area (Rectangle x1 y1 x2 y2) = (abs $ x2 - x1) * (abs $ y2 - y1)
```

### 优化 Shape

```haskell
data Point = Point Float Float deriving (Show)
data Shape = Circle Point Float | Rectangle Point Point deriving (Show)

area :: Shape -> Float
area (Circle _ r) = pi * r ^ 2
area (Rectangle (Point x1 y1) (Point x2 y2)) = (abs $ x2 - x1) * (abs y2 - y1)
```

导出值构造器

```haskell
module (
  Point(..),
  Shape(..) -- .. 表示会把 Shape 和 Shape 下的所有值构造器导出
  Shape     -- 这样写的话只导出 Shape 类型，不会导出 Shape 类型的值构造器。这样的好处就是形成了一个黑盒，Shape 的作者可以任意修改 Shape 的内部结构，而不对使用者产生影响
) where
```

## 记录语法

好处是是直观，不用关注顺序

```haskell

data Car = Car String String Int deriving (Show) -- 可以写成下面 👇 那样

data Car = Car {
  company :: string,
  model :: string,
  year :: Int
} deriving (Show)

Car {
  company = "Ford",
  model = "Mustang",
  year = 1967
}
```

## 类型参数

同值构造器可以使用值作为参数，类型构造器也可以使用类型作为参数

```haskell
data Maybe a = Nothing | Just a
```

值构造器和类型构造器

```haskell
data Vector a = Vector a a a deriving (Show)
```

## 派生实例

```haskell
data Person = Person {
  firstName :: String,
  lastName :: String,
  age:: Int,
} deriving (Eq)
```

```haskell
-- mikeD 和 jackma 是可以相互比较的
-- 首先会判断它们两个的值构造器是否相等
-- 让后会依次比较每个值，要求是每个值必须是 （Eq）类型类的类型下的值
mikeD = Person { firstName = "mike", lastName = "D", age = 17 }
jackma = Person { firstName = "jack", lastName = "ma", age = 19 }
```

read 可以把某个字符串转换成某个具体的值，带参数的类型也可以但是必须指定参数

```shell
ghci> "Just a" :: Maybe Int
```

对于有多个值构造器的自定义类型来说，前面的值构造器的类型被认为比较起来比较小

实现类似枚举的功能

```haskell
data day = Monday | Tuesday | Wednesday
    deriving (Eq, Ord, Show, Read, Bounded, Enum)
```

## 类型别名