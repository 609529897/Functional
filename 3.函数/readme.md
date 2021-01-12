# 函数

## 模式匹配

```haskell
sayMe :: Int -> String
sayMe 1 = "1"
sayMe 2 = "2"
sayMe 3 = "3"
sayMe 4 = "4"
sayMe 5 = "5"
sayMe x = "Not between 1 and 5" 
-- 万能模式，一般模式匹配的时候都留一个万能模式，这样程序就不会奔溃
```

递归实现阶乘函数

```haskell
factorial :: Int -> Int
factorial 0 = 1
factorial n = n * factorial (n - 1)
```

### 元祖的模式匹配

```haskell
addVectors :: (Double, Double) -> (Double, Double) -> (Double,Double)
addVectors a b = (fst a + fst b, snd a + snd b)
```

上面的元祖写成模式匹配为

```haskell
addVectors (x1, y1)  (x2, y2) = (x1 + x2, y1 + y2)
```

### 列表与列表推导式的模式匹配

```haskell
let xs = [(1, "a"), (2, "b")]
[a + b | (a + b) <- xs]
```

实现 head 函数
```haskell
head' :: [a] -> a
head' [] = error "..."
head' (x:_) = x
```

> 模式匹配无法使用 ++，以为 haskell 无法确定两边的值

### As 模式

```haskell
firstletter :: String -> String
firstletter "" = "Empty string, whoops!"
firstletter all@(x:xs) = "The first letter of" ++ all ++ "is" ++ [x]
```

## 哨位（guard）

类似于 if else 语句，但是是在函数定义阶段就会进行条件判断

```haskell
bmiTell :: Double -> String
bmiTell bmi
    | bmi <= 18.5 = "underweight"
    | bmi <= 25.0 = "normal"
    | bmi <= 30.0 = "fat"
    | otherwise = "whale"
```

实现 max

```haskell
max' = (Ord a) => a -> a -> a
max' a b
     | a < b = b
     | atherwise = a
```

实现 compare

```haskell
compare' :: (Ord a) => a -> a -> Ordering
a `compare'` b
    | a == b = EQ
    | a <= b = LT
    | otherwise = GT
```

> 也可以 a `compare'` b 这样定义函数

## where?!

`where` 可以抽出哨位中的条件运算，从而让代码更加简约和易读

```haskell
bmiTell :: Double -> Double -> String
bmiTell weight height
    | bmi <= skinny = "1"
    | bmi <= normal = "2"
    | bmi <= fat = "3"
    where bmi = weight / height ^ 2
          skinny = 18.5
          normal = 25.0
          fat = 30.0
```

### wehere 作用域

只对本函数可见，可以定义全局 where 这样变量值全局都可以使用

where 中定义的内容不可以在其他模式中访问

# ??? where 好像在模式匹配的时候，只能用在最下面的选项，而用在哨位时可以在任意选项使用变量

```haskell
greet :: String -> String
greet "Juan" = nice ++ "Juan"
greet "Fernando" = niceGreet ++ "Fer"
greet name = niceGreet ++ name
    where nice = "hello"
          niceGreet = "Oh"
```

### where 中模式匹配

```haskell
...
where bmi = weight / height ^ 2
    (skinny, normal, fat)  = (18.5, 25.0, 30.0) 
```

```haskell
initials :: String -> String -> String
initials firstname lastname = [f] ++ ". " ++ [1] ++ "."
    where (f:_) = firstname
          (l:_) = lastname
```

### where 块中的函数

```haskell
calcBmis :: [(Double, Double)] -> [Double]
calcBmis xs = [bmi w h | (w, h) <- xs]
    where bmi weight height = weight / height ^ 2
```

## let

可以在函数体的任意位置定义变量

```haskell
cylinder :: Double -> Double -> Double
cylinder r h =
    let sideArea = 2 * pi * r * h
        topArea = pi * r ^ 2
    in sideArea + 2 * topArea
```

let 可以放到代码的任意位置

```haskell
4 * （let a = 9 in a + 1）+ 2
```

在局部作用域定义函数

```haskell
[let square x = x * x in (square 5, square 3, square 2)]
````

同时绑定多个变量

```haskell
[let a = 100; b = 200; c = 300 in a * b * c]
```

模式匹配

```haskell
(let (a, b, c) = (1, 2, 3) in a + b + c) * 100
```

### 列表推导式中的 let

```haskell
calcBmis :: [(Double, Double)] -> [Double]
calcBmis xs = [bmi | (w, h) <- xs, let bmi = w / h ^ 2, bmi > 25.0]
-- (w, h) <- xs 被称为生成器，在这个题目里在生成器中不可使用 bmi 因为它在 let 定义的前面
```

### GHCI 中的 let

在 GHCI 中定义函数和常量时，可以忽略 in 这样在一个会话的中的随处可以使用定义的语句

```haskell
let zoot x y z = x * y * z
```

## case 表达式

类似模式匹配，其实模式匹配就是 case 表达式的语法糖


```haskell
-- 表达式结构

case expression of pattern -> result
                   pattern -> result
                   pattern -> result

head' :: [a] -> a
head' xs = case xs of [] -> "error"
                      (x:_) -> x
```

模式匹配只能在函数定义时使用，而 case 可以在函数体任意地方使用

```haskell
describeList :: [a] -> String
describeList ls = "The list is" ++ case ls of [] -> "empty"
                                              [x] -> "1"
                                              xs -> "2"
```

```haskell
describeList :: [a] -> String
describeList ls = "The list is" ++ what ls
    where what [] = "empty"
          what [x] = "a singleton list"
          what xs = "a longer list"
```