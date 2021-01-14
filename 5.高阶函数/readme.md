# 高阶函数

接受的参数和返回值可以是函数的函数被称为 **高阶函数**

## 柯里函数

函数柯里化

```haskell
multThree :: Int -> Int -> Int -> Int

-- 等价于

multThree :: Int -> (Int -> (Int -> Int))
```

```haskell
let multTwoWithNice = multThree 9
multTwoWithNice 1 2
```

### 截断（section）

可以截断中缀函数，从而柯里化使用

```haskell
divideByTen :: (Floating a) => a -> a
divideByTen = (/10)

divideByTen 10 -- 1.0
```

```haskell
isUpperAlphanum :: Char -> Bool
isUpperAlphanum = (`elem`, ['A'..'Z'])
```

> (-4) 并不是截断函数，可以是 (subtract 4) 代替

### 打印函数

## 更多高阶函数

> 定义高阶函数的类型时，会使用圆括号括起来这样就表示接受的参数为函数

appleTwice

```haskell
appleTwice :: (a -> a) -> a -> a
appleTwice f x = f (f x)

-- 使用
appleTwice (+3) 10 -- 16
appleTwice (++ "HAHA") "HEY" -- "HAHA HAHA HEY"
```

### 实现 zipWith

```haskell
zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' _ [] = []
zipWith' _ _ [] = []
zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys

-- 使用

zipWith' (*) [1, 2] [1, 2] -- [1, 4]
```

### 实现 flip

```haskell
flip' :: (a -> b -> c) -> (b -> a -> c)
flip' f y x = f x y
```

## 函数工具箱

### map

```haskell
map :: (a -> b) -> [a] -> [b]
map _ = []
map f (x:xs) = f x: map f xs

map (+6) [1, 2, 3] -- [7, 8, 9]
[x + 6 | x <- [1, 2, 3]] -- 使用列表推导式实现
```

### filter

```haskell
filter :: (a -> Bool) -> [a] -> [a]
filter _ [] = []
filter p (x:xs)
    | p x = x: filter p xs  -- p x True 的话加入到列表
    | otherwise = filter p xs

filter (>3) [1, 2, 4] -- [4]
```

使用 filter 实现 quciksort

```haskell
quciksort :: (Ord a) => [a] -> [a]
quciksort [] = []
quciksort (x:xs) =
    let smallOrSqual = filter (<= x) xs
        larger = filter (> x) xs
    in quciksort smallOrSqual ++ [x] ++ quciksort larger
```

> where p x = x `mod` 200 == 0 -- 定义函数

### 克拉兹序列

定义

- 从任意自然数开始

- 如果是 1，停止

- 如果是偶数，将它除以 2

- 如果是奇数，将它乘 3 然后加 1

- 取所得的结果，重复上述的算法

问题：分别以 1 到 100 之间的所有数作为数起始数，有多少克兹拉链长度大于 15？

```haskell
chian :: Integer -> [Integer]
chian 1 = [1]
chian n
    | even n = n:chian (n `div` 2)
    | odd n = n:chian (n * 3 + 1)

numLongChians :: Int
numLongChians = length (filter isLong (map chian [1..100])
    where isLong xs = length xs > 15
```

多参数函数映射的话就跟柯里化差不多，就是延迟调用了

```haskell

let listOfFuns = (*) [0..] -- 类似于 [*0, *1, *4..]
(listOfFuns !! 4) 5 -- 20
```

## lambda

一次性的匿名函数

声明时使用 `\` 符号，参数使用 `->`

重写 numLongChain:

```haskell
numLongChians = length (filter (\xs -> length xs > 15) (map chian [1..100])
```

```haskell
-- 取多参
zipWith (\a b -> (a * 30 + 3) / b) [5, 4, 3, 2, 1] [1, 2, 3, 4, 5]
```

```haskell
-- 模式匹配
map (/a b -> a + b) [(1, 2), (3, 4)]
```

## 折叠纸鹤

折叠允许我们将一个数据结构归纳成单个值

组成

- 一个二元函数（参数是两个的函数）

- 初始值

- 待折叠的数据结构

### foldl 左折叠

```haskell
sum' :: (Num a) => [a] -> a
sum' xs = foldl (\acc x -> acc + x) 0 xs
sum' [1, 2, 3] -- 6
```

```haskell
sum' :: (Num a) => [a] -> a
sum' = foldl (+) 0
```

> 善用柯里化：比如 foo a = bar b a 这种函数直接可以写成 foo = bar b

### foldr 右折叠

- 功能跟 foldl 一样，二元函数的参数顺序有差别，第一个是当前值，第二个是累加值

实现 map

```haskell
map' :: (a -> b) -> [a] -> [b]
map' f xs = folder (\x acc -> f x : acc) [] xs
```

> 因为头部添加的效率 `:` 比尾部追加 `++` 的效率大的多，所以一般生成列表时使用 `:`

> 左折叠不可以处理无限列表，右折叠可以

```haskell
elem' :: (Eq a) = a => [a] -> Bool
elem' y ys = foldr (\x acc -> if x === y then True else acc) False ys
```

### foldll 和 foldrl 函数

跟 `foldl` 和 `foldr` 相似，区别是不需要明确的初始值，会直接把第一个值（最后一个）作为初始值

实现 maximum

```haskell
maximum' :: (Ord a) => [a] -> a
maximum' = foldll max
```

### 例子

实现 reverse

```haskell
reverse' :: [a] -> [a]
reverse' = foldl (flip(:)) []
```

```haskell
product' :: (Num a) => [a] -> a
product' foldl (*) 1
```

filter

```haskell
filter' :: (a -> Bool) -> [a] -> [a]
filter' p = folder (\x acc -> if p x then x : acc else acc) []
```

last

```haskell
last' :: [a] -> a
last' = foldll (\_ x -> x)
```

### 理解 fold

```haskell
foldl (\acc x -> if x > 10 then x:acc else acc) 0 []
f 3 (f 4 (f 5 (f 6 z )) -- 类似于这种思路去理解，套娃调用
```

### 无限列表的折叠

`and` 函数，接受布尔值组成的列表一旦列表存在 False 返回 False 不然返回 True

```haskell
and' :: [Bool] -> Bool
and' xs = foldr (&&) True xs

--

(xs !! 2) && (( xs !! 1) && ((xs !! 0) && True))
```

> 惰性求值：只有单元真正使用的时候才分配内存

### 扫描（scanl 和 scanr）,（scanll 和 scanrl）

和 fold 相似，不过扫描会把每一次函数执行的结果放到列表并返回，初始值也会添加其中

```haskell
scanl (+) 0 [1, 2, 3] -- [0, 1, 3, 6]
```

## 有 $ 的函数应用

$: 函数应用符

```haskell
($) :: (a -> b) -> a -> b
f $ x = f x
```

一般的函数调用是左结合的而，`$` 是右结合的

```haskell
(((f a) b) c) -- 左结合
(c (b (f a))) -- 右结合
```

```haskell
sum (map sqrt [1..130])
sum $ map sqrt [1..130]
```

```haskell
sum (filter (> 10) (map (*2) [2..10]))
sum $ filter (>10) $ map (*2) [2..10]
```

`$` 还可以把函数应用转换成函数

```haskell
-- 取一个函数并应用到 3 上面
map ($ 3) [(4+), (10*)] -- [7.0, 30]
```

## 函数组合 function composition

```haskell
(.) :: (b -> c) -> (a -> b) -> a -> c
f . g = \x -> f (g x)
```

```haskell
map (\xs -> negate (sum (tail xs))) [1..20]
map (negate . sum . tail) [1..20]
```

### 多参函数组合

通过部分应用，使每个函数只剩一个参数就好

```haskell
sum (replicate 5 (max 6.7 8.9))
sum . replicate 5 $ max 6.7 8.9
```

通过找到最里面的函数的参数写下来，在它们的前面加一个 $ ，接着略去其余函数的最后一个参数，通过组合在一起

```haskell
replicate 2 (product (map (*3) (zipWith max [1, 2] [4, 5])))
replicate 2 . product . map (*3) $ zipWith max [1, 2] [4, 5]
```

### Point-Free （pointless）风格

不使用所要处理的值，只合成运算过程。只有运算没有值

```haskell
sum' :: (Num a) => [a] -> a
sum' xs = foldl (+) 0 xs

sum' = foldl (+) 0 -- point free
sum' [1..10]
```

```haskell
fn x = ceiling (negate (tan (cos (max 50 x))))

fn = ceiling . negate . tan . cos . max 50 -- point free
```

```haskell
oddSquareSum :: Integer
oddSquareSum = sum (takeWhile (<10000) (filter odd (map (^2) [1..])))

oddSquareSum = sum . takeWhile (<10000) . filter odd $ map (^2) [1..]
```



