# 模块

模块是指包含函数、类型与类型类的定义的文件

Prelude 自动导入的模块

```haskell
import ModuleName
```

一旦导入模块，那么你可以在代码的任意位置调用这个模块

```haskell
import Data.List

numUniques :: (Eq a) -> [a] -> Int
numUniques = length . nub -- nub 方法是 Data.List 的
```

```shell
ghci> :m + Data.List                     # ghci 导入模块
ghco> :m + Data.List Data.Map Data.Set   # ghci 装载多个模块
```

```haskell
import Data.List (nub, sort)  -- 部分导入
import Data.List hiding (nub) -- 导入除了 nub 之外的 Data.List 里的函数
```

避免函数名冲突, 可以使用 qualified import

函数名冲突：比如 Data.Map 和 Prelude 都有一个 filter，这时调用的话就不知道使用的是那个了

```haskell
import quelified Data.Map      -- 使用 filter: Data.Map.filter
import quelified Data.Map as M -- 取别名, 使用: M.filter
```

> 如何区分限定导入话函数组合的 . ，当点号在限定导入的模块名与函数中间且没有空格时，就视作对模块中函数的引用

## 使用模块中的函数求解问题

`Data.List.words` 字符串统计

```haskell
words "hey these"
["hey", "these"]
```

`Data.List.group` 按值分类（只能相领，不相领可以先排序后 group）

`Data.List.sort` 排序

```haskell
group [1,1,2,3,4,4]
[1,1], [2],[3],[4,4]
```

统计列表中每个值出现的的次数

```haskell
import Data.List

wordNums :: String -> [(String, Int)]
wordNums = map (\ws -> (head ws, length ws)) . group . sort . words
```

### 干草堆中的缝纫针

从一个列表中判断另一个列表是否存在，Data.List 里面也有叫做 `isInfixOf`

待搜索的列表：**干草堆** haystack
使用的列表： **缝纫针** needle

```haskell
import Data.List

isIn :: (Eq a) => [a] -> [a] -> Bool
needle `isIn` haystack = any (needle `isPrefixOf`) (tail haystack)
```

### 凯撒密码沙拉

移动字符在 Unicode 编码中的位置，从而加密

```haskell
import Data.Char

encode :: Int -> String -> String
encode offset msg = map (\c -> chr $ ord c + offset) msg
```

### 严格左折叠

foldl 可能回导致 stack overflow，因为 haskell 的延迟计算，回占据大量的计算空间内存，从而 stack overflow

Data.list 中提供了一种，不延迟计算的 `foldl'` 函数，可以在使用大量的数据折叠是可以使用 `foldl'` 避免 stack overflow

### 寻找酷数

```haskell
Nothing -- 表示列表为空
Just 1  -- 表示列表至少包含 1
Maybe Int -- 表示可能会失败的结果，返回空或者一个值

Maybe Int 等价于 Nothing 和 Just Int 相加
```

寻找第一个值相加后等于 n 的列表

```haskell
firstTo40 :: Int -> Maybe Int
firstTo40 n = find (\x -> digitSum x == n) [1..]
```

## 映射键与值

定义一个函数：接受一个 k 和一个使用序对（k,v）组成的列表，返回第一个满足 k 的 v

```haskell
findkey :: (Eq k) => k -> [(k, v)] -> Maybe v
findkey key [] = Nothing
findkey key ((k,v):xs)
    | key == k = Just v
    | otherwise = findkey key xs
```

使用折叠

```haskell
findkey :: (Eq k) => k -> [(k, v)] -> Maybe v
findkey key xs = foldr (\(k,v) acc -> if key == k then Just v else acc) Nothing xs
```

> Maybe 返回 Nothing (空) 或者 Just a（一个值）

### Data.Map

Data.Map 函数和 Prelude 和 Data.List 可能存在一些命名冲突

```haskell
import quelified Data.Map as Map
```

Map.fromList 会把一个序对组成的列表变成映射，但也有可能会对原有列表产生影响

1. 会对列表的值按序对的 k 值进行排序

2. 如果列表中的序对的 k 值相同，那么就会删除掉其余的元素只留下一个元素

```haskell
import quelified Data.Map as Map
phoneBook :: Map.Map String String
phoneBook = Map.fromList $ [("a","1"),("b", "1")]

Map.lookup "a" phoneBook     -- 获取 "1"
Map.insert "c" "3" phoneBook -- 插入
Map.size phoneBook           -- 检查大小
```
## 构造自己的模块

```haskell
-- 定义模块，并导出部分模块
module Geometry 
(
  sphereVolume,
) where

import Geometry -- 导入模块
````

> 使用 Geometry 的其他模块必须与 Geometry 在同一目录下

### 模块分层

有个叫 Some 的文件夹，文件夹下有三个文件分别叫

```haskell
module Some.a ( x, y ) where
```

```haskell
module Some.b ( n, m ) where
```

使用时

```haskell
import Some.a (x)
import Some.b hiding (m)
import qualified Some.a as A
```