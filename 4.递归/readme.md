# 递归 Recursion

## 不可思议的最大值

```haskell
maximum' :: (Ord a) => [a] -> a
maximum' [] -> error "empty"
maximum' [x] = x
maximum' (x:xs) = max x (maximum' xs)
```

> 列表可以模式匹配成 元组

## 更多递归函数

实现各种内置函数

- replicate

```haskell
replicate' :: Int -> a -> [a]
replicate' n x
    | n <= 0 = []
    | otherwise = x:replicate' (n - 1) x
```

- take

```haskell
take' :: (Num i, Ord i) => i -> [a] -> [a]
take' n _
    | n <= 0   = []
take' _ []     = []
take' n (x:xs) = x : take' (n - 1) xs
```

- revserse

```haskell
reverse' :: [a] -> [a]
reverse' [] = []
reverse' (x:xs) = reverse' xs ++ [x]
```

- repeat

```haskell
repect' :: a -> [a]
repect' x = x: repect' x
```

- zip

```haskell
zip' :: [a] -> [b] -> [(a, b)]
zip' _ [] = []
zip' [] _ = []
zip' (x:xs) (y:ys) = (x, y): zip' xs ys
```

- elem

```haskell
elem' :: (Eq a) => a -> [a] -> bool
elem' a [] = False
elem' a (x: xs)
    | a == x = True
    | otherwise = a `elem'` xs
```

## 快点，排序！

快速排序：取一个基准数，比基准数大的排序到右边，小的排序到左边。让后在按照相同原理对左右的列表进行排序，依次递归下去，最后左右两个列表为空时排序结束，产生结果

```haskell
quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) =
    let smallOrEqual = [a | a <- xs, a <= x]
        larger = [a | a <- xs, a > x]
    in quicksort smallOrEqual ++ [x] ++ quicksort larger
```

## 递归地思考

1. 定义基准
2. 调用自身，拆分成相似的子问题
3. 利用子问题的结果组合成最终的结果