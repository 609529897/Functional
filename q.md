# Q

## where

1. where 的作用域？
2. 可以在列表的生成器中定义使用 let 吗？

## 高阶函数

1. point free 风格意思是只有运算没有，数据。只是一个通道？

2. 

```haskell
oddSquareSum = sum . takeWhile (<10000) . filter odd $ map (^2) [1..]

oddSquareSum = sum . takeWhile (<10000) . filter odd $ map (^2) $ [1..] -- 需要 $ 吗？
```

