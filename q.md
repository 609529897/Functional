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

## 代数类型类型 和 ADT

```haskell
singleton :: a -> Tree a
singleton x = Node x EmptyTree EmptyTree

treeInsert :: (Ord a) => a -> Tree a -> Tree a
treeInsert x EmptyTree = singleton x
treeInsert x (Node a left right)
    | x == a = Node x left right
    | x < a = Node a (treeInsert x left) right
    | x > a = Node a left (treeInsert x right)


treeElem :: (Ord a) => a -> Tree a -> Bool
treeElem x EmptyTree = False
treeElem x (Node a left right)
    | x == a = True
    | x < a = treeElem x left
    | x > a = treeElem x right
```

## 值构造器和类型构造器?

## Functor