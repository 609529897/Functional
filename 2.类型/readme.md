# 类型

## intro

- 类型推导
- `:t` 指令可以直接获得值的类型如： `:t 'a'` 返回 `'a'::Char`
- `::` 意思是：类型为
  - 如果你不知道自己函数类型是啥样的，可以先写函数让后使用 `:t` 进行检查，在书写

```haskell
removeNonUppercase::[Char] -> [Char]
removeNonUppercase st = [c | c <- st, c `elem` ['A'..'Z']]
```

```haskell
-- 多参数
addThree :: Int -> Int -> Int -> Int
addThree x y z = x + y + z
```

## 常见类型

| 类型    | 描述                                                         |
| ------- | ------------------------------------------------------------ |
| Int     | 整数（7 是，7.5 不是），它是有界的跟使用的机器有关           |
| Integer | 表示的也是整数，可以存放很大的值，但是效率不如 Int           |
| Float   | 单精度浮点数                                                 |
| Double  | 双精度浮点数，位是一般的浮点数的两倍，使其精度更高，但是内存占用也更大 |
| Bool    | 布尔值，True / False                                         |
| Char    | Unicode 字符                                                 |

## 类型变量

```haskell
-- a 就是类型变量
[a] -> a
```

- 使用了类型变量的函数被称为 **多态函数**

- 通常使用单个小字符表示类型变量 `a, b, c ..`

## typeclass 

- `==`, `+` , `/`  ...  infix function
- `=>` type constraint* 类型约束

```haskell
(Num a) => a -> a
```

```haskell
(==) 1 2
```

```haskell
:t (==)
(==) :: (Eq a) => a -> a -> Bool
```

some typeclass

| typeclass | description                                                  | exmple                                  |
| --------- | ------------------------------------------------------------ | --------------------------------------- |
| Eq        | impl: `==, /=`                                               |                                         |
| Ord       | impl: `<, >, <=, >=`                                         |                                         |
| Show      | 其他类型变成 `String` 类型，处理函数                         | show True -- "True"                     |
| Read      | String type => Read type；read "4" 报错，因为编译器不知道返回的啥类型，可以使用 **类型注解** 解决这个问题 | read "True" ｜false  --True             |
| Enum      | 都是连续，可枚举的                                           | succ 'B'  -- 'C'       pred 'B'  -- 'A' |
| Bounded   | 有上线和下限, 元祖中的所有值都是这个类型那表示元祖也是这个类型 | minBound, maxBound                      |
| Num       | 数值类型类，只有已经是 Show 类和 Eq 类的才可能是 Num 类      |                                         |
| Floating  | 实例类仅包含 Float 和 Double                                 |                                         |
| Integeral | 表示整数，实例类包含 Int 和 Integeral                        |                                         |

> String 是 [Char] 的别名

**类型注解**

类型注解，就是程序员明确告诉编译器想要返回的类型

```haskell
read "12" :: int
-- 12
read (read "5" :: Int) * 3
-- 15

[read "True", False, True] -- 编译器可以按照列表的其他项推导当前项
```

