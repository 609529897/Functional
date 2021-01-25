# 输入与输出

## 纯粹与非纯粹的分离

```haskell
main = putStrLn "hello word"
```

main 应该是 IO 操作的头，可以基于 main 组合更多的 IO 操作

> () unit

## 组合 I/O 操作

```haskell
main = do
  pubStrLn "hello"
  name <- getLine                            -- 输出的值只能这样取值
  putStrLn ("hey" ++ name ++ ", you rock!" ) -- 最后一个行不可以 <- 取值
```

### 在 I/O 操作中使用 let

```haskell
import Data.Char

main = do
    pubStrLn "what't your first name?"
    firstname <- getLine
    pubStrLn "what't your last name?"
    lastname <- getLine
    let bigFirstName = map toUpper firstName
        bigLastName = map toUpper lastName
    putStrLn $ "hey" ++ bigFirstName ++ "" ++ bigLastName ++ ", how are you?"
```

**return** 

- 能够基于一个纯的值来构造 I/O 操作。取一个值作为参数包装到容器里

- 跟 <- 的操作正好相反

- 不会打断程序的运行

## I/O 函数


**putStr**

取一个字符串并在终端输出文字的 IO 操作，不会换行

**putChar**

取一个字符并在终端输出的 IO 操作

**print**

取一个 show 的实例类型，并输出 `putStrLn . show`

**when**

取一个布尔值和 IO 操作，如果布尔值为真那就返回传递给他的 IO 操作，如果为假返回 return ()

```haskell
import Control.Monad

main = do
    input <- getLine
    when (input == "SWORDFISH") $ do -- ? 必须是 do 后面跟着 IO 操作 ?
        putStrLn input
```

**sequence**

```haskell
main = do
    rs <- sequence [getLine, getLine, getLine]
    print rs
```

**mapM 和 mapM_**

```haskell
mapM print [1, 2, 3]
1
2
3
[(),(),()]-- 结果, 使用 mapM_ 就不会有这个结果
```

**forever**

取一个 IO 操作为参，返回一个永远重复执行该 I/O 操作的 I/O 操作

```haskell
import Control.Monad
import Data.Char

main = forever $ do
    putStr "Give me some imput: "
    l <- getLine
    putStrLn $ map toUpper l
```

**forM**

为列表的每个元素创建对应的 I/O 操作，而 I/O 操作的具体行为与元素相关，最后执行这些 I/O 操作，并将结果绑定为某名字



















