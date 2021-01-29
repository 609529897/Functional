# 更多的输入输出操作

## 文件和流

```haskell
import Control.Monad
import Data.Char

main = forever $ do
  l <- getLine
  putStrLn $ map toUpper l

```

```haskell
import Data.Char

main = do
  putStrLn <- getContents
  putStr $ map toUpper contents
```


## 读写文件

```haskell
import System.IO

main = do
  handle <- openFile "girlfriend.text" ReadMode
  contents <- hGetContents handle
  putStr contents
  hClose handle
```

withFile

```haskell
withFile :: FilePath -> IOMode -> (Handle -> IO a) -> IO a

import System.IO

main = do
  withFile "girlfriend.txt" ReadMode (\handle -> do
    contents <- hGetContents handle
    putStr contents)
```

```haskell
withFile :: FilePath -> IOMode -> (Handle -> IO a) -> IO a
withFile name mode f = bracket (openFile name mode)
  (\handle -> hClose handle)
  (\handle -> f handle)
```

```haskell
-- readFile, writeFile, appendFile
-- writeFile 和 appendFile 区别
-- writeFile 清空文件后写内容
-- appendFile 在内容后面添加内容
import System.IO
import Data.Char
main = do
  contents <- readFile "girlfriend.txt"
  writeFile "girlfriend.txt" (map toUpper contents)
```

## TODO 列表

```haskell
import System.IO

-- 添加
main = do
  todoItem <- getLine
  appendFile "todo.txt" (todoItem ++ "\n")
```

## 随机数

```haskell
random :: (RandomGen g, RandomGen a) => g -> (a, g)
random (mkStdGen 949488) :: (Integer, StdGen)
-- 949488 不变的话，随机数不变
```
## 字节串

- 类型: `ByteString`

- 严格的和惰性字节串

- 一个严格的字节串代表数组的一系列字节

- 惰性的字节串是存储在一些块里，每一块差不多 64kb，读取文件时可以按块读取

- 当需要进行高性能的 文件读写，命令行 的 IO 操作时可以使用字节串



