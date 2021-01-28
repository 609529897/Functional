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

