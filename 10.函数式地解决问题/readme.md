# 函数式地解决问题

## 逆波兰式计算器 reverse polish notation

使用栈来存储值，遇到操作符时从栈中 pop 出两个值计算结果后 push 进栈，如此反复最后只留下一个结果值

```haskell
-- 平常
(10 + 2) * 10

-- 逆波兰式
10 2 + 10 *
```
### RPN 函数

```haskell
solveRPN :: String -> Double
solveRPN = head . foldl foldingFunction [] . words
    where  foldingFunction (x:y:ys) "*" = (y * x):ys
           foldingFunction (x:y:ys) "+" = (y + x):ys
           foldingFunction (x:y:ys) "-" = (y - x):ys
           foldingFunction (x:y:ys) "/" = (y / x):ys
           foldingFunction (x:y:ys) "^" = (y ** x):ys
           foldingFunction (x:xs) "ln" = log x:xs
           foldingFunction xs "sum" = [sum xs]
           foldingFunction xs numberString = read numberString:xs
```

### 从希思罗机场到伦敦

```haskell
import Data.List

main = do
  contents <- getContents
  let threes = groupsOf 3 (map read $ lines contents)
      roadSystem = map (\[a, b, c] -> Section a b c) threes
      path = optimalPath roadSystem roadSystem
      pathString = concat $ map (show . fst) path
      pathTime = sum $ map snd path
  putStrLn $ "The best path to take is:" ++ pathString
  putStrLn $ "Time taken:" ++ show pathTime

data Section = Section { getA::Int, getB::Int, getC::Int }
  deriving (Show)

type RoadSystem = [Section]

heathrowToLondon :: RoadSystem
heathrowToLondon = [Section 50 10 30
                   , Section 5 90 20
                   , Section 40 2 25
                   , Section 10 8 0
                   ]

data Label = A | B | C deriving (Show)
type Path = [(Label, Int)]

potimalPath :: RoadSystem -> Path
potimalPath RoadSystem = 
  let (bestAPath, bestBPath) = foldl roadStep ([], []) RoadSystem
  in if sum (map snd bestAPath) <= sum (map snd bestBPath)
        then reverse bestAPath
        else reverse bestBPath

roadStep :: (Path, Path) -> Section -> (Path, Path)
roadStep (pathA, pathB) (Section a b) = 
  let timeA = sum (map snd pathA)
      timeB = sum (map snd pathB)
      forwardtimeToA = timeA + a
      crosstimeToA = timeB + b + c
      forwardtimeToB = timeA + a + c
      newPathToA = if forwardtimeToA <= crosstimeToA
                      then (A, a):pathA
                      else (C, c):(B, b):pathB
      newPathToB = if forwardtimeToB <= crosstimeToB
                      then (B, b):pathB
                      else (C, c):(A, a):pathA
    in (newPathToA, newPathToB)

groupsOf :: Int -> [a] -> [[a]]
groupsOf 0 _ = undefined
groupsOf _ [] = []
groupsOf n xs = take n xs : groupsOf n (drop n xs)

```