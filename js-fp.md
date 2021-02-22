# FP with js

>《函数式编程指北》

## intro

运用数学常识去编程和简化函数式风格程序

```javascript
// 结合律（assosiative）
add(add(x, y), z) == add(x, add(y, z));

// 交换律（commutative）
add(x, y) == add(y, x);

// 同一律（identity）
add(x, 0) == x;

// 分配律（distributive）
multiply(x, add(y,z)) == add(multiply(x, y), multiply(x, z));
```

---

## 一等公民函数

在 JS 函数和对象具有同一待遇，可赋值给变量，当作参数传递，当作返回值，放进数组里

```javascript
// 函数应更加抽象和通用

// 只针对当前的博客
const validArticles = articles =>
  articles.filter(article => article !== null && article !== undefined),

// 对未来的项目更友好
const compact = xs => xs.filter(x => x !== null && x !== undefined);
```

---

## 纯函数

*纯函数* 是这样一种函数，即相同的输入，永远会得到相同的输出，而且没有任何可观察的副作用。

也就是数学意义上的函数

> cognitive load 认知负荷

```JavaScript
// JS 创建不可变状态
var immutableState = Object.freeze({
  minimum: 21
});
```

### 副作用

*副作用* 是在计算结果的过程中，系统状态的一种变化，或者与外部世界进行的*可观察的交互*。

也就是所有外部可能影响到输出的东西，都可以称为副作用

副作用可能包含，但不限于：

- 更改文件系统
- 往数据库插入记录
- 发送一个 http 请求
- 可变数据
- 打印/log
- 获取用户输入
- DOM 查询
- 访问系统状态

### 八年级数学

函数是不同数值之间的特殊关系：每一个输入值返回且只返回一个输出值。

函数可以描述为一个集合，这个集合里的内容是 (输入, 输出) 对

### 追求“纯”的理由

#### 可缓存性（Cacheable）

因为输入对应着输出所以，所以我们可以缓存值，当输入值不变时总是返回对应值

```javascript
const memoize = function(f) {
  const cache = {};
  
  return function() {
    const arg_str = JSON.stringify(arguments);
    cache[arg_str] = cache[arg_str] || f.apply(f, argument);
    return  cache[arg_str]
  }
}
```

通过延迟执行的方式我们可以把不纯的函数变成纯函数，当我们输入 url 和 params 是总会返回特定的函数

```javascript
var pureHttpCall = memoize(function(url, params) {
  return function() { 
    return $.getJSON(url, params) 
  }
})
```

#### 可移植性 / 自文档化 （Portable / Self - Documenting）

#### 可测试性（Testable）

#### 合理性（Reasonable）

> 引用透明性 （referential transparency）
>
> 如果一段代码可以替换成它执行所得的结果，而且是在不改变整个程序行为的前提下替换的，那么我们就说这段代码是引用透明的。

#### 并行代码

因为纯函数根本不需要访问共享的内存，而且根据其定义，纯函数也不会因副作用而进入竞争态（race condition）。

#### DEBUG 友好

因为输入输出是一个对应关系，所以一旦发现输入一个值并触发 BUG 那么我们可以输入这个值从而复现 BUG

---

## 柯里化（curry）

## 组合（compose）

```javascript
const compose = function (f, g) {
  return function (x) {
    return f(g(x));
  };
};
```

```javascript
export default function compose(...funcs) {
    if (funcs.length === 0) {
        return arg => arg
    }

    if (funcs.length === 1) {
        return funcs[0]
    }

    return funcs.reduce((a, b) => (...args) => a(b(...args)))
}
```

结合律

```javascript
compose(f, compose(g, h)) == compose(compose(f, g), h);
```

### pointfree

类似于组合一个管道，我们不需要关注这个管道会流入什么，只需要建立一个管道就好

不要提到数据

```javascript
// 非 pointfree
var snakeCase = function(word) {
    return word.toLowerCase().replace(/\s+/ig, '_')
}

// pointfree
var snakeCase = compose(replace(/\s+/ig, '_'), toLowerCase)
```

### debug

可以使用 trace 函数进行 debug

```javascript
var trace = curry(function(tag, x){
  console.log(tag, x);
  return x;
});
```

### 范畴学

范畴学（category theory）是数学中的一个抽象分支，能够形式化诸如集合论（set theory）、类型论（type theory）、群论（group theory）以及逻辑学（logic）等数学分支中的一些概念。范畴学主要处

理对象（object）、态射（morphism）和变化式（transformation），而这些概念跟编程的联系非常紧密。

有着以下这些组件（component）的搜集（collection）就构成了一个范畴：

- 对象的搜集

- 态射的搜集

- 态射的组合

- identity 这个独特的态射

**对象的搜集**

对象就是数据类型，例如 String、Boolean、Number 和 Object 等等。通常我们把数据类型视作所有可能的值的一个集合（set）。像 Boolean 就可以看作是 [true, false] 的集合，Number 可以是所有

实数的一个集合。把类型当作集合对待是有好处的，因为我们可以利用集合论（set theory）处理类型。

**态射的搜集**

态射是标准的、普通的纯函数。

**态射的组合**

```javascript
var g = function(x){ return x.length; };
var f = function(x){ return x === 4; };
var isFourLetterWord = compose(f, g);
```

**identity 这个独特的态射**

```javascript
var id = function(x){ return x; };
```

## Hindley-Milner 类型签名

> 哇 Hindley-Milner 太好玩了，一定要深入学生函数式编程和编程语言理论

### 初识类型

- 自由定理（free theorems）

- 编译时检测（compile time checks）

### 神秘的传奇故事

```javascript
// capitalize :: String -> String
var capitalize = s => toUpperCase(head(s)) + toLowerCase(tail(s))
```

```javascript
// match :: Regex -> (String -> [String])
var match = curry(function(reg, s) {
    return s.match(reg);
})
```

```javascript
//  id :: a -> a
var id = function(x){ return x; }

//  map :: (a -> b) -> [a] -> [b]
var map = curry(function(f, xs){
  return xs.map(f);
});
```

```javascript
//  head :: [a] -> a
var head = function(xs){ return xs[0]; }

//  filter :: (a -> Bool) -> [a] -> [a]
var filter = curry(function(f, xs){
  return xs.filter(f);
});

//  reduce :: (b -> a -> b) -> b -> [a] -> b
var reduce = curry(function(f, x, xs){
  return xs.reduce(f, x);
});
```

### 缩小可能性范围 narrowing of possibility

- parametricity

- 多态性（polymorphism）

注意看 head，可以看到它接受 [a] 返回 a。我们除了知道参数是个数组，其他的一概不知；所以函数的功能就只限于操作这个数组上。**在它对 a 一无所知的情况下，它可能对 a 做什么操作呢？换句话说，a 告诉我们它不是一个特定的类型，这意味着它可以是任意类型；那么我们的函数对每一个可能的类型的操作都必须保持统一。这就是 parametricity 的含义**。要让我们来猜测 head 的实现的话，唯一合理的推断就是它返回数组的第一个，或者最后一个，或者某个随机的元素；当然，head 这个命名应该能给我们一些线索。

我们对 a 的类型一无所知，所以无法对 a 进行 字符串，数值 等等操作。那我们只能猜测 head 函数做的事情对所有类型都统一，这就是 parametricity （同源性），那唯一合理的推测是从数组中返回一项，

从函数名 head 我们差不多可以推测返回的是第一个元素

```haskell
head :: [a] -> a
```

接受未知类型数组返回同一未知类型数组，那 reverse 实现的功能是不是排序啊，不可能因为你咋能知道 a 未知类型一定能排序啊，那可能是反转

> 那可不可是 返回部分项目啊，是不可以吗，违反了函数式数据不可变原则？

```haskell
reverse :: [a] -> [a]
```

### 自由定理

```haskell
-- head :: [a] -> a
-- f . head == head . map f
compose(f, head) == compose(head, map(f));

-- filter :: (a -> Bool) -> [a] -> [a]
-- map f . filter (p . f) == filter p . map f
compose(map(f), filter(compose(p, f))) == compose(filter(p), map(f));
```

### 类型约束 type constraints

```haskell
sort :: Ord a => [a] -> [a]

assertEqual :: (Eq a, Show a) => a -> a -> Assertion
```