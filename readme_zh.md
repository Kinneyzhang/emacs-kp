## 介绍
Emacs-kp 实现了 knuth-plass 排版算法，但其功能不局限于英文排版，我对算法的进一步优化，实现了 CJK 与 Latin 系语言的混合排版。

## 演示
先来看一下排版效果的 demo：

![ekp-demo](./images/ekp-demo-with-cache.gif)

## 局限
目前只支持 CJK 与任意一种拉丁系语言的混合排版，不支持多种拉丁系语言混合排版的场景。原因是无法精确判断单词属于哪种语言，从而对其进行 hyphen 断词。

## 用法

### 配置项
`ekp-latin-lang` 用来设置文本中主要的拉丁系语言，默认设置为 "en_US"，dictionaries 下可以看到所有支持的语言，语言的名称也需要按照词典中 hyph_ 后面的名称来设置。测试用例中有德文和法文排版的例子，其他语言没有测试过，理论上应该没有问题，但不排除需要更为细节的调教。

### 核心函数

提供了两个函数:

```(ekp-pixel-justify string line-pixel)```

将文本 STRING 按照每行像素宽度为 LINE-PIXEL 排版，返回排版后的文本。

```(ekp-pixel-range-justify string min-pixel max-pixel)```

在 MIN-PIXEL 到 MAX-PIXEL 的返回内寻找最优排版，返回一个 cons-cell，car 是排版后的文本，cdr 是最优排版效果的像素值。