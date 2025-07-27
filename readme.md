## 介绍
emcas-kp 实现了 knuth-plass 的排版算法，单其功能不局限于算法本身。原本的KP算法实现了对英文的排版，emacs-kp对其进行了进一步的拓展，实现了 CJK 语言与 latin 系语言的混合混合排版。

## 演示
先来看一下排版效果的 demo：

![ekp-demo](./images/ekp-demo-with-cache.gif)

## 局限
目前只支持 CJK 与任意一种拉丁系语言的混合排版，不支持多种拉丁系语言混合排版的场景。原因是无法精确判断单词属于哪种语言，从而对其进行 hyphen 断词。

## 用法

### 配置项
`ekp-latin-lang` 用来设置文本中主要的拉丁系的语言，dictionary 下可以看到所有支持的语言。

## 核心函数

提供了两个函数:

```(ekp-pixel-justify string line-pixel)```

将文本 STRING 按照每行像素宽度为 LINE-PIXEL 排版，返回排版后的文本。

```(ekp-pixel-range-justify string min-pixel max-pixel)```

在 MIN-PIXEL 到 MAX-PIXEL 的返回内寻找最优排版，返回一个 cons-cell，car 是排版后的文本，cdr 是最优排版效果的像素值。