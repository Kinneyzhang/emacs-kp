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

`ekp-param-set` 这是一个函数，用来设置排版的基础参数，它们分别为：

| 参数                  | 含义                                      |
|:----------------------|:------------------------------------------|
| ekp-lws-ideal-pixel   | 拉丁语言单词之间的理想像素宽度            |
| ekp-lws-stretch-pixel | 拉丁语言单词之间的可拉伸像素宽度          |
| ekp-lws-shrink-pixel  | 拉丁语言单词之间的可压缩像素宽度          |
| ekp-mws-ideal-pixel   | 拉丁语言单词和CJK字符之间的理想像素宽度   |
| ekp-mws-stretch-pixel | 拉丁语言单词和CJK字符之间的可拉伸像素宽度 |
| ekp-mws-shrink-pixel  | 拉丁语言单词和CJK字符之间的可压缩像素宽度 |
| ekp-cws-ideal-pixel   | CJK字符之间的理想像素宽度                 |
| ekp-cws-stretch-pixel | CJK字符之间的可拉伸像素宽度               |
| ekp-cws-shrink-pixel  | CJK字符之间的可压缩像素宽度               |

例如 `(ekp-param-set 7 3 2 5 2 1 0 2 0)` 对应设置上面的值。请勿直接设置上面的变量，必须要使用这个函数来设置。

如果不手动设置，默认会按照 KP 算法推荐的规则设置一个合适的值：理想宽度设置为空格的像素宽度；可拉伸宽度为理想宽度的1/2；可压缩宽度为理想宽度的1/3。默认设置 `ekp-mws-ideal-pixel = ekp-lws-ideal-pixel - 2`；拉伸和压缩比例与上面一致。中文字符间的理想宽度为0；可拉伸宽度2；无可压缩宽度。

### 核心函数

提供了两个函数:

```(ekp-pixel-justify string line-pixel)```

将文本 STRING 按照每行像素宽度为 LINE-PIXEL 排版，返回排版后的文本。

```(ekp-pixel-range-justify string min-pixel max-pixel)```

在 MIN-PIXEL 到 MAX-PIXEL 的返回内寻找最优排版，返回一个 cons-cell，car 是排版后的文本，cdr 是最优排版效果的像素值。请注意，该函数会遍历计算最小和最大像素之间的排版代价取最小值的情况，如果范围设置的太大执行时间可能会显著变长。后续考虑使用 rust 动态模块来并行计算，提高性能。

## 下一步

- [x] 重排之后，保留文本原本的样式。
- [ ] 使用 rust 动态模块重写：利用 rust 并行计算提升渲染性能。
- [ ] 实现排版自动修正功能：比如修正中文中使用的英文标点；英文中使用的中文标点等

## 感谢

1. 毫无疑问核心算法源自此篇论文 "Breaking Paragraphs into Lines" by DONALD E. KNUTH AND MICHAEL F. PLASS

2. 拉丁单词的 hypen 断词的实现是由 Pyphen 这个 python 库的代码转写而来的，词库也来源于此: https://github.com/Kozea/Pyphen
