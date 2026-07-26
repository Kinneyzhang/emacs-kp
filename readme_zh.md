# Emacs-KP: Knuth-Plass 排版算法 Emacs 实现

[English Documentation](./readme.md) | [开发者指南](./DEVELOPER_ZH.md)

Emacs-kp 在 Emacs 内部完整实现了 Knuth-Plass 最优断行算法,支持中日韩
(CJK)与拉丁文混合排版。

## 特性

- **全局最优断行** — Knuth-Plass 动态规划求段落全局最优断点,而非贪心
  首次适应。
- **CJK 支持** — 每个 CJK 字符都是可断行的盒子;避头尾规则保证标点正确
  附着(`,。` 不出现在行首,`「《` 不出现在行尾);汉字间距、中西文间距
  独立可调。
- **连字符断词** — Frank Liang 算法(TeX 同款),内置 70+ 种语言的
  Hunspell 词典。
- **像素级两端对齐** — 每一行渲染宽度精确等于目标像素宽度(通过
  `display (space :width ...)` 属性实现),支持变宽字体。
- **文本属性保留** — face、颜色等属性完整保留;断词插入的连字符继承所
  在单词的样式。
- **困难输入不丢内容** — 超长不可断 token(URL、窄栏长词)退化为紧急
  断行而不是吞掉文本;任何输入都有输出。
- **可选 C 模块** — 动态模块用 C 执行 DP,线程池并行处理多个段落(见
  性能数据)。

## 环境要求

- Emacs **29.1+**(依赖 `string-pixel-width` 与 `object-intervals`)
- 可选(C 模块):C11 编译器和 pthreads

## 快速开始

```elisp
(add-to-list 'load-path "/path/to/emacs-kp")
(require 'ekp)

;; 按 600 像素宽度两端对齐
(insert (ekp-pixel-justify "这是一段测试文本..." 600))

;; 在范围内寻找最优宽度,返回 (对齐文本 . 最优宽度)
(ekp-pixel-range-justify "测试文本" 400 800)
```

多行字符串按行分段处理,空行保留。

### C 模块(长文本推荐)

```bash
cd ekp_c && make        # 需要 C11 编译器,产出 ekp.dylib/.so/.dll
```

```elisp
(ekp-c-module-load)     ; 显示 "ekp-c module loaded (version 1.1, N threads)"
```

加载后(`ekp-use-c-module` 默认为 `t`)所有排版调用自动走 C 引擎。
Elisp 与 C 两个引擎的输出**完全一致**;Elisp 是永远可用的后备。若磁盘
上的模块版本旧于 Elisp 代码的要求,加载会拒绝并提示重新编译。

## 交互使用(buffer 与 region)

`ekp-region.el` 把字符串 API 变成 buffer 级命令:

```elisp
(require 'ekp-region)
```

- `M-x ekp-justify-region` — 把选区排版到窗口文本宽度(数字前缀参数
  可指定像素宽)。
- `M-x ekp-unjustify-region` — **精确**还原原文,包括被折叠的连续空
  格。排版是无损的:每个合成空隙、软换行、软连字符都携带它所替换的
  原文,还原是纯结构变换,即使排版后又编辑过也能正确还原。
- `M-x ekp-auto-justify-mode` — 让整个 buffer 保持按窗口宽度排版。
  窗口宽度变化时自动重排(防抖延迟 `ekp-auto-justify-resize-delay`);
  编辑后只重排被改动的段落(空闲延迟 `ekp-auto-justify-edit-delay`),
  未变段落直接命中段落缓存。关闭 mode 时 buffer 精确恢复原状。

`ekp-region-margin-pixel`(默认 2)是从窗口宽度中扣除的取整安全边距。

### 保护代码块与 verbatim 文本

- 段落级:携带 `ekp-verbatim` 文本属性(`M-x ekp-verbatim-region`)、
  face 在 `ekp-region-skip-faces` 列表中(如 `org-block`、
  `markdown-code-face`)、或被 buffer-local 的
  `ekp-region-skip-predicate` 判定的段落**原样跳过**,一个字节都不动。
- 行内级:带 `ekp-no-break` 属性的区间(`M-x ekp-no-break-region`)
  成为刚性原子——不断行、不断词、空格保持字面宽度——适合行内代码、
  产品名、数字加单位。

## 排版特性

- **对齐模式** — `ekp-alignment`:`justify`(默认)/`ragged-right`/
  `ragged-left`/`center`。非两端对齐模式下词间距保持自然,K-P 仍在
  每行 `ekp-ragged-stretch-pixel`(≈2 em)的余量内全局最小化参差。
- **标点悬挂** — 置 `ekp-protrusion` 为 `t`,行尾标点(。、」以及
  西文句读、断词连字符)按 `ekp-protrusion-ratios` 悬出齐边。全角
  闭合标点默认 0.5,视觉上等价于 CLREQ 的行尾标点半角化。
  `ekp-auto-justify-mode` 自动预留悬挂宽度。
- **段落形状** — `ekp-first-line-indent`(`t` = 2 em)实现中文段首
  缩进惯例;或用 TeX 式 `ekp-parshape` 逐行指定 `(缩进 . 宽度)`。
  两者走 Elisp 2D 路径(C 模块自动旁路,同 `ekp-looseness`)。
- **不可断字符** — NBSP、窄 NBSP、数字空格、WORD JOINER 天然把两侧
  锁在同一行。
- 禁则覆盖全角**与半角**标点:行首不会出现 `。、」!?` 或独立的
  `.,;:!?`,行尾不会出现 `「(` 等。

已知限制:行中的 CLREQ 标点**压缩**(如「字。下」行内挤压)无法渲
染——Emacs 不能缩减字形 advance——因此行边压缩以悬挂方式呈现;左缘
悬挂同理不可渲染(文本无法起笔于行原点之前)。

## 配置

### 断词语言

```elisp
(setq ekp-latin-lang "de_DE")   ; 默认 "en_US"
```

`dictionaries/hyph_<lang>.dic` 中的任意语言均可;`"de"` 这类短代码会解
析到第一个匹配的词典。

### 间距参数

三类 glue 控制间距(单位均为像素):

| 参数组 | 位置 |
|:-------|:-----|
| `lws-*` | 拉丁词之间 |
| `mws-*` | 拉丁词与 CJK 字符之间 |
| `cws-*` | CJK 字符之间 |

每类包含理想宽度、最大拉伸、最大收缩:

```elisp
(ekp-param-set lws-ideal lws-stretch lws-shrink
               mws-ideal mws-stretch mws-shrink
               cws-ideal cws-stretch cws-shrink)
;; 例如 (ekp-param-set 7 3 2  5 2 1  0 2 0)
```

- 从不调用 `ekp-param-set` 时,参数按每个字符串的字体自动计算。
- 显式设置的参数**持久生效**,直到调用 `ekp-param-reset` 恢复自动模式。

### 算法参数

| 变量 | 默认值 | 含义 |
|:-----|:-------|:-----|
| `ekp-line-penalty`               | 10  | 每行基础代价;越大越倾向少行 |
| `ekp-hyphen-penalty`             | 50  | 连字符断词代价(以 penalty² 计入) |
| `ekp-adjacent-fitness-penalty`   | 100 | 相邻行松紧等级相差 >1 的代价 |
| `ekp-consecutive-hyphen-penalty` | 100 | 连续断词行的代价系数(× 次数²) |
| `ekp-last-line-min-ratio`        | 0.5 | 末行最小填充比例 |
| `ekp-last-line-short-penalty`    | 50  | 末行过短的代价系数 |
| `ekp-looseness`                  | 0   | 目标行数偏移:+1 比最优多一行,−1 少一行 |

所有参数对两个引擎都生效:每次调用 C 之前 Elisp 会同步这些参数。
`ekp-looseness` 由专门的 Elisp 路径处理(非零时自动绕过 C 模块)。

### 缓存

分词、测宽和 DP 结果按段落缓存。

- `ekp-para-cache-limit`(默认 256):缓存段落数上限,超过后整体清空。
- `M-x ekp-clear-caches` 清空所有缓存(更换字体或影响字宽的主题后使用)。

## 性能

基于内置示例文本(`tests/ekp-bench.el`)、batch Emacs 30.2、Apple
Silicon 测得;方法见 DEVELOPER_ZH.md:

| 场景(text-zh.txt ≈ 3.6KB) | Elisp(字节编译) | C 模块 |
|:----------------------------|------------------:|-------:|
| 两端对齐,宽 200px          |             96 ms |  57 ms |
| 最优宽度搜索 340–380        |            294 ms |  75 ms |
| 仅 DP,宽 400px             |             15 ms | 1.3 ms |

**请字节编译本包**——编译后 Elisp 引擎快约 10 倍。两引擎输出完全一
致;C 模块在最优宽度搜索和长多段文本上收益最大。

## 已知限制

- 宽度按字符串自身的文本属性测量。若目标 buffer 重映射了 face(不同
  `:height`、主题),宽度可能有偏差;请用与显示时相同的属性做排版。
- 计算默认间距时假定每段落的拉丁/CJK 各使用一种字体;混合字体段落可以
  工作,但默认间距取自找到的第一个字体。
- `ekp-pixel-range-justify` 用三分搜索加局部扫描最小化平均 demerits;
  代价关于宽度并非严格单峰,结果是很好的局部最优,不保证全局最优。
- batch/tty 模式下像素宽度退化为字符列数(整条管线仍可工作,便于测试)。

## 测试

```bash
tests/run-tests.sh /path/to/emacs     # 36 个 ERT 测试,全部支持 batch
```

## 致谢

- **核心算法**: ["Breaking Paragraphs into Lines"](https://gwern.net/doc/design/typography/tex/1981-knuth.pdf) by Donald E. Knuth and Michael F. Plass (1981)
- **断词算法**: Frank Liang 算法,改编自 [Pyphen](https://github.com/Kozea/Pyphen)
- **词典**: [Hunspell 断词模式](https://github.com/Kozea/Pyphen)
