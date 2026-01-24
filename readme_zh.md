# Emacs-KP: Knuth-Plass 排版算法 Emacs 实现

Emacs-kp 实现了 Knuth-Plass 最优断行算法，并扩展支持 CJK（中日韩）与拉丁文混合排版。

## 演示

![ekp-demo](./images/ekp-demo-with-cache.gif)

## 算法原理

### Knuth-Plass 算法

本算法基于 Donald Knuth 和 Michael Plass 于 1981 年发表的经典论文 ["Breaking Paragraphs into Lines"](https://gwern.net/doc/design/typography/tex/1981-knuth.pdf)。与大多数文本编辑器使用的贪心断行不同，K-P 算法**同时考虑所有可能的断点**，寻找全局最优解。

#### 核心概念

**1. Box（盒子）、Glue（胶水）、Penalty（惩罚）**

文本被建模为三种元素的序列：
- **Box**：不可分割的内容（字符、单词），具有固定宽度
- **Glue**：弹性空白，具有理想宽度、可拉伸量、可压缩量
- **Penalty**：在特定位置断行的代价（如连字符断词）

```
┌─────┐      ┌─────┐      ┌─────┐
│ Box │─Glue─│ Box │─Glue─│ Box │
└─────┘      └─────┘      └─────┘
 单词     (弹性空白)    单词
```

**2. Badness（劣度）：衡量行的质量**

每行的质量由 glue 需要拉伸/压缩的程度来衡量：

```
            ⎧ 0                           若 adjustment = 0
badness =   ⎨ ∞                           若无法容纳
            ⎩ 100 × |adjustment/flexibility|³
```

- `adjustment` = 目标宽度 - 自然宽度
- `flexibility` = 可拉伸总量（拉伸时）或可压缩总量（压缩时）

**3. Demerits（缺陷值）：评估断行序列**

Demerits 综合 badness 和 penalty 来评估整个段落的排版质量：

```
demerits = (line_penalty + badness)² + penalty² + fitness_penalty
```

其中：
- `line_penalty`：每行的基础代价（默认：10）
- `penalty`：断点特定代价（连字符：50）
- `fitness_penalty`：相邻行松紧度差异过大时的额外代价

**4. Fitness Classes（适应度等级）**

行按松紧度分类，确保视觉一致性：
- 等级 0：紧凑（显著压缩）
- 等级 1：正常（接近理想）
- 等级 2：宽松（拉伸）
- 等级 3：非常宽松（显著拉伸）

相邻行等级差超过 1 会产生额外惩罚。

**5. 动态规划**

算法使用 DP 在所有有效断点中寻找最小 demerits 路径：

```
dp[k] = min over all valid i < k {
    dp[i] + demerits(从 i 到 k 的行)
}
```

时间复杂度：O(n²)，n = 潜在断点数量。

### CJK 扩展

Emacs-kp 为 CJK 文本扩展了原算法：

1. **字符级断行**：CJK 文本可在任意字符间断行
2. **混合间距**：Latin-Latin、Latin-CJK、CJK-CJK 三种 glue 类型
3. **标点处理**：CJK 标点附着于相邻字符

### 连字符断词

拉丁语单词断词使用 Frank Liang 的算法（TeX 的断词算法）：
- 基于模式匹配的优先级方法
- 特定语言的词典（en_US、de_DE、fr 等）
- 可配置断点前后的最小字符数

## 局限

目前仅支持 CJK 与**一种**拉丁语言的混合排版。不支持多种拉丁语言混排，因为系统无法可靠判断单词属于哪种语言以进行断词。

## 用法

### 配置项

**`ekp-latin-lang`**：用于断词的主要拉丁语言（默认：`"en_US"`）。
支持的语言见 `dictionaries/` 目录。

**`ekp-param-set`**：配置间距参数（单位：像素）：

| 参数                    | 说明                           |
|:------------------------|:-------------------------------|
| `ekp-lws-ideal-pixel`   | 拉丁单词间的理想间距           |
| `ekp-lws-stretch-pixel` | 拉丁单词间的最大拉伸量         |
| `ekp-lws-shrink-pixel`  | 拉丁单词间的最大压缩量         |
| `ekp-mws-ideal-pixel`   | 拉丁与 CJK 之间的理想间距      |
| `ekp-mws-stretch-pixel` | 拉丁与 CJK 之间的最大拉伸量    |
| `ekp-mws-shrink-pixel`  | 拉丁与 CJK 之间的最大压缩量    |
| `ekp-cws-ideal-pixel`   | CJK 字符间的理想间距           |
| `ekp-cws-stretch-pixel` | CJK 字符间的最大拉伸量         |
| `ekp-cws-shrink-pixel`  | CJK 字符间的最大压缩量         |

示例：`(ekp-param-set 7 3 2 5 2 1 0 2 0)`

**请勿直接设置这些变量——必须使用 `ekp-param-set` 函数。**

默认值遵循 K-P 推荐：
- 理想宽度 = 空格字符宽度
- 可拉伸 = 理想 × 0.5
- 可压缩 = 理想 × 0.33

### K-P 算法参数

| 参数                          | 默认值 | 说明                         |
|:------------------------------|:-------|:-----------------------------|
| `ekp-line-penalty`            | 10     | 每行断行的基础惩罚           |
| `ekp-hyphen-penalty`          | 50     | 连字符断词的惩罚             |
| `ekp-adjacent-fitness-penalty`| 100    | 相邻行松紧度不一致的惩罚     |
| `ekp-last-line-min-ratio`     | 0.5    | 末行最小填充比例             |
| `ekp-looseness`               | 0      | 目标行数偏移（±n 行）        |

### 核心函数

```elisp
(ekp-pixel-justify string line-pixel)
```
将 STRING 按 LINE-PIXEL 宽度排版，返回排版后的文本。

```elisp
(ekp-pixel-range-justify string min-pixel max-pixel)
```
在 [MIN-PIXEL, MAX-PIXEL] 范围内使用三分搜索寻找最优宽度。
返回 `(排版文本 . 最优像素值)`。

注：使用 O(log n) 三分搜索，并积极缓存。

```elisp
(ekp-clear-caches)
```
清除所有段落缓存。

## 路线图

- [x] 排版后保留原始文本属性
- [x] 完整的 Knuth-Plass demerits 模型与 fitness classes
- [x] 支持连续连字符惩罚的断词
- [ ] Rust 动态模块实现并行计算
- [ ] 混合标点自动修正

## 致谢

- 核心算法：Donald E. Knuth 和 Michael F. Plass 的论文 ["Breaking Paragraphs into Lines"](https://gwern.net/doc/design/typography/tex/1981-knuth.pdf)（1981）
- 断词算法：改编自 [Pyphen](https://github.com/Kozea/Pyphen)，使用 Liang 算法
- 词典：[Hunspell 断词模式](https://github.com/Kozea/Pyphen)
