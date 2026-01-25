# Emacs-KP 开发者文档

本文档详细介绍了 `emacs-kp` 的内部架构、API 和算法原理。旨在帮助贡献者和高级用户理解其工作机制。

## 1. 架构概览

`emacs-kp` 采用分层架构，将文本处理、布局计算和渲染分离。

```
┌─────────────────────────────────────────────────────────────────┐
│                      用户 API 层 (ekp.el)                        │
│  ekp-pixel-justify  ekp-pixel-range-justify  ekp-clear-caches   │
└─────────────────────────────────────────────────────────────────┘
                               │
                               ▼
┌─────────────────────────────────────────────────────────────────┐
│                     缓存层 (ekp-utils.el)                        │
│  ekp--get-para (段落缓存)         ekp-dp-cache (DP 结果缓存)    │
└─────────────────────────────────────────────────────────────────┘
                               │
               ┌───────────────┴───────────────┐
               ▼                               ▼
┌─────────────────────────┐     ┌─────────────────────────┐
│    纯 Elisp 路径         │     │     C 模块路径          │
│   ekp--dp-cache-elisp   │     │   ekp--dp-cache-via-c   │
│   (Elisp 实现 O(n²) DP) │     │   (调用 C 进行 DP)      │
└─────────────────────────┘     └─────────────────────────┘
                                               │
                                               ▼
                                 ┌─────────────────────────┐
                                 │      C 动态模块          │
                                 │   ekp_break_with_prefixes│
                                 │   (8 线程并行计算)       │
                                 └─────────────────────────┘
                               │
                               ▼
┌─────────────────────────────────────────────────────────────────┐
│                        渲染层                                    │
│  ekp--render-justified (应用断点，插入 display 属性胶水)        │
└─────────────────────────────────────────────────────────────────┘
```

## 2. Elisp 核心 (ekp.el)

### 数据结构

#### `ekp-para` 结构体

核心数据结构是 `ekp-para`，代表预处理后的段落。它被缓存以避免重复分词和测量。

```elisp
(cl-defstruct ekp-para
  string           ; 带有属性的原始文本
  latin-font       ; 检测到的拉丁字体
  cjk-font         ; 检测到的 CJK 字体
  boxes            ; Box 字符串向量
  boxes-widths     ; Box 像素宽度向量
  boxes-types      ; 类型向量 (start-type . end-type)
  glues-types      ; 胶水类型符号向量 (lws, mws, cws, nws)
  hyphen-pixel     ; 连字符宽度
  hyphen-positions ; 可断词 Box 索引向量
  ideal-prefixs    ; 前缀和：理想宽度 (用于 O(1) 宽度计算)
  min-prefixs      ; 前缀和：最小宽度
  max-prefixs      ; 前缀和：最大宽度
  dp-cache)        ; 哈希表：行宽像素 → DP 结果
```

#### 胶水类型 (Glue Types)
- `lws` (Latin Word Space): 拉丁词间距
- `mws` (Mixed Word Space): 中西文间距
- `cws` (CJK Word Space): CJK 字符间距
- `nws` (No Word Space): 固定间距

### 核心函数

#### `(ekp-pixel-justify STRING LINE-PIXEL)`
将 `STRING` 按 `LINE-PIXEL` 宽度对齐。
1. 检查缓存中是否有对应的 `ekp-para`。
2. 若未命中，创建 `ekp-para`（分词、测量、断词处理）。
3. 调用 DP 引擎（Elisp 或 C）计算断点。
4. 使用 display 属性（特别是 `space` 属性）渲染结果。

#### `(ekp-pixel-range-justify STRING MIN-PIXEL MAX-PIXEL)`
在范围内寻找“最佳”宽度。使用三分搜索 (O(log n)) 最小化 demerits。用于自动寻找最适合该段落的宽度。

#### `(ekp-param-set ...)`
设置 9 个间距参数（LWS/MWS/CWS 的 Ideal/Stretch/Shrink）。

## 3. C 动态模块 (ekp_c)

对于长文本，C 模块通过并行化 O(n²) 动态规划阶段提供约 20 倍的加速。

### 源码结构
- `ekp_c/ekp.c`: Emacs 模块入口点。
- `ekp_c/ekp_kp.c`: Knuth-Plass 算法实现。
- `ekp_c/ekp_thread_pool.c`: 工作线程池。
- `ekp_c/ekp_hyphen.c`: Liang 断词算法。

### C API (暴露给 Elisp)

#### `(ekp-c-init)`
初始化模块和线程池。

#### `(ekp-c-break-with-prefixes ...)`
底层 DP 函数。它接收来自 Elisp 的扁平数组（指针）：
- 前缀和数组 (ideal, min, max)
- 每个 Box 的胶水参数
- 连字符位置
- 目标行宽

返回断点索引列表和总代价。

### 内存模型
- **零拷贝 (Zero Copy)**: Elisp 直接将向量数据的指针传递给 C。
- **扁平数组**: 数据结构为并行数组，提高缓存效率。
- **线程安全**: 模块使用固定线程池。DP 算法采用波前模式 (Wavefront) 并行化内部循环。

## 4. 算法细节

### Knuth-Plass 算法
基于 1981 年论文 "Breaking Paragraphs into Lines"。

**代价函数 (Demerits):**
`D = (LinePenalty + Badness)² + Penalty²`

**劣度 (Badness):**
`100 * |Adjustment / Flexibility|³`

### CJK 扩展
- **Boxes**: 每个 CJK 字符视为一个独立的 Box。
- **Glues**: 针对 CJK-CJK 和 CJK-Latin 的特定胶水类型允许精细调整间距（例如在汉字和英文之间增加微小的空隙）。

### 断词 (Hyphenation)
使用 Frank Liang 算法（TeX 标准）。
- 模式从 `dictionaries/*.dic` 加载。
- `ekp-hyphen.el` 在纯 Elisp 中处理。
- C 模块有自己的实现 (`ekp_hyphen.c`)，目前主要由 Elisp 负责分词。
