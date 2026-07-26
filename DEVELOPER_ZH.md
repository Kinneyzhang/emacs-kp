# Emacs-KP 开发者文档

本文档描述 `emacs-kp` 的实际内部架构、算法与 API,面向贡献者和高级用户。

## 1. 处理管线

一次排版调用经过五个阶段:

```
 字符串
   │
   ▼
 ① 分词          ekp-split-to-boxes           (ekp-utils.el)
   │             拉丁词 / CJK 单字 / 空格串 → 盒子(box);
   │             CJK 标点按避头尾规则附着
   ▼
 ② 断词          ekp--split-with-hyphen        (ekp.el + ekp-hyphen.el)
   │             拉丁词盒子 → 音节盒子(Liang 模式)
   ▼
 ③ 测量与索引    ekp--make-para                (ekp.el)
   │             像素宽度、glue 类型、前缀和数组
   │             → 缓存为 `ekp-para` 结构
   ▼
 ④ 断行(DP)    ekp--dp-run-1d / C 模块       (ekp.el / ekp_c/)
   │             Knuth-Plass 动态规划 → 断点序列
   ▼
 ⑤ 渲染          ekp-line-glues, ekp--pixel-justify
                 分配 glue 像素、剥离行首尾空格盒、附加连字符
                 → 以 "\n" 连接的行
```

`ekp-pixel-justify` 按 `"\n"` 拆分输入,每个非空段独立走这条管线
(C 模块可用时通过 batch API 并行处理)。

## 2. 数据结构

### `ekp-para`(段落缓存条目)

DP 和渲染需要的一切,每段只算一次:

| 字段 | 内容 |
|:-----|:-----|
| `string`, `latin-font`, `cjk-font` | 原文与检测到的字体 |
| `boxes` | 盒子字符串向量 |
| `boxes-widths` | 每个盒子的像素宽(带去重测量) |
| `boxes-types` | 每盒 `(首类型 . 尾类型)`:`latin`/`cjk`/`cjk-punct`/`space` |
| `glues-types` | 每个盒子*之前*的 glue 类别:`lws`/`mws`/`cws`/`nws` |
| `hyphen-pixel`, `hyphen-positions` | 连字符宽度;可断词盒索引的有序向量 |
| `ideal/min/max-prefixs` | 盒+glue 宽度在理想/最收/最伸状态下的前缀和(n+1 个元素) |
| `glue-ideals/shrinks/stretches` | 按盒索引的前导 glue 值(n 个)——原样传给 C |
| `lws/mws/cws-prefixs` | 各可伸缩 glue 类别的前缀**计数** → 每候选行 O(1) 数间隙 |
| `lead-spaces` | `lead-spaces[i]` = 从盒 i 开始的连续空格盒总宽;下标 0 强制为 0(首行缩进保留) |
| `trail-spaces` | `trail-spaces[k]` = 到盒 k−1 结束的连续空格盒总宽 |
| `glue-params` | 创建时九个间距值的 plist 快照 |
| `dp-cache` | 哈希:行宽 → dp-result plist |

段落缓存(`ekp--para-cache`)以 `equal` 比较结构化 key——字符串内容、
文本属性区间的打印形式、检测字体、断词语言(`ekp-latin-lang`)、九个
显式间距值(或符号 `auto`)。结构化 key 使哈希碰撞无害(旧的 `sxhash`
整数方案理论上可能串段)。超过 `ekp-para-cache-limit` 时整体清空。
单条快路径(`ekp--last-para`,按字符串 `eq` + 语言校验)覆盖同一次
排版内的大量同字符串查询。

### dp-result

`(:rests R :gaps G :breaks B :cost C :line-count N)`。`breaks` 为每行
的排他终点索引;`rests[i]` = 行宽 − 行理想宽(glue 需要吸收的像素);
`gaps[i]` = `(lws数 mws数 cws数)` 用于 glue 分配(单盒行和末行为 nil)。

## 3. 行度量

候选行覆盖盒子 `[i, k)` 时:

```
raw     = prefix[k] − prefix[i] − 前导glue(i)
space-w = min(raw, lead-spaces[i] + trail-spaces[k])
width   = raw − space-w  (若盒 k−1 处断词,再加连字符宽)
```

理想/最小/最大三个值均 O(1) 得出。行边缘的空格盒串被排除,因为渲染层
会剥离它们;DP 与渲染层因此严格一致,每一行的渲染宽度精确等于目标宽
(测试 `ekp-test-justify-line-width-invariant`)。

## 4. Knuth-Plass 动态规划

`ekp--dp-run-1d` 从左到右松弛位置。对每个可达起点 `i` 扫描终点 `k`,
直到行的最小宽度超过目标。断点合法条件:`min ≤ 目标 ≤ max`,或末行
`ideal ≤ 目标`。

**Demerits**(每行,与 `ekp_c/ekp_kp.c` 完全一致):

```
demerits = (line-penalty + badness)²
         + penalty²                       ; 断词处为 hyphen-penalty
         + adjacent-fitness-penalty       ; 当 |fitness − 前行fitness| > 1
         + consecutive-hyphen-penalty × 连续次数²
badness  = min(10000, 100·|adjustment/flexibility|³)
```

松紧等级(tight/decent/loose/very-loose)沿用 TeX 的比例阈值。特殊情
况:单盒行 flexibility 固定为 1、fitness 为 decent;末行代价为
`(line-penalty + 短行badness)²`,填充率低于 `ekp-last-line-min-ratio`
时 `短行badness = last-line-short-penalty × (1 − 填充率)`。

与 1981 论文的差异(有意为之):penalty 一律以 `+p²` 计入(无负
penalty/flagged 断点),主流程无 `q`/looseness(见 §6),相邻松紧惩
罚为平坦常数。

### 两遍紧急策略

某些输入不存在合法排版:比行宽更宽的不可断盒子,或无法伸展到目标宽
的刚性(全 `nws`)区段。先跑严格遍;若段尾不可达,第二遍额外允许
**紧急断行**——demerits 为 `(line-penalty + 10000)² + rest²` 的单盒行,
不低于任何常规行的代价。由位置归纳可证:任何输入必有输出(回归:窄
栏 CJK 曾整段返回空串),常规输入不付任何代价、保持纯 K-P 最优。两个
引擎实现完全相同的策略。

## 5. 渲染

`ekp-line-glues` 把每行的 `rest` 转成各 glue 的像素值:

- rest > 0 → 拉伸,按 拉丁 → 中西 → CJK 优先级分配;CJK 间隙可吸收超
  出名义容量的剩余(紧急摊布)。
- rest < 0 → 收缩,同样的优先级,不低于各类收缩下限;glue 宽度钳制
  ≥ 0。
- 末行右侧不齐(理想 glue + 尾部填充);单盒行的尾部填充钳制 ≥ 0。

`ekp--pixel-justify` 随后剥离行首空格盒(首行除外——缩进)与行尾空格
盒,在断词处附加连字符(继承所断单词的文本属性)。剥离的宽度**不再**
重新分配:DP 已经排除了它们(§3)。

Glue 渲染为 `(space :width (N))` display 属性,GUI 下像素级精确,
batch/tty 下按字符列精确。

渲染输出是**无损**的:先用 `ekp--box-offsets` 在原串中定位每个盒子,
然后每一处合成/隐藏内容都记录它所对应的原文——

| 属性              | 位置            | 值 / 含义                  |
|-------------------|-----------------|----------------------------|
| `ekp-glue`        | 合成的 glue 空格| 它所替换的原文             |
| `ekp-soft-break`  | 插入的 `\n`     | 断点处被吞掉的空白         |
| `ekp-soft-hyphen` | 插入的连字符    | 仅作标记                   |
| `ekp-hidden`      | 段落边缘文本    | 原样保留,`display ""` 隐藏|

零宽 glue 若对应非空原文,直接渲染为隐藏的原文本身,因此任何字符都
不会丢失。`ekp-region.el` 对这四类标记做纯结构逆变换
(`ekp-unjustify-region`)——即使排版后又被编辑过也能精确还原——并在
其上实现 `ekp-justify-region` / `ekp-auto-justify-mode`。

### 5.1 断行许可、对齐、悬挂、段形

- **断行许可**:每个 CJK 字符(含标点)独立成盒;
  `ekp-para-breaks-allowed` 按禁则(全角与半角)、`ekp-no-break`
  区间及 NBSP 族连接符禁止相应间隙,被禁间隙不携带 glue。DP 跳过
  被禁候选但继续延伸行;紧急兜底把"内部无许可断点的连跑段"视为
  原子。C 侧接收稀疏 `forbidden-positions` 向量。
- **对齐**(`ekp-alignment`):非两端对齐把 glue 伸缩数组与类参数
  置零,DP 给 `max_w` 加每行额外伸展 R(`ekp-c-set-penalties` 第 7
  参),badness = 100·(欠宽/R)³;渲染层按模式分派剩余(尾部/对半/
  头部)。
- **悬挂**(`ekp-protrusion`):逐间隙 `tail-protrudes[k]`(穿透尾
  随空格盒取最后内容盒)加 `hyphen-protrude` 标量,在 DP、
  `ekp-line-glues`、C 结果重建三处同步放宽每个候选的有效目标宽
  (`lw = width + release`)——三处必须保持一致。
- **每行宽度**(`ekp-parshape` / `ekp-first-line-indent`):由
  `ekp--line-spec`(行号 → 缩进 . 宽度)解析;需要(位置×行数)DP,
  与 looseness 一样旁路 C。缩进渲染为行首 `ekp-glue` 垫片。

C 模块 1.4:`ekp-c-break-with-arrays` 14 参(…、
forbidden-positions、tail-protrudes、hyphen-protrude);batch 向量
14 元;`ekp-c-set-penalties` 4–7 参。

特性完成后的性能(字节编译 + C,Apple Silicon,batch):justify zh
w=200 ≈ 54 ms、range zh ≈ 117 ms——justify 与特性前持平,range 因盒
数增加约 +55%。热路径缓存:`ekp--str-type` 按字符记忆化、glue 字符
串驻留、(段落, 宽度) 渲染结果缓存进 dp-cache(上限 64 个宽度)。连
续变宽实测(60 段 2.6 万字文章,含 region 层全链路):每次变宽约
73 ms,重访宽度更快;编辑后单段增量重排约 17 ms。

## 6. Looseness

`ekp-looseness` ≠ 0 时切换到 `ekp--dp-run-loose`:完整的
(位置 × 行数)DP,为每个行数保留最优路径,最终选取与
(最优行数 + looseness)最接近的行数,平局取 demerits 更小者。该路径
比 1D 重,仅有 Elisp 实现;looseness 激活期间 `ekp--c-available-p`
返回 nil,两引擎永不分歧。

## 7. C 模块集成

C 模块(`ekp_c/`,版本 1.1)只执行阶段 ④。所有字体相关数据以 Elisp
为唯一事实来源。

- `ekp-c-break-with-arrays`(11 参数):para 的前缀数组、glue 数组、
  断词数据、行宽和两个空格串数组。返回 `(breaks . cost)`。
- `ekp-c-break-batch`:11 元素向量的向量,由 pthread 线程池并行处理
  ——每段一个任务(这是正确的并行粒度;DP 本身天然串行)。
- `ekp-c-set-penalties`(4–6 参数):`ekp--c-sync-params` 在**每次**
  进入 C 之前调用,保证 `ekp-line-penalty` 等变量始终生效(回归:此
  前从未同步)。
- `ekp-c-module-load` 拒绝低于 `ekp-c-module-required-version` 的模块
  并回落到 Elisp,避免升级后的参数数量不匹配。

C 端任何失败(返回 NULL)都会静默回落到 Elisp 引擎。两引擎输出逐字
节一致,由 `ekp-test-c-parity-simple` / `ekp-test-c-parity-files` 验证。

`ekp-c-break-lines`(经 `ekp_paragraph.c`、`ekp_hyphen.c` 的 C 端自行
分词路径)是实验性的独立路径,ekp.el 不使用;见 `ekp_c/README.md`。

## 8. 断词(ekp-hyphen.el)

Liang 模式算法,兼容 Pyphen:

- `dictionaries/hyph_*.dic` 首次使用时编译为模式哈希并按路径缓存。
  文件可为 UTF-8 或 ISO-8859(Emacs 自动检测;由
  `ekp-test-hyphen-de-iso8859-dict` 验证)。
- `ekp-hyphen-create LANG` 先精确匹配,再逐级缩短(`"de_CH" → "de"`)。
- 断点两侧默认至少保留 2 个字符。

词盒按 `^[左标点]* (拉丁词) [右标点]*$` 匹配,因此被标点包裹的词
(`(word)`、`word!`、`»word«`)仍可断词;标点粘在首/末音节盒上。

## 9. 测试与基准

```bash
tests/run-tests.sh [emacs]        # 36 个 ERT 测试,batch 可跑
emacs -Q --batch -L . --eval '(setq ekp-use-c-module nil)' -l tests/ekp-bench.el
emacs -Q --batch -L . --eval '(progn (require (quote ekp)) (ekp-c-module-load))' \
      -l tests/ekp-bench.el
```

核心被测不变式:渲染行宽 == 目标宽(像素级对齐)、任意宽度下不丢内
容、O(1) 前缀机制与暴力算法交叉验证、内置文本上的 Elisp/C 一致性、
参数持久化/同步回归。

基准结果(batch Emacs 30.2、Apple Silicon、`tests/text-zh.txt` ≈
3.6KB 中文及各示例;3 次冷缓存取最小值)——"改造前"为重写前的实现
(解释执行):

| 场景                   | 改造前 (Elisp) | 改造后 (Elisp 解释) | 改造后 (Elisp 编译) | 改造后 (C) |
|:-----------------------|---------------:|--------------------:|--------------------:|-----------:|
| justify 中文 w=200     |        7547 ms |             1780 ms |               96 ms |      57 ms |
| justify 中文 w=400     |        2928 ms |              815 ms |               71 ms |      57 ms |
| justify 混排 w=300     |        5540 ms |             1275 ms |               53 ms |      23 ms |
| range 中文 340–380     |       29696 ms |             8937 ms |              294 ms |      75 ms |
| range 混排 280–320     |       68534 ms |            14552 ms |              480 ms |      34 ms |
| 仅 DP,中文 w=400      |        2382 ms |              591 ms |               15 ms |     1.3 ms |

("改造后 (C)" 列在字节编译的 Elisp 环境下测得。作为参照,重写前的
C 模块在 justify-中文-200 / range-中文 / 仅-DP 上分别为 197 ms /
430 ms / 25 ms——重写通过 para 级预建 glue 数组、para 缓存的 `eq'
快路径和 O(1) 重建 rest/gap,把 C 路径也提速了 3–19 倍。)

主要收益来源:前缀数组带来的 O(1) 行度量(旧内层每候选分配 O(n) 子
序列,总计 O(n³))、两遍紧急策略(保持 DP 稀疏)、盒宽测量去重。

## 10. 文件地图

```
ekp.el            核心:para 结构、缓存、DP(1D + looseness)、
                  glue 分配、渲染、公共 API
ekp-utils.el      分词器(盒子、避头尾)、带 batch/tty 回退的字体
                  检测、C 模块加载
ekp-hyphen.el     Liang 断词 + 词典注册
ekp_c/            C 动态模块(见 ekp_c/README.md)
dictionaries/     Hunspell 断词模式(来自 Pyphen)
tests/            ekp-tests.el(ERT)、ekp-bench.el、ekp-demo.el、
                  示例文本、run-tests.sh
archive/          历史原型;不参与加载,仅作参考
```
