# Emacs-KP 开发者文档

本文档描述 `emacs-kp` 的实际内部架构、算法与 API,面向贡献者和高级用户。

当前仓库健康度与后续工作的优先级见
[2026-07-28 系统审计](./Docs/REPOSITORY_AUDIT_20260728.md)。

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
| `dp-cache` | 以 `equal` 比较的哈希:完整 DP 签名 → dp-result plist |

段落缓存(`ekp--para-cache`)以 `equal` 比较结构化 key——字符串内容、
文本属性区间的打印形式、检测字体、断词语言(`ekp-latin-lang`)、九个
显式间距值或自动 CJK stretch 默认值。其他自动值由字体测量派生,已
由字体与显示上下文字段表达。结构化 key 使哈希碰撞无害(旧的
`sxhash` 整数方案理论上可能串段)。超过 `ekp-para-cache-limit` 时
整体清空。单条快路径(`ekp--last-para`)只绕过哈希 lookup,复用前仍
比较同一个完整结构 key。因此排版相关 text property 的原地修改会让
两条路径都 miss,并得到与 fresh paragraph 相同的结果。

DP 签名独立于段落 key,包含行宽、looseness 与全部六个运行时代价参数。
参数变化会选择新结果,无需丢弃与宽度无关的段落数据;结构化 `equal`
比较也让非零 looseness 签名能够正常命中缓存。

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
`ekp--line-stripped-space-pixel` 统一拥有 1D/loose DP、C 结果重建与
渲染层使用的这一排除规则。

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

`ekp-layout-plan` 是布局决策与渲染方式之间的表示边界。它把段落盒子
及其原文 offset、DP 断点、逐行 glue 目标、缩进、边缘剥离和可选断词
决策组合成 `ekp-layout-plan`、`ekp-layout-line` 和 `ekp-layout-gap`
记录。plan 不包含 buffer 位置或具体显示机制。
`ekp-render-layout-string` 用它服务现有字符串 API;buffer 集成可复用
完全相同的决策,无需重新运行或重新解释 KP 算法。

两个消费方拥有不同且明确的表示权限。

#### 字符串渲染器

`ekp-render-layout-string` 保持公开字符串 API 兼容。它剥离首尾空格
盒、合成 display 空格与视觉换行,并在选中断词点附加继承样式的连字符。
返回字符串通过四类私有标记保持无损:

| 属性              | 位置             | 值 / 含义                 |
|-------------------|------------------|---------------------------|
| `ekp-glue`        | 合成的 glue 空格 | 它所替换的原文            |
| `ekp-soft-break`  | 合成的 `\n`      | 断点处吞掉的边界空白      |
| `ekp-soft-hyphen` | 合成的连字符     | 仅作标记                  |
| `ekp-hidden`      | 段落边缘文本     | 保留源文本、显示为空      |

这份物理表示只存在于返回字符串中,用于兼容既有 API,绝不会安装到源
buffer。`ekp--layout-marker-properties` 统一拥有其标记词汇表与不继承
契约。

#### Buffer 渲染器

`ekp-buffer.el` 保持 buffer 字符序列不变,只在现有源字符上用文本属性
投影同一份 plan:

- 源 ASCII 空格:
  `((space-width FACTOR) (min-width ((TARGET-PIXELS))))`;
- 无源空格的 CJK/混排 glue:把 `min-width` 加在前一个完整字素上,
  目标为其自然 advance 加 glue;
- 缩进:`line-prefix`;
- 源空白断点:第一个边界字符显示成换行,其余显示为空;
- CJK 或拉丁断词断点:replacing display string 重现已有完整字素,
  接上可选连字符和视觉换行。

`ekp-buffer--display` 与 `ekp-buffer--line-prefix` 记录精确 owner。
移除时只有公开属性的值仍与 EKP owner 值相同时才清除,因此后来的外部
修改不会被误删。带外部 `display`、`line-prefix`、`wrap-prefix`、
`composition` 或 `invisible` owner 的段落保持 verbatim。若精确排版
要求缩窄 tab/非 ASCII 空白也会拒绝,因为 `space-width` 只影响 ASCII
空格。

所有安装/移除都在 `with-silent-modifications` 内完成,自有属性设为
nonsticky。因此 buffer 字符、point/mark、modified 状态、undo、字符
修改 tick 与外部 change hook 仍完全由源编辑拥有。任何 EKP buffer
路径都不会创建 overlay。重投影直接恢复 mark marker,不会调用会激活
选区的 `set-mark`,随后独立恢复 `mark-active`;因此布局不能把旧 mark
变成选区。

保存、普通搜索、语法和直接 Elisp 字符 API 不再需要逻辑文本 adapter:
真实 buffer 本来就是逻辑文本。复制过滤仍有必要,因为
`buffer-substring` 按设计保留文本属性。EKP 先组合原有
`filter-buffer-substring-function`,再只移除自己在复制字符串中的投影
元数据。

#### 实时流式排版

实时编辑直接消费正常的整段 Knuth-Plass plan,不会把编辑状态交给
核心规划器:

1. live state 按最窄窗口权威宽度持有最后一次提交的硬行源文本、正常
   `ekp-layout-plan`、语义签名、投影 span 与稳定断行锚点。
2. 第一次真实变更打开一个编辑事务。事务先快照提交态,再只移除包含该
   编辑的最小投影 span 范围中的 EKP 属性;span 对象与无关锚点仍保持
   注册。
3. 脏岛内的后续变更交给原生软折行,不规划整条硬行。若逻辑源文本精确
   回到快照,EKP 直接恢复保存的自有属性区间与 marker 偏移;state、plan、
   signatures 和 spans 都保持对象身份。
4. 跨过原生视觉行边界才提交。EKP 计算或复用一次整条硬行 plan,得到
   全部已完成语义行,并在同一个 command-loop 转换中静默发布真正变化的
   后缀;新的当前行保持自然。
5. 其他提交事件只有硬换行/段落完成、下一次真实编辑发生在脏岛之外、
   显式 refill,以及宽度/字体/face/主题或布局上下文变化。point 移动永远
   不是提交,即使跨越硬段落也一样;live 路径没有 `post-command-hook`。
6. 稳定行签名只负责在提交时减少属性写入,buffer-local 16 项 LRU 只负责
   复用近期文本/上下文 plan。何时允许布局变化由事务控制,不是二者控制。
7. IME preedit、外部显示所有权、不支持的收缩、超长硬行、过期
   generation 或发布错误都会 fail closed 到原生显示。半成品投影会
   回滚,原始错误继续向外报告。

原生软折行是状态机的前提,不能只希望用户碰巧开启它。mode 启用时先
保存 `truncate-lines` 和 `truncate-partial-width-windows` 的值与
buffer-local 所有权,再把二者设为 buffer-local `nil`;teardown 时恢复
原局部值,或移除临时局部绑定让全局值重新接管。这样 Emacs 默认的
50 列分栏阈值就不会悄悄把窄分栏变成横向滚动。

这个状态模型不需要 live lookahead、push/pull 收敛、逐键整行规划器或
idle formatter。point 移动本身严格零副作用;之后若在别处发生真实编辑,
则允许提交先前活动硬行,即使 narrowing 让该行已不在可访问区内。

不存在编辑空闲后整段 formatter。resize/后台工作都带 generation。
大 buffer 按可见优先的硬段落 chunk 处理;单个硬段落超过
`ekp-auto-justify-paragraph-limit` 时,自动路径保持自然显示,只有显式
`ekp-refill-paragraph` 才执行无界完整质量 pass。文本属性属于 buffer,
所以以最窄活动窗口作为唯一权威宽度。

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
  `ekp--line-spec`(行号 → 缩进 . 宽度)解析。纯首行缩进只改第 0 行,
  而"以盒 0 开头的行"恰好对应 DP 起点 i = 0,故 1D 遍(以及 C 引擎,
  经 `FIRST-LINE-WIDTH` 参数)无需额外状态即可处理;只有完整的
  `ekp-parshape` 和 `ekp-looseness` 才需要(位置×行数)DP 并旁路 C。
  缩进渲染为行首 `ekp-glue` 垫片。

C 模块 1.6:`ekp-c-break-with-arrays` 15 参(…、forbidden-positions、
tail-protrudes、hyphen-protrude、first-line-width);batch 向量 15 元;
`ekp-c-set-penalties` 4–7 参。

特性完成后的性能(字节编译 + C,Apple Silicon,batch):justify zh
w=200 ≈ 54 ms、range zh ≈ 117 ms——justify 与特性前持平,range 因盒
数增加约 +55%。热路径缓存:`ekp--str-type` 按字符记忆化、glue 字符
串驻留、(段落, 宽度) 渲染结果缓存进 dp-cache(上限 64 个宽度)。连
续变宽实测(60 段 2.6 万字文章,含 region 层全链路):每次变宽约
73 ms,重访宽度更快;编辑后单段增量重排约 17 ms。

`ekp-bench-adversarial-builders` 单独测量源码解释模式下的退化
builder。输入从 1,000 增到 8,000 字符时,基于片段的 tokenizer 增长
6.3×,密集插入增长 7.7×,接近输入 8× 的线性增长。8,000 字符分别从
3.133 s 降到 1.100 s、从 0.945 s 降到 0.013 s。断词位置缓存使用
显式 miss sentinel,因此合法的 nil 结果也能复用。

## 6. Looseness

`ekp-looseness` ≠ 0 时切换到 `ekp--dp-run-loose`:完整的
(位置 × 行数)DP,为每个行数保留最优路径,最终选取与
(最优行数 + looseness)最接近的行数,平局取 demerits 更小者。该路径
比 1D 重,仅有 Elisp 实现;looseness 激活期间 `ekp--c-available-p`
返回 nil,两引擎永不分歧。

## 7. C 模块集成

C 模块(`ekp_c/`,版本 1.6)只执行阶段 ④。所有字体相关数据以 Elisp
为唯一事实来源。

- `ekp-c-break-with-arrays`(15 参数):para 的前缀数组、glue 数组、
  断词数据、行宽、两个空格串数组、禁则/悬挂数组和首行宽度。返回
  `(breaks . cost)`。
- `ekp-c-break-batch`:15 元素向量的向量,由 pthread 线程池并行处理
  ——每段一个任务(这是正确的并行粒度;DP 本身天然串行)。线程池在
  首次多段落 batch 时惰性创建,按机器核心数定大小;队列满时提交方
  阻塞等待而非丢弃任务。
- `ekp-c-set-penalties`(4–7 参数):`ekp--c-sync-params` 在**每次**
  进入 C 之前调用,保证 `ekp-line-penalty` 等变量始终生效(回归:此
  前从未同步)。
- `ekp-c-module-load` 拒绝低于 `ekp-c-module-required-version` 的模块
  并回落到 Elisp,避免升级后的参数数量不匹配。

模块不可用、分配/无结果返回 nil 或 ABI 版本不兼容时回落到 Elisp。
直接 API 的非法输入 signal `ekp-c-invalid-input`;已启用后端发出的
任何 signal 都会穿过公共 formatter,dispatcher 不捕获或隐藏。模块
不会在部分失败时静默产出不同的排版。两引擎输出逐字节一致,由
`ekp-test-c-parity-simple` / `ekp-test-c-parity-files` 及 300 例性质
fuzz 验证。

### 未来方向:段落句柄 API

每次 `ekp-c-break-with-arrays` 调用都会重新编组段落的宽度无关数组
(约 18·n 次 `env` 提取)。单次 justify 时这无关紧要,但
`ekp-pixel-range-justify` 会对每个候选宽度重新编组同一批数组:即便
段落缓存已暖,C 路径每个宽度仍约 12 ms,其中大部分是编组而非 DP
(整篇样本的 DP 约 2.5 ms)。

解法是 `make_user_ptr` 句柄:`ekp-c-para-upload` 把数组一次性拷进 C
结构体并返回带 GC finalizer 的句柄,`ekp-c-break (handle, width)` 之后
只传两个宽度相关标量。它**有意**不纳入本次发布——这是引入 C 端对象
生命周期的破坏性(2.0)ABI 变更,而常见交互路径(单次 justify、
`ekp-auto-justify-mode`)本就命中 dp-cache、避开了重复编组。当宽度
搜索或超大批处理成为瓶颈时,这是明确的下一步。

## 8. 断词(ekp-hyphen.el)

普通 Liang 模式算法:

- `dictionaries/hyph_*.dic` 首次使用时编译为模式哈希并按路径缓存。
  文件可为 UTF-8 或 ISO-8859(Emacs 自动检测;由
  `ekp-test-hyphen-de-iso8859-dict` 验证)。
- `ekp-hyphen-create LANG` 先精确匹配,再逐级缩短(`"de_CH" → "de"`)。
- 断点两侧默认至少保留 2 个字符。
- 非注释 pattern 出现斜杠时失败关闭。libhyphen 替换规则只在断点
  胜出时改变可见文字与宽度,当前固定宽度 box 无法诚实表达;编译器
  计数后 signal `ekp-hyphen-unsupported-pattern`,公共排版入口保留该
  错误。
- `dictionaries/MANIFEST.tsv` 将 49 个条目固定到 LibreOffice 提交
  (另明确标记一个 legacy Basque 字节),记录 SHA-256、语法标记和许可
  证据。`tests/check-dictionaries.sh` 做离线门禁,
  `dictionaries/update.sh check` 在 macOS/Linux 对照固定上游字节。

词盒按 `^[左标点]* (拉丁词) [右标点]*$` 匹配,因此被标点包裹的词
(`(word)`、`word!`、`»word«`)仍可断词;标点粘在首/末音节盒上。

## 9. 测试与基准

```bash
tests/run-tests.sh [emacs]        # batch 可跑的 ERT 测试集
tests/run-tests.sh [emacs] --random-order
tests/run-tests-isolated.sh [emacs] # 每个 ERT 使用全新进程
tests/check-dictionaries.sh           # 离线清单/校验值门禁
dictionaries/update.sh check          # 核对固定上游字节
make -C ekp_c PROFILE=portable      # 默认可移植发布构建
make -C ekp_c PROFILE=native        # 仅本机基准
make -C ekp_c PROFILE=debug         # 调试符号,不优化
make -C ekp_c PROFILE=sanitize      # ASan + UBSan
emacs -Q --batch -L . --eval '(setq ekp-use-c-module nil)' -l tests/ekp-bench.el
emacs -Q --batch -L . --eval '(progn (require (quote ekp)) (ekp-c-module-load))' \
      -l tests/ekp-bench.el
```

可用 `EKP_TEST_SEED` 复现或改变乱序。测试 fixture 会动态恢复其隔离的
全部 EKP 配置；分派类测试必须经过公开排版入口，不能只断言内部资格
谓词。

GUI 矩阵需显式加载 `tests/ekp-gui-verify.el`。任一行失败时，它先打印
完整表格，再以状态码 1 退出；ERT 套件包含该边界的强制失败负控。

`M-x ekp-c-module-build` 使用同一组四种 profile，并在 `ekp_c/` 中以
argv 直接启动 make，不再构造 shell `cd` 命令。发布/CI 使用
`portable`；`native` 仅用于将在同一机器运行的基准。

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
ekp-buffer.el     纯文本属性 buffer/region 投影、同步实时流动、窗口
                  lifecycle、复制过滤与诊断
ekp_c/            C 动态模块(见 ekp_c/README.md)
dictionaries/     Hunspell 断词模式(来自 LibreOffice)
tests/            ekp-tests.el、ekp-buffer-tests.el(ERT)、
                  ekp-fuzz.el(一致性 fuzz)、ekp-bench.el、
                  ekp-demo.el、ekp-showcase.el、示例文本、run-tests.sh
```
