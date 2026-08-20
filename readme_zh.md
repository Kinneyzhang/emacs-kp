# Emacs-KP: Knuth-Plass 排版算法 Emacs 实现

[English Documentation](./readme.md) | [开发者指南](./DEVELOPER_ZH.md) | [仓库审计](./Docs/REPOSITORY_AUDIT_20260728.md)

Emacs-kp 在 Emacs 内部完整实现了 Knuth-Plass 最优断行算法,支持中日韩
(CJK)与拉丁文混合排版。

## 特性

- **全局最优断行** — Knuth-Plass 动态规划求段落全局最优断点,而非贪心
  首次适应。
- **CJK 支持** — 每个 CJK 字符都是可断行的盒子;避头尾规则保证标点正确
  附着(`，。` 不出现在行首,`「《` 不出现在行尾);汉字间距、中西文间距
  独立可调。
- **连字符断词** — Frank Liang 算法(TeX 同款),内置 49 份带固定来源
  与 SHA-256 的 Hunspell pattern 词典。
- **像素级两端对齐** — 一份语义 layout plan 同时驱动字符串与 buffer
  渲染器。字符串 API 使用像素空格;buffer 组合 `space-width` 与绝对
  像素 `min-width`,不插入排版字符也能支持变宽字体。
- **干净且可编辑的 buffer** — buffer 命令不创建 overlay,也不会向字符流
  加入 glue 空格、软换行或断词连字符。`buffer-string`、`char-after`、
  搜索、语法、保存及普通 Elisp 文本 API 看到的都是源字符。
- **文本属性保留** — face、颜色等属性完整保留;断词插入的连字符继承所
  在单词的样式。
- **困难输入不丢内容** — 未受保护的超长 token(URL、窄栏长词)退化为紧急
  断行而不是吞掉文本;任何输入都有输出。
- **可选 C 模块** — 动态模块用 C 执行 DP,线程池并行处理多个段落(见
  性能数据)。

## 环境要求

- Emacs **29.1+**(依赖 `string-pixel-width` 与 `object-intervals`)
- 可选(C 模块):C11 编译器和 pthreads

## 安装

克隆仓库并加入 `load-path`(`dictionaries/` 目录须与 `.el` 文件同级):

```elisp
(add-to-list 'load-path "/path/to/emacs-kp")
(require 'ekp)
(require 'ekp-buffer)   ; buffer/region 命令
```

强烈建议字节编译——编译后 Elisp 引擎约快 10 倍。

## 快速开始

```elisp
(require 'ekp)

;; 按 600 像素宽度两端对齐
(insert (ekp-pixel-justify "这是一段测试文本..." 600))

;; 在范围内寻找最优宽度,返回 (对齐文本 . 最优宽度)
(ekp-pixel-range-justify "测试文本" 400 800)
```

多行字符串按行分段处理,空行保留。

### C 模块(长文本推荐)

```bash
cd ekp_c && make PROFILE=portable # 默认;产出 ekp.dylib/.so/.dll
```

```elisp
(ekp-c-module-load)     ; 显示 "ekp-c module loaded (version 1.6, N threads)"
(ekp-c-module-build)    ; 选择 portable/native/debug/sanitize
```

加载后(`ekp-use-c-module` 默认为 `t`)所有排版调用自动走 C 引擎。
Elisp 与 C 两个引擎的输出**完全一致**;未启用模块或 C 返回 nil 时走
Elisp。已启用模块若 signal,则作为后端契约错误直接呈现。若磁盘上的
模块版本旧于 Elisp 代码的要求,加载会拒绝并提示重新编译。

自动 live append 另有 `ekp-auto-justify-native-append` 开关,默认开启。
当兼容模块已经加载时,auto-mode 可让已准备好的 append DP 走 native,
即使 `ekp-use-c-module` 为 nil；完整字符串/buffer 排版仍遵守
`ekp-use-c-module`。将该开关设为 nil 可强制 live append 使用纯 Elisp；
模块不可用时会自动回退。

## 交互使用(buffer 与 region)

`ekp-buffer.el` 把字符串 API 变成 buffer 级命令:

```elisp
(require 'ekp-buffer)
```

- `M-x ekp-justify-region` — 把选区排版到窗口文本宽度(数字前缀参数
  可指定像素宽)。没有激活选区时,排版光标所在段落。
- `M-x ekp-justify-buffer` — 排版整个 buffer。
- `M-x ekp-unjustify-region` / `ekp-unjustify-buffer` — 移除 EKP 的
  显示投影。buffer 排版从未替换源文本,因此不需要“还原字符”。
- `M-x ekp-auto-justify-mode` — 让已完成硬段落保持按窗口宽度排版,
  同时让普通输入保持稳定。活动硬行由“已提交投影 + 一个局部编辑事务”
  组成。同一原生视觉行内输入时,不会规划整条硬行,也不会改写无关的已
  投影行。编辑中间已投影行时,只让该行的真实脏岛恢复自然;后续断行锚点
  保持不动,局部单词迁移交给 Emacs 原生软折行。输入自然跨入下一视觉行
  时,EKP 才调用或复用一次完整 `ekp-layout-plan`,并原子发布所有已完成
  行;新行继续自然显示。删除后插回完全相同的源文本会立即逐属性恢复保存
  的完整投影。
  仅移动 point 永远不会规划或写布局属性,即使 point 离开段落也是如此。
  全局提交只发生在视觉行跨越、硬换行/段落完成、下一次真实编辑发生在
  别处、显式 refill,或宽度/字体/布局上下文变化时。不存在编辑后空闲触发
  的整段跳变。窗口尺寸变化仍通过
  `ekp-auto-justify-resize-delay` 防抖。
  mode 启用期间会临时关闭显式行截断以及 Emacs 的窄分栏窗口截断,
  因此左右分栏再窄也会正常软折行;关闭 mode 时会精确恢复这两个变量
  原来的 buffer-local 或全局所有权。
  活动行首尾输入一个空格或 tab 会在同一次输入中立即可见;删除其后的
  字符也不会把该源空白隐藏。重投影还会分别保持 mark 位置与
  `mark-active`,因此改变宽度不会把旧 mark 变成意外选区。
  mode 激活时,标准 **EKP** 菜单提供排版、保护与窗口适配诊断命令;
  `C-h m` 也会说明同一套流程。

投影只使用现有源字素上的文本属性:

- 源 ASCII 空格使用
  `((space-width FACTOR) (min-width ((TARGET-PIXELS))))`。
- 没有源空格的 CJK/混排间距,把 `min-width` 加到前一个完整字素上,
  目标值为“字素自然 advance + glue”。
- 缩进使用 `line-prefix`;视觉断行与断词连字符使用挂在现有完整字素上的
  replacing display string。
- EKP 绝不创建 overlay,也不抢占外部的 replacing `display`、
  `line-prefix`、`wrap-prefix`、`composition` 或 `invisible`。有冲突
  的硬段落保持自然显示,`M-x ekp-diagnose` 会报告原因。

因此:

- **Elisp API 与保存**直接看到原始字符序列,视觉空格、换行和连字符不可能
  进入磁盘、语法或搜索逻辑。`buffer-substring` 会保留文本属性,所以可能
  携带 EKP 的显示属性;`buffer-substring-no-properties` 是纯源字符串。
- **搜索**(包括 isearch)直接搜索源文本,一个拉丁单词不会因为视觉断词而
  变成两个词。
- **复制/剪切**会在组合已有 substring filter 的同时移除 EKP 自有投影
  属性;粘贴内容只包含逻辑文本及非 EKP 属性。
- 投影更新包在 `with-silent-modifications` 中:启用、编辑、缩放及关闭
  排版不会制造仅由布局引起的 undo 条目、modified 状态或字符修改 tick。

`ekp-buffer-margin-pixel`(默认 2)是从窗口宽度中扣除的取整安全边距。

大 buffer(超过 `ekp-auto-justify-lazy-threshold` 字符,默认 2 万)
自动改为可视优先重排:屏幕内的部分同步完成,其余在空闲时后台分块
补齐,每个时间片有时间预算(`ekp-auto-justify-tick-budget`),并优先
处理你滚动到的区域。

自动规划还按硬段落设有上限。`ekp-auto-justify-paragraph-limit` 默认
2 048 字符。更长的单个硬段落保持自然折行与完整可编辑性,避免一次无界
Knuth-Plass 计算阻塞输入;`M-x ekp-diagnose` 会报告这一原因。确实需要
对该段执行无上限完整质量排版时,显式运行 `M-x ekp-refill-paragraph`。

各 mode 的显式本地保护预设——各一行:

```elisp
(add-hook 'org-mode-hook      #'ekp-org-setup)
(add-hook 'markdown-mode-hook #'ekp-markdown-setup)
```

在 Org 与 Markdown buffer 里,若你没有自定义配置,
`ekp-auto-justify-mode` 会自动查询 `ekp-buffer-mode-policy-alist`。
它不会把 profile 值复制成 buffer-local 变量;只有显式调用上面的
setup 函数时才会写入本地 face 列表。

### 保护代码块与 verbatim 文本

- 段落级:携带 `ekp-verbatim` 文本属性(`M-x ekp-verbatim-region`)、
  face 在 `ekp-buffer-skip-faces` 列表中(如 `org-block`、
  `markdown-code-face`)、或被 buffer-local 的
  `ekp-buffer-skip-predicate` 判定的段落**原样跳过**,一个字节都不动。
- 自动行内级:face 在 `ekp-buffer-inline-faces` 中的精确区间
  (Org 的 `org-code`/`org-verbatim`,Markdown 行内代码默认由 mode
  profile 提供)使用 `ekp-inline-code-policy`。默认 `no-hyphen`
  保留源空格字面宽度并禁止词典断词,但仍可在合法源边界换行。自动
  `no-break` 区间若宽于有效栏宽,会降级为 `no-hyphen`。
- 显式硬原子级:带 `ekp-no-break` 属性的区间(`M-x ekp-no-break-region`)
  成为刚性原子——不断行、不断词、空格保持字面宽度——适合行内代码、
  产品名、数字加单位。原子宽于栏宽时仍保持完整,但不保证独占一行:
  最终遍可能把它与前面的普通内容放在同一条溢出行。普通欠宽候选使用
  固定的 emergency stretch 并按正常 K-P 代价评分;若超宽候选会让最终遍
  的活动路径全部消失,核心按 TeX 的 artificial demerits 语义保留最后
  路径。这里没有中文孤字、单位或截图专用规则。

手动属性明确只在**当前 buffer 会话**有效:普通文本保存与重新打开不会
恢复它们。使用 `M-x ekp-allow-break-region` /
`ekp-clear-verbatim-region` 清除。需要从持久文档语法派生保护时,使用
mode face 或 buffer-local 的 `ekp-buffer-skip-predicate`(Org/Markdown
预设会自动这样做)。

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
  首行缩进走高速的 1D 路径与 C 引擎;只有完整的 `ekp-parshape` 和
  `ekp-looseness` 才回落到 Elisp 专属的 2D 动态规划。
- **不可断字符** — NBSP、窄 NBSP、数字空格、WORD JOINER 天然把两侧
  锁在同一行。
- 禁则覆盖全角**与半角**标点:行首不会出现 `。、」!?` 或独立的
  `.,;:!?`,行尾不会出现 `「(` 等。日文行首禁则还覆盖小假名、长音
  符和叠字符(`っ ょ ー 々`),可通过 `ekp-cjk-no-line-start-extra`
  配置。

已知限制:行中的 CLREQ 标点**压缩**(如「字。下」行内挤压)无法渲
染——Emacs 不能缩减字形 advance——因此行边压缩以悬挂方式呈现;左缘
悬挂同理不可渲染(文本无法起笔于行原点之前)。

## 配置

### 断行策略与测量宽度

默认策略的目标是让代码可读,但不把所有"看起来像代码"的片段都变成
硬原子:

| 选项 | 默认值 | 作用域 | 含义 |
|:-----|:-------|:-------|:-----|
| `ekp-inline-code-policy` | `no-hyphen` | 全局、profile、本地 | 行内 face 的 `normal`、`no-hyphen` 或自动适配 `no-break` |
| `ekp-hyphenation` | `auto` | 全局、profile、本地、区域命令 | `auto`/`on` 在词典可用时断词;`off` 禁止词典断词 |
| `ekp-token-break-policies` | URL/path/identifier `no-hyphen`,number-unit `no-break` | 全局、profile、本地 | 按 token 类别合并的自动策略 |
| `ekp-number-unit-suffixes` | 常见 CSS、时间、数据、频率、度量单位 | 全局、profile、本地 | 紧凑数字单位识别的后缀 |
| `ekp-kinsoku-profile` | `common` | 全局、profile、本地 | `common`、`zh`、`ja`、`off` 或 `custom` 禁则 |
| `ekp-cjk-no-line-start-extra` / `ekp-cjk-no-line-end-extra` | `""` | 全局、profile、本地 | `custom` profile 使用的附加禁则字符 |
| `ekp-overlong-token-policy` | `emergency` | 全局、profile、本地 | 普通超宽 Latin-like token 的 `emergency`、`overflow` 或 `natural` |
| `ekp-buffer-measure` | `narrowest-window` | 全局、profile、本地 | `narrowest-window`、固定像素整数或 `(max . PIXELS)` |
| `ekp-buffer-skip-faces` | 由 profile 提供 | 全局、profile、本地 | 段落级 verbatim face |
| `ekp-buffer-inline-faces` | 由 profile 提供 | 全局、profile、本地 | 使用行内策略的精确 face 区间 |
| `ekp-buffer-mode-policy-alist` | Org 与 Markdown profile | 全局/安全本地值 | 自动与手动 buffer 排版查询的 mode profile |

有效优先级固定为:显式区域文本属性 > 显式 buffer/file/dir-local 值 >
第一个匹配的 major-mode profile > 全局默认值。区域 `ekp-break-policy`
可取 `normal`、`hyphenate`、`no-hyphen`;它只覆盖精确区间内的自动
token/行内策略,不会创建硬原子。`ekp-no-break` 仍是唯一显式硬原子
属性,并且胜过所有自动策略。

支持 file/dir local 的上述变量都有封闭的 safe-local 谓词。
`M-x ekp-diagnose` 会报告请求宽度、最窄活动窗口、有效宽度、溢出风险、
冲突数量,以及当前行内/断词/禁则/超宽策略摘要。EKP 菜单提供诊断与
区域命令:普通断行、开启区域断词、关闭区域断词、清除区域断行策略、
no-break 与 verbatim。

### 断词语言

```elisp
(setq ekp-latin-lang "de_DE")   ; 默认 "en_US"
```

`"de"` 这类短代码会解析到第一个匹配的词典。受支持词典自身的
`LEFTHYPHENMIN` /
`RIGHTHYPHENMIN` 都会被遵守(英文在断点前保留 ≥2 字母、之后 ≥3);
给 `ekp-hyphen-create` 传显式边距可覆盖。

EKP 支持普通 Liang pattern。`eo`、`ca`、`hu_HU` 与 `sq_AL` 含有
斜杠/替换规则,会在选中断点时改写字形;把它们当成普通位置既会产生
错误拼写,也会让 DP 使用错误宽度。因此这些语言会明确 signal
`ekp-hyphen-unsupported-pattern`,而不是静默降级。完整文件清单、
SHA-256、固定来源路径和许可证据见 `dictionaries/MANIFEST.tsv` 与
`dictionaries/LICENSES.md`。

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

两个引擎都实现了这些参数:实际进入 C 计算前,Elisp 会同步当前值。
DP 缓存签名包含表中的全部参数,因此修改后下一次调用即生效,无需
手动清缓存。`ekp-looseness` 由专门的 Elisp 路径处理(非零时自动
绕过 C 模块)。

### 缓存

分词、测宽和 DP 结果按段落缓存;盒宽还额外做会话级缓存,跨段落共享
的字形整个会话只测量一次。
显式间距值和自动模式的 `ekp-default-cws-stretch-pixel` 都属于段落
缓存签名。同字符串快路径也使用同一个完整结构 key,因此在已缓存的
字符串对象上增删 `ekp-no-break` 等排版属性会立即生效。

- `ekp-para-cache-limit`(默认 256):缓存段落数上限,超过后整体清空。
- `M-x ekp-clear-caches` 清空所有缓存(更换字体或影响字宽的主题后使用)。

## 性能

基于内置示例文本(`tests/ekp-bench.el`)、batch Emacs 30.2、Apple
Silicon 测得;方法见 DEVELOPER_ZH.md:

| 场景(text-zh.txt ≈ 3.6KB) | Elisp(字节编译) | C 模块 |
|:----------------------------|------------------:|-------:|
| 两端对齐,宽 200px          |            150 ms |  41 ms |
| 最优宽度搜索 340–380        |            529 ms | 106 ms |
| 仅 DP,宽 400px             |             30 ms | 2.5 ms |

**请字节编译本包**——编译后 Elisp 引擎快约 10 倍。两引擎输出完全一
致;C 模块在最优宽度搜索和长多段文本上收益最大。(绝对数值随机器与
功耗状态波动,重点看比例。)

## 已知限制

- 文本属性属于 buffer,不能为同一 buffer 在不同宽度窗口保存两套 plan。
  EKP 以显示该 buffer 的最窄活动窗口为权威宽度;较宽窗口可能右侧留白,
  但不会溢出。
- 显式检查文本属性的 Elisp API 能看到 EKP 自有布局属性;干净保证针对
  字符流。复制/剪切会移除这些投影元数据。
- `space-width` 不能缩窄 tab 或非 ASCII 空白。若精确 plan 要求这种操作,
  EKP 会让受影响硬段落保持自然显示并报告冲突。
- 测量会跟随当前 buffer 的 face 重映射(`text-scale-mode`、主题等),
  并在无 fringe 的窗口里为截断指示符预留一列,排版行贴合真实显示。
  若在特殊配置下仍出现截断或偏短,在该 buffer 里执行
  `M-x ekp-diagnose`——它会报告测量与渲染是否一致。完整贴合矩阵是
  `tests/ekp-gui-verify.el` 中的开发工具;执行
  `M-x ekp-gui-verify` 前须先加载该文件。
- 计算默认间距时假定每段落的拉丁/CJK 各使用一种字体;混合字体段落可以
  工作,但默认间距取自找到的第一个字体。
- `ekp-pixel-range-justify` 用三分搜索加局部扫描最小化平均 demerits;
  代价关于宽度并非严格单峰,结果是很好的局部最优,不保证全局最优。
- batch/tty 模式下像素宽度退化为字符列数(整条管线仍可工作,便于测试)。

## 交互式演示

```bash
emacs -Q -L /path/to/emacs-kp -l tests/ekp-showcase.el -f ekp-showcase
```

单 buffer 按键交互:`-`/`+` 增减像素宽度(头行实时显示本次重排毫秒
数),`d` 宽度扫掠动画并报告 fps,`a` 循环四种对齐,`p` 标点悬挂,
`i` 首行缩进,`s` 楔形 parshape,`c` 切换 C/Elisp 引擎对比性能,
`w` 进入跟随窗口宽度模式(真实 `ekp-auto-justify-mode`)。样例文本
内置受保护代码块、行内不可断原子与 NBSP 锁定的数字。

## 测试

```bash
tests/run-tests.sh /path/to/emacs     # 全部支持 batch 的 ERT 测试集

# 完整的交互式 GUI 贴合矩阵
emacs -Q -L /path/to/emacs-kp -L /path/to/emacs-kp/tests \
  -l /path/to/emacs-kp/tests/ekp-gui-verify.el \
  -f ekp-gui-verify-matrix
```

矩阵会打印全部行；任一贴合检查失败时以状态码 1 退出，因此同一命令可
作为本地发布门禁。验证器是 `tests/` 下的开发工具，
`(require 'ekp-buffer)` 不会加载它。

## 致谢

- **核心算法**: ["Breaking Paragraphs into Lines"](https://gwern.net/doc/design/typography/tex/1981-knuth.pdf) by Donald E. Knuth and Michael F. Plass (1981)
- **断词算法**: Frank Liang 算法,改编自 [Pyphen](https://github.com/Kozea/Pyphen)
- **词典**: 断词模式来自 [LibreOffice dictionaries](https://github.com/LibreOffice/dictionaries);准确来源、校验值和许可证据见 `dictionaries/MANIFEST.tsv`、`dictionaries/LICENSES.md` 与随附的各词典说明。
