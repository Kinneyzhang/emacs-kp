# KP 算法系统性优化 — 进度交接文档

> **2026-07-28 superseded note:** 本文的“未完成事项”保留为 2026-07-26
> 历史快照;后续审计、整改和验证状态以
> `task_repository_audit_20260728.md` 及 `.phrase/docs/ISSUES.md`
> 为准。
>
> 阶段:phase-kp-overhaul-20260726
> 状态:**已完成并提交**——分支 `kp-overhaul`(基于 main@11437cb),
> 合并到 main:`git checkout main && git merge kp-overhaul`
> 审查状态:自查 + 300 例随机性质测试已完成(0 失败);多智能体 workflow 审查仍可选(见"未完成事项")
> 本文档面向下一次会话/贡献者,保证无缝衔接。

## 〇、续做增量(同日第二轮)

- **新发现并修复缺陷 #15**:缓存 key 与 `ekp--last-para` 快路径均未包含
  `ekp-latin-lang`——切换断词语言后同一字符串返回**旧语言的断词结果**
  (旧代码同样存在此 bug,已实测复现)。修复:key 与快路径都纳入语言;
  新增回归测试 `ekp-test-para-cache-tracks-language`。测试总数 35 → **36**,
  全部通过;byte-compile 仍零警告。
- **300 例随机性质测试通过(0 失败)**:随机中西混排/CJK 标点/ZWSP/双空格/
  超长词 × 随机宽度 1–300px,断言 ① C 与 elisp 输出逐字节一致 ② 内容零丢失
  ③ 代价有限 ④ 不报错。脚本已固化为 tests/ekp-fuzz.el(确定性 LCG 种子 42,可复现;
  需 C 模块,单独运行:emacs -Q --batch -L . -l tests/ekp-fuzz.el)。

## 一、本阶段目标(原始指令)

清除未提交文件 → 全面分析仓库 → 找出 kp 算法设计缺陷与未完善功能 →
系统性优化,确保功能全部实现、性能实测 → 重写/完善文档。

## 二、提交内容

以下改动已在 `kp-overhaul` 分支提交(refactor! 单提交,含本文档):

```
M  ekp.el              # 核心重写:DP、缓存、参数、渲染、C 桥接
M  ekp-utils.el        # 字体检测 batch 回退、全角/组合字符修复、删 Rust 死代码
M  ekp-hyphen.el       # 仅 docstring 修正
M  ekp_c/ekp_kp.c      # 两遍紧急策略、badness 封顶、空格数组、参数化 penalties
M  ekp_c/ekp.c         # API v1.1:break-with-arrays 11 参、set-penalties 4-6 参
M  ekp_c/ekp_module.h  # 版本 1.1、结构体新字段
M  tests/ekp-tests.el  # 全新 ERT 套件(36 个测试)
A  tests/ekp-bench.el  # 基准脚本
A  tests/ekp-demo.el   # 交互式 demo(从旧 tests 迁移)
A  tests/run-tests.sh  # 一键跑测试
A  tests/ekp-fuzz.el    # 300 例随机性质测试(需 C 模块,单独运行)
M  readme.md / readme_zh.md / DEVELOPER.md / DEVELOPER_ZH.md / ekp_c/README.md
```

`ekp_c/ekp.dylib` 已用新源码重新编译(版本 1.1,gitignore 忽略编译产物)。
会话开始时已按指令 `git clean -fd` 清除了全部 Syncthing sync-conflict 垃圾文件。

**建议提交信息**(Conventional Commits,单提交或按 fix/perf/test/docs 拆分):

```
refactor!: overhaul KP core — correctness, C parity, performance, tests, docs

- fix: ekp-param-set silently reset after first justify (now persists; ekp-param-reset added)
- fix: narrow-width CJK returned empty string (data loss); two-pass emergency breaks
- fix: penalties never synced to C module; space-box metrics divergence C vs elisp
- fix: fullwidth letters/digits misclassified as CJK punctuation
- fix: combining chars split from base char in tokenizer
- fix: punctuation-wrapped words (word!/(word)/word;) never hyphenated
- fix: para cache hash-collision aliasing (equal-keyed structured keys + limit)
- fix: renderer double-counted stripped space widths; negative glue clamped
- feat: real looseness support via (position × line-count) DP
- perf: O(1) line metrics/gap counts (was O(n) allocs in O(n²) loop);
        box measurement dedupe; eq fast-path para lookup; C module 3-19× faster
- test: 36 batch-safe ERT tests + 300-case property fuzz replacing ad-hoc suite
- docs: all five docs rewritten to match implementation

BREAKING: requires Emacs 29.1+; C module must be rebuilt (v1.1, arity changes);
ekp-threshold-factor / ekp-flagged-penalty / ekp-forced-break-penalty removed;
Rust module stubs removed.
```

## 三、已完成工作(按类别)

### 1. 实测确认并修复的正确性缺陷(elisp)

| # | 缺陷 | 修复 |
|---|------|------|
| 1 | `ekp-param-set` 一次性失效:第二次排版起用户参数被静默重置 | 显式参数持久化(`ekp--params-explicit`),新增 `ekp-param-reset`;auto 模式按字符串派生 |
| 2 | 超窄宽度 CJK 整段返回空串(数据丢失);超长不可断词产生负宽 glue | 两遍 DP:严格遍 + 紧急单盒断行遍(仅在段尾不可达时);glue 钳制 ≥0 |
| 3 | `flagged-positions` 死代码(从未填充)、`ekp-threshold-factor` 剪枝语义可疑 | 连同 `ekp-flagged-penalty`/`ekp-forced-break-penalty` 一并删除 |
| 4 | para 缓存用 sxhash 整数 key,碰撞会串段 | `equal` 结构化 key(内容+属性区间+字体+参数或 `auto`)+ `ekp-para-cache-limit`(256) |
| 5 | 全角字母/数字(ＡＢＣ１２３)被当标点附着到前字 | `ekp-cjk-fw-punct-p` 排除 FF10-19/FF21-3A/FF41-5A |
| 6 | 组合字符(café NFD)被当空格拆成独立 box | 零宽附着类(Mn/Mc/Me、ZWJ/ZWNJ、变体选择符)并入前文;ZWSP 仍作断点 |
| 7 | `word!`、`(word)`、`word;` 等不断词(正则类不全) | 左右标点类补全(`ekp--word-left/right-punct`) |
| 8 | batch/tty 下 `font-at` 崩溃,包完全不可用 | 字体检测全部加 `display-multi-font-p` 回退 → 测试可自动化 |
| 9 | 宽度 ≤0 静默吞文本 | `user-error` 校验;非字符串输入 `wrong-type-argument` |
| 10 | looseness ±1 无效(alt-paths 只延伸最优前缀,状态不闭合) | 真正的 (位置×行数) 2D DP(`ekp--dp-run-loose`) |
| 11 | 断词连字符不带样式;宽度按无属性 "-" 测量 | 渲染继承所断词属性;宽度按字符串首字符属性测量 |
| 12 | 渲染层剥离空格 box 后又把宽度再分配(与 DP 的排除度量双重计算) | 删除再分配;DP 契约:度量已排除,行宽精确 == 目标(有测试锁定) |
| 13 | force-break demerits 不累计前缀(与 C 不一致) | 统一为紧急断行公式 `(lp+10000)²+rest²`,两引擎一致 |
| 14 | shrink 容量计算不含 cws(与 min-prefix 可行域矛盾) | badness/分配均含 cws-shrink |

### 2. C/Elisp 一致性(全部实测验证)

- **参数同步**:`ekp--c-sync-params` 每次进 C 前推送 6 个 penalty(C `ekp-c-set-penalties` 扩为 4-6 参;consec-hyphen/last-line-short 不再硬编码)。
- **badness 封顶**:C 侧超 10000 曾变 `EKP_INFINITY`(断点被丢),现与 elisp 一致封顶 10000。
- **空格 box 度量**:新增 `lead-spaces`/`trail-spaces` 数组(n+1)传给 C;`ekp-c-break-with-arrays` 9→11 参,batch 向量 9→11 元素。
- **两遍紧急策略**:C 与 elisp 完全相同(严格遍 → 不可达时紧急遍)。
- **版本门禁**:模块版本 1.1;`ekp-c-module-load` 拒绝旧模块并回落 elisp(`ekp-c-module-required-version`)。
- **looseness ≠ 0 时自动绕过 C**(`ekp--c-available-p`)。
- **验证结果**:6 个测试文件 × 5 宽度 = 30/30 输出逐字节一致;penalty 极值下同样一致。

### 3. 性能(实测,batch Emacs 30.2,Apple Silicon,3 次冷缓存取最小)

| 场景 | 改造前 elisp(解释) | 改造后 elisp(编译) | 改造后 C |
|------|-----:|-----:|-----:|
| justify 中文 w=200 | 7547 ms | **96 ms** | **57 ms** |
| justify 混排 w=300 | 5540 ms | 53 ms | 23 ms |
| range 中文 340-380 | 29696 ms | 294 ms | 75 ms |
| range 混排 280-320 | 68534 ms | 480 ms | 34 ms |
| 仅 DP(zh, w=400) | 2382 ms | 15 ms | **1.3 ms** |

(旧 C 模块对照:justify-zh-200 197ms / range-zh 430ms / DP 25ms → 新 C 快 3-19×)

关键优化:① 前缀计数数组使行度量/间隙统计 O(1)(旧内层每候选 O(n) 分配,总 O(n³));② 两遍法保持 DP 稀疏;③ 盒宽测量去重(段属性均匀时仅按字符串 key);④ `ekp--last-para` eq 快路径(消除每次 get-para 的 prin1+全串哈希);⑤ para 级 glue 数组跨 C 调用复用;⑥ bool-vector 连字符标志。

基线/复现脚本:`tests/ekp-bench.el`(改造前基线数字已录入 DEVELOPER*.md §9)。

### 4. 测试(tests/ekp-tests.el,36 个 ERT,**全部通过**(含语言切换回归))

覆盖:断词(en/de-ISO8859/边距/语言回退)、分箱(kinsoku 开闭标点/全角/组合字符/空格保留)、行宽不变式、任意宽度不丢内容、窄宽回归、非法参数、参数持久化/reset、参数同步到 C、looseness、缓存(命中/属性区分/上限/dp 复用)、O(1) 度量与暴力交叉验证、属性保留、连字符继承属性、range-justify、C/elisp 一致性(含 batch)。C 模块未编译时相关测试自动 skip。

运行:`tests/run-tests.sh /Applications/Emacs.app/Contents/MacOS/Emacs`

### 5. 文档(全部重写,与实现逐条对齐)

readme.md / readme_zh.md(用户指南 + 真实性能表 + 已知限制)、
DEVELOPER.md / DEVELOPER_ZH.md(五阶段管线、数据结构、demerits 公式与
TeX 差异、两遍策略、C 集成、基准方法学)、ekp_c/README.md(修正了
"wavefront 并行"“zero copy" 等与实现不符的旧说法;明确 `ekp-c-break-lines`
为实验路径)。**Package-Requires 已改为 Emacs 29.1**(string-pixel-width
/ object-intervals 实际要求;旧标注 27.1 不真实)。

### 6. 死代码清理

ekp-utils.el 的 Rust 模块支持(ekp_rust 目录不存在)已删除;
process 回调的 eval 式 lambda 改为词法闭包;byte-compile 零警告。

## 四、未完成事项(下次会话优先处理)

1. **多智能体对抗审查(可选)**:因会话限额(21:50 Asia/Shanghai 重置)
   未能以 workflow 形式执行;已用两项替代手段覆盖主要风险:
   ① 针对脚本 prompt 中列出的重点自查项逐项人工核查——eq 快路径过期
   (发现并修复了语言维度的真实 bug,见"续做增量")、C 空格数组索引
   (lead/trail 均 n+1 元素,i<n / k≤n 界内)、rest²/penalty² 均以 double
   计算无 int32 溢出;② 300 例随机性质测试 0 失败。如仍需 workflow 审查,
   脚本已存盘:
   `~/.claude/projects/-Users-geekinney-IPARA-3-RESOURCES-emacs-config-github-emacs-kp-ekp-c/ffca1e3d-9bda-4b18-814c-e95d7a8222c5/workflows/scripts/ekp-final-review-wf_03fd954b-f7a.js`
   (resumeFromRunId: `wf_03fd954b-f7a`),或直接 `/code-review`。
2. **GUI 真实字体视觉验证**:batch 下全部验证通过;真实字体渲染建议用户在
   图形 Emacs 里执行 `tests/ekp-demo.el` 中的注释示例
   (如 `(ekp-demo-justify "zh" "en_US" "Cascadia Next SC" 666)`)。
3. **合并**:改动已提交到 `kp-overhaul` 分支;确认后合并到 main
   (`git checkout main && git merge kp-overhaul`),如需推送再 `git push`。
4. (可选后续)`ekp-justify-region` 之类的交互命令、词典编码显式处理
   (目前依赖 Emacs 自动检测,de_DE ISO-8859 已实测正确)。

## 五、快速接续命令

```bash
EMACS=/Applications/Emacs.app/Contents/MacOS/Emacs
REPO=~/IPARA/3-RESOURCES/emacs/config/github/emacs-kp

# 全量测试(36 个,约 25s,C 模块存在时含一致性测试)
$REPO/tests/run-tests.sh $EMACS

# 重建 C 模块(改 ekp_c/ 后必须;版本门禁 1.1)
cd $REPO/ekp_c && make clean && make

# 基准(elisp / C 两引擎)
$EMACS -Q --batch -L $REPO --eval '(setq ekp-use-c-module nil)' -l $REPO/tests/ekp-bench.el
$EMACS -Q --batch -L $REPO --eval '(progn (require (quote ekp)) (ekp-c-module-load))' -l $REPO/tests/ekp-bench.el
```

## 六、关键设计契约(改动任何一侧都要维护)

1. **两引擎逐字节一致**:改 demerits/度量公式必须同时改 `ekp--dp-run-1d`
   与 `ekp_c/ekp_kp.c` 的 `dp_process_position`,并跑一致性测试。
2. **DP 与渲染的空格契约**:DP 度量排除行首(i>0)/行尾空格串
   (lead/trail-spaces 数组),渲染剥离同一批 box 且**不再**补偿宽度。
3. **紧急断行只在第二遍**:存在合法排版时结果必须是纯 K-P 最优。
4. **`ekp--last-para` 失效点**:任何影响 para 内容的全局状态变化
   (参数 apply/reset、clear-caches)都必须置 nil。
5. C 模块 API 变化必须递增 `EKP_VERSION_MINOR` 并同步
   `ekp-c-module-required-version`。
