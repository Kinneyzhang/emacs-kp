# emacs-kp 仓库系统审计

> 审计日期：2026-07-28
> 审计基线：`main@29cef97`（本地 tag `v1.0.0`）
> 范围：Elisp 核心、C 动态模块、buffer/region 集成、测试、性能、兼容性、文档、发布与扩展边界
> 性质：问题盘点与方向判断，不包含运行代码修复

## 后续状态

- `task002` 已在审计后的工作树中解决 P1-01：DP key 现在包含行宽、
  looseness 与全部六个运行时代价参数，并按结构相等比较。
- `task003` 已继续解决 P2-01：auto-CWS 默认值现在同时进入段落哈希键
  与最近段落快路径；`issue001` 的两个缓存缺陷均已闭环。
- `task004` 已解决 P1-02：保存从逻辑副本序列化，文件系统错误、编码
  错误与用户中断均不再改变显示 buffer；`issue002` 已闭环。
- `task005` 已解决 P1-03/P2-06：copy filter 可组合并恢复，integration
  生命周期由 auto mode 与实际 justified span 共同决定；`issue003`
  已闭环。
- `task006` 已解决 P1-04：测试 fixture 完整隔离配置，parshape 用例
  经过公开分派路径；CI、可复现乱序和逐例全新进程入口共同阻断顺序
  假绿，`issue004` 已闭环。
- `task007` 已解决 P2-05：GUI 矩阵共享结构化结果与报告断言边界，
  任一失败行在 batch 中退出 1；强制失败负控 2/2，真实 GUI 七场景
  7/7，`issue007` 已闭环。
- `task008` 已解决 P2-02：C 1.6 在提取前验证单段/批量完整 schema，
  非法调用统一 signal `ekp-c-invalid-input`，有效 int32 输入使用
  int64 中间量；模块 signal 不再被 Elisp 静默吞掉。
- `task009` 已解决 P2-03：默认构建改为 portable，native/debug/
  sanitize 需显式选择；交互命令以 argv 和 `default-directory` 启动
  make，含空格路径实测通过。`issue005` 已闭环。
- `task010` 已解决 P2-07 的仓库内边界：CI action 与 package-lint
  固定到完整提交，Windows 运行 Elisp 基线，`.phrase` 改为可追踪事实
  来源，release gate 与远端/产物 checklist 分责。未执行 push、tag
  变更或发布。
- `task011` 已解决 P2-04：49 份词典固定来源路径与 SHA-256，更新器
  可在 macOS/Linux 重建相同输出；4 份含斜杠/替换语法的词典明确失败
  关闭，不再静默降级。缺少准确许可证据的 Sanskrit 文件已移除。
- `task012` 已按 benchmark 修复 nil 缓存与长文本构造热点；
  `task013` 已补齐交互保护命令、反馈与 session-local 文档；
  `task014` 已集中行边空格与 render-marker 规则归属。
- 最终独立审查新增的 P1-05 已由 `task016` 解决：most-recent fast
  path 与 hash cache 共享完整 `ekp--para-key`，同一字符串对象原地
  修改排版属性也会失效；`issue010` 已闭环。
- `task015` 已完成终局清理与门禁：删除九个死 accessor 和依赖私有
  substring-filter API 的调用；最终独立代码审查 `APPROVE`、架构审查
  `CLEAR`，全部仓库内 issue/task 均已闭环。
- `task017` 已将当前编辑器集成模块直接改名为 `ekp-buffer.el`；
  下文的 `ekp-region.el` 与 `ekp-region-*` 仍保留为审计基线的历史
  证据，不代表当前加载入口。
- 下文保留 `main@29cef97` 的原始证据与判断，不把历史问题改写成从未发生。

## 结论先行

emacs-kp 的算法地基是可靠的：Elisp 掌握文本与字体事实，C 只镜像动态规划热路径；严格遍/紧急遍保证困难输入不丢内容；buffer 层用可逆布局标记保存逻辑文本；测试覆盖了核心算法、C/Elisp parity、编辑器协议与真实 GUI 像素贴合。

审计基线最需要处理的不是重写算法，而是四个已经实证的边界缺陷：

1. 算法 penalty 改变后，既有 DP 缓存仍按旧参数返回结果。
2. 保存失败会让原本已排版的 buffer 留在未排版状态。
3. EKP 直接覆盖已有的 `filter-buffer-substring-function`，可能破坏其他 mode 的复制语义。
4. 一个 C 绕过测试依赖前序测试泄漏的全局状态，整套测试为绿但单独运行失败。

**【Taste Score】** 🟡 Acceptable

设计方向正确，关键算法有真实测试；但缓存键、保存事务和编辑器协议组合这几个边界还没有单一 owner。继续堆 fallback、watcher 或 hook 会放大问题，下一步应先修正状态模型与边界契约。

**【Fatal Issues】**

- `ekp--dp-key` 没有覆盖所有影响 demerits 的参数，配置修改会静默失效。
- save/copy 集成不是可组合、可回滚的事务边界。
- 测试 fixture 没有恢复完整的全局配置，已经产生假绿。

**【Improvement Direction】**

- 先把“影响缓存结果的完整输入”变成显式键。
- 把保存、复制、isearch 视为编辑器协议边界，做可组合和失败恢复。
- 让每个测试独立运行，并让 GUI/本地 C 边界失败能可靠返回非零。
- C 模块继续只做 DP；除非 benchmark 证明跨边界复制是主要瓶颈，不要把字体、分词或渲染语义下沉到 C。

## 1. 审计方法与验证基线

### 1.1 读取范围

- 核心：`ekp.el`、`ekp-utils.el`、`ekp-hyphen.el`
- 编辑器集成：`ekp-region.el`
- C 模块：`ekp_c/*.c`、`ekp_c/*.h`、`ekp_c/Makefile`
- 测试与演示：`tests/*.el`、`tests/run-tests.sh`
- 文档与发布：README、DEVELOPER、CHANGELOG、CONTRIBUTING、CI、词典更新脚本、`.phrase`
- 历史决策：`.phrase/phases/phase-kp-overhaul-20260726/HANDOFF.md`、`.phrase/phases/phase-p1p2-20260726/NOTES.md`

### 1.2 新鲜验证

| 验证 | 结果 | 说明 |
|---|---|---|
| 默认 ERT 入口 | PASS，94/94 | Emacs 30.2，加载 C 1.5 |
| C/Elisp fuzz | PASS，300/300 | 字节级一致，0 failure |
| 临时干净副本 byte-compile | PASS | `byte-compile-error-on-warn=t` |
| 临时干净副本 checkdoc | PASS | 四个发布 Elisp 文件无输出 |
| 临时干净副本 C release build | PASS | C11、`-Wall -Wextra -Wpedantic` 无警告 |
| 临时干净副本 ERT + fuzz | PASS | 新构建 C 模块，94/94 + 300/300 |
| GUI 像素矩阵 | PASS，7/7 | base、缩放、face remap、无 fringe、窄窗均无 overflow |
| 单测独立负控 | FAIL（符合审计预期） | `ekp-test-parshape-bypasses-c` 单独加载 C 后失败，确认测试顺序依赖 |

GUI 数值矩阵的七种场景均满足 `widest == target` 且 `over=0`。干净截图的第二次抓取被 macOS 录屏权限提示覆盖；未批准系统权限，因此矩阵输出可作为数值证据，但本次不把截图当成无噪声视觉证据。

### 1.3 未验证项

- 未运行本地 `package-lint`；仓库 CI 会从 MELPA 动态安装它，但本机没有固定版本。
- 未运行 Windows 构建与 GUI 验证。
- 未验证远端 GitHub Actions 的运行结果；本地分支尚未推到 `origin/main`。
- C 极值整数风险未在 macOS sanitizer 下动态复现：`make DEBUG=1` 能构建，但当前系统因 sanitizer runtime code-signing policy 拒绝加载该模块。

## 2. 当前架构

```text
字符串 API
  ekp-pixel-justify / ekp-pixel-range-justify
    ├─ ekp-utils：分箱、字体与像素测量
    ├─ ekp-hyphen：Liang pattern 编译与断词
    ├─ ekp.el：ekp-para、前缀数组、缓存
    ├─ DP
    │   ├─ Elisp 1D / 2D
    │   └─ C 1D（15 字段边界，批量按段落并行）
    └─ renderer：glue、soft break、soft hyphen、hidden payload

buffer API
  ekp-region.el
    ├─ justify / unjustify
    ├─ auto mode：宽度跟随、dirty 段落、lazy chunk
    └─ 编辑器协议：save、isearch、kill/yank、undo、modified state
```

### 2.1 扎实的部分

1. **Elisp 是语义事实源，C 只做纯 DP。**

   证据：`ekp.el:1733-1749` 构造 15 字段输入；`ekp_c/ekp.c:590-611` 暴露对应 API；`ekp_c/README.md` 明确字体相关数据留在 Elisp。

   判断：这是正确边界。不要重新把分词、字体测量或渲染下沉到 C。

2. **严格遍 → 紧急遍的失败模型一致。**

   证据：`ekp.el:1173-1202` 与 `ekp_c/ekp_kp.c:501-537`；fuzz 300/300。

   判断：困难输入不丢内容的核心承诺有代码与性质测试双重保护。

3. **布局是可逆结构，不是破坏性格式化。**

   证据：`ekp.el:2180-2191` 定义四类布局标记；`ekp-region.el:402-445` 做结构反转；region 测试覆盖 roundtrip、save、isearch、kill、undo。

   判断：模型有价值，但所有编辑器协议都必须显式适配，生命周期边界必须更硬。

4. **CI 覆盖面优于一般小型 Elisp 包。**

   证据：`.github/workflows/ci.yml` 覆盖 Emacs 29.1/30.1/snapshot、C parity、fuzz、ASan/UBSan、macOS。

   判断：测试类型正确；主要缺口是可复现性、Windows 和失败退出契约。

## 3. 按优先级排序的问题

优先级含义：

- P0：数据丢失、安全或默认路径不可用，立即阻断发布
- P1：已实证的核心行为错误或测试可信度缺陷
- P2：重要边界风险、兼容性/维护性缺陷或已知能力落差
- P3：优化与体验机会，应由基准或真实需求驱动

本次未发现 P0。

### P1-01 算法参数不在 DP 缓存键中，修改后静默复用旧结果

- **Evidence**
  - `ekp--dp-key` 只包含 `line-pixel` 和非零 `ekp-looseness`：`ekp.el:1184-1190`。
  - demerits 直接读取 `ekp-line-penalty`、`ekp-hyphen-penalty`、`ekp-adjacent-fitness-penalty`、`ekp-consecutive-hyphen-penalty` 等全局值：`ekp.el:1080-1125`。
  - README 声称所有算法参数对两个引擎生效。
  - 实测同一段落/宽度先用 hyphen penalty 0，再改为 1000000：缓存对象仍相同；清缓存后断点改变，cost 从 `100226389.0` 变为 `601214639.0`。
- **Inference**
  - 问题 owner 是 DP 缓存键，不是 C 参数同步。C 同步只在真正计算时运行，命中缓存时根本不会进入同步或 DP。
- **Impact**
  - 用户通过 `setq` 或 Custom 调整排版质量参数时，界面可能看起来“设置无效”，直到显式 `ekp-clear-caches` 或段落键变化。
- **Direction**
  - 把所有影响 DP 结果的参数收敛到一个不可变的 DP key；不要再靠零散 watcher 猜测失效点。
  - 回归测试必须在不清缓存的情况下修改每一类参数，并断言结果或 cost 与 fresh computation 一致。
- **Confidence**：High

### P1-02 保存失败会留下未排版 buffer 和 stale save state

> 后续状态（2026-07-28）：已由 `task004` 修复。保存不再执行
> unjustify/rejustify 事务，而是在 `write-region` annotation 边界写入
> 隐藏逻辑副本；成功、文件系统失败、编码失败和 `quit` 均有回归覆盖。

- **Evidence**
  - `before-save-hook` 先把全部 justified spans 反转：`ekp-region.el:461-479`。
  - 只有 `after-save-hook` 会重排并清空 `ekp-region--save-state`：`ekp-region.el:481-492`。
  - 负路径实测：让 visited file 的目标目录消失后调用 `save-buffer`，得到 `still-justified=nil`、`save-state=t`。
- **Inference**
  - 成功路径测试不足以证明保存事务；底层写入报错或保存被中断时，`after-save-hook` 不会提供 finally 语义。
- **Impact**
  - README 的“保存逻辑文本，同时屏幕保持排版态”承诺在失败路径不成立；后续保存还可能携带陈旧 marker。
- **Direction**
  - 先定义保存事务的 owner 和失败恢复路径，再实现；恢复必须无条件执行，写盘失败仍要回到原显示态。
  - 增加写盘失败、编码失败、用户中断三类负路径测试。
- **Confidence**：High

### P1-03 覆盖已有 copy filter，破坏其他 mode/user 的复制语义

> 后续状态（2026-07-28）：已由 `task005` 修复。测试覆盖 local/global
> prior filter、复制、DELETE kill、手动 unjustify 与 mode 关闭。

- **Evidence**
  - 安装集成时直接 `setq-local filter-buffer-substring-function`：`ekp-region.el:378-389`。
  - 移除时只删除 EKP 自己的当前值，没有保存或恢复前一个 filter：`ekp-region.el:391-399`。
  - 负路径实测：预设 filter 为复制内容加 `PRE:`，justify 后该前缀消失。
- **Inference**
  - `filter-buffer-substring-function` 是单槽协议，EKP 必须显式组合或恢复前一个 owner；直接覆盖不是可组合集成。
- **Impact**
  - 在已有复制过滤逻辑的 major mode 中，kill/copy 可能丢失 mode 定义的语义。
- **Direction**
  - 记录前一个 buffer-local filter，明确调用顺序，并在最后一个 justified span 消失时恢复。
  - 测试已有 filter、EKP filter、删除式 kill 三条真实 public path。
- **Confidence**：High

### P1-04 测试顺序依赖制造假绿

> **Resolved by task006 (2026-07-28):** fixture 使用动态绑定恢复全部
> tunable；parshape 用例绑定真实 `ekp-parshape` 并驱动公开 formatter。
> 108/108 通过可复现乱序，全部 108 个测试也逐个在全新 Emacs 进程
> 中通过。

- **Evidence**
  - `ekp-test-parshape-bypasses-c` 的名称和 docstring 测 parshape，代码却绑定 `ekp-first-line-indent`：`tests/ekp-tests.el:225-228`。
  - C 1.5 已支持 first-line indent；真正绕过 C 的条件是 `ekp-parshape` 非 nil：`ekp.el:1667-1675`。
  - `ekp-test-params-affect-c-module` 在结束时把全局 `ekp-use-c-module` 留为 nil：`tests/ekp-tests.el:434-448`。
  - 全套 94/94；单独加载 C 后运行该测试则 1/1 FAIL。
- **Inference**
  - 默认字母序恰好隐藏了错误测试；当前 fixture 只恢复部分 penalty，没有恢复引擎和全部样式状态。
- **Impact**
  - 测试套件可能继续掩盖其他全局状态泄漏，绿灯可信度下降。
- **Direction**
  - 让 clean-state fixture 保存/恢复所有全局 tunable；该测试绑定真实 `ekp-parshape`。
  - CI 增加 isolated 或 randomized-order lane，至少覆盖依赖全局状态的测试。
- **Confidence**：High

### P1-05 同一字符串对象的属性修改绕过完整段落 key

> 后续状态（2026-07-28）：已由 `task016` 修复。most-recent slot
> 保存并比较完整 `ekp--para-key`，删除六个补偿性 style watcher；
> CJK 与 Latin-with-space 原地属性修改回归、专项缓存矩阵及完整
> default/permuted/isolated ERT 均通过。

- **Evidence**
  - `ekp--para-key` 包含过滤后的 text-property intervals。
  - `ekp--get-para` 的 most-recent fast path 只比较对象 identity、语言、
    width context 与 spacing signature。
  - 独立审查实测：warm `"文中"` 后在原对象加入 `ekp-no-break`，下一次
    lookup 仍返回原 paragraph；清缓存后的 fresh paragraph 不允许相同
    break。
- **Inference**
  - 这不是 hash collision，而是第二套不完整 identity 规则绕开了唯一
    完整 key。
- **Direction**
  - 让 fast path 与 hash cache 共享 `ekp--para-key`，删除并行维护的
    partial signature/watchers。
  - 用 CJK 与 Latin-with-space 两类原地 property mutation 锁定 public
    paragraph resolver。
- **Confidence**：High

### P2-01 自动 CJK stretch 默认值不在段落缓存键中

> 后续状态（2026-07-28）：已由 `task003` 修复。新增回归分别覆盖
> 段落哈希键、`ekp--last-para` 快路径及相同签名真实命中；修复没有
> 新增 watcher。

- **Evidence**
  - `ekp-default-cws-stretch-pixel` 定义于 `ekp.el:94-97`，在 auto 参数计算中使用：`ekp.el:329-336`。
  - auto 模式的 para key 只写入符号 `auto`：`ekp.el:740-746`。
  - 实测把默认值从 2 改为 9 后，返回同一个 para，缓存中的 `:cws-stretch` 仍为 2。
- **Inference**
  - 这是 P1-01 同一类模型缺陷：缓存键没有完整表达决定结果的输入。
- **Direction**
  - 将该值纳入 auto 参数 snapshot/key；测试修改后无需手动清缓存。
- **Confidence**：High

### P2-02 C API 的输入契约、错误语义与数值范围不闭合

> **Resolved by task008 (2026-07-28):** C 1.6 对 15 字段 shape、长度、
> 类型和 int32 范围做整体验证，非法输入使用专用 condition；DP 中间
> 行宽/前缀差/剩余空间改为 int64。六项边界红测 0/6 → 6/6，公开
> dispatcher error 传播 0/1 → 1/1，完整 ERT 116/116、fuzz 300/300。

- **Evidence**
  - 文档说 bad argument 返回 nil；直接传入长度错误的 vector 实测得到 `args-out-of-range`。
  - `ekp_c/ekp.c:176-237`、`323-374` 在多次 `vec_get`/`extract_integer` 后才统一检查 pending non-local exit。
  - 输入先 clamp 到 `int32_t`，随后在 `ekp_c/ekp_kp.c:287-304` 继续做有符号加减。
- **Inference**
  - 正常 `ekp.el` 调用会捕获 signal 并回落 Elisp，因此常规文本不受影响；但直接 C API 的文档契约不真实。
  - `INT32_MAX` 附近的 width/protrusion/prefix 组合可能触发 C signed overflow。真实字体不接近此范围，但公开 API 没有限制它。
- **Direction**
  - 先校验 15 字段 arity、`n/n+1` 数组长度、排序位置和数值范围，再分配/提取。
  - 统一选择“明确 signal”或“返回 nil”，不要文档与实现各说一套。
  - 内部度量改为 `int64_t` 或 checked/saturating arithmetic，API 边缘再收窄。
  - 增加 malformed vectors、极值、batch >1024 的 C 边界测试。
- **Confidence**：Medium-High

### P2-03 C 构建入口对路径、CPU 和调试环境过度假设

> **Resolved by task009 (2026-07-28):** `PROFILE=portable` 成为默认，
> native/debug/sanitize 独立；交互构建不经过 shell。两项红测
> 0/2 → 2/2，四 profile 均零警告，真实交互构建与含空格副本构建
> 均成功。

- **Evidence**
  - `ekp-c-module-build` 拼接 `cd %s && make` 交给 shell，路径没有 quote：`ekp-utils.el:405-419`。
  - 默认 Makefile 使用 `-march=native -flto`：`ekp_c/Makefile:33-36`。
  - README 把普通 `make` 描述为通用入口。
  - 当前 macOS 上 `make DEBUG=1` 构建成功，但加载时因 sanitizer runtime code-signing policy 失败。
- **Inference**
  - 安装路径含空格或 shell 元字符时，交互 build 会失败，特殊路径还形成命令注入面。
  - `-march=native` 适合本机 benchmark，不适合分发预编译产物。
- **Direction**
  - 用进程 `default-directory` + 参数向量调用 `make`，不要启动 shell 执行 `cd`。
  - portable 作为默认 profile；`NATIVE=1`、`LTO=1`、`DEBUG=1` 显式选择。
  - 文档分别说明 Linux sanitizer 与 macOS 调试模块的 runtime/code-signing 条件。
- **Confidence**：High

### P2-04 词典覆盖与 parser 能力被文档高估

> 后续状态（2026-07-28）：已由 `task011` 闭环。进一步核对发现除
> 2399 条三语言 replacement rules 外，Esperanto 还有 4 条斜杠模式。
> 当前固定宽度 DP 无法表达“仅断点胜出时替换文字与宽度”，因此四份
> 词典统一 signal 而非产生错误断词；49 项 manifest/upstream gate 与
> 双导出一致性均通过。

- **Evidence**
  - 仓库实际有 50 个 `hyph_*.dic`，不是 README 所写的 70+。
  - `ekp-hyphen--compile` 跳过所有含 `/` 的 alternative patterns：`ekp-hyphen.el:114-153`。
  - 仓库中 3 个词典含 2399 条非注释 alternative pattern，主要来自 Hungarian，也涉及 Catalan/Albanian。
  - 50 个词典中有 20 个没有同名 `README_hyph_*.txt`；README 却要求“见每个词典的 README”。
  - `dictionaries/update.sh` 拉取移动中的 LibreOffice HEAD，没有 pinned commit、manifest、checksum、错误退出或跨平台处理。
- **Inference**
  - “任意 bundled dictionary 都完整工作”不成立。普通 Liang patterns 可用，但 replacement/alternative 语义被有意忽略，相关语言的断词质量会下降。
  - 许可证不一定不合规，但仅凭当前仓库无法完整核对每个词典的来源版本与许可文件。
- **Direction**
  - 先诚实声明 parser 支持的语法子集。
  - 为 Hungarian/Catalan/Albanian 加 golden words，再决定是否值得实现 replacement semantics。
  - 更新脚本必须 pin upstream commit，生成来源/许可/checksum manifest，并在 macOS/Linux 都可运行。
- **Confidence**：High

### P2-05 GUI 验证工具有结果，但不是可靠的发布门禁

> **Resolved by task007 (2026-07-28):** 单场景返回结构化结果，统一
> report 在 batch 任一失败时退出 1。README 明确工具加载边界；强制
> 失败/成功负控 2/2，真实 GUI 矩阵 7/7，干净全屏单窗口截图已复核。

- **Evidence**
  - README 推荐 `M-x ekp-gui-verify`，命令实际位于 `tests/ekp-gui-verify.el`，普通 `(require 'ekp-region)` 不会定义它。
  - `ekp-gui-verify-matrix` 在 noninteractive 下只打印 `FAIL` 文本，不会以非零状态退出：`tests/ekp-gui-verify.el:184-191`。
  - 本次真实 GUI 矩阵 7/7 PASS。
- **Inference**
  - 工具对人工诊断有价值，但当前无法直接成为 CI gate；用户也可能按 README 执行一个尚未加载的命令。
- **Direction**
  - 二选一：把单次诊断纳入发布包，或明确给出加载 tests 工具的命令。
  - matrix 发现任何 FAIL 时必须 signal/exit 1；表格只是报告，不是判定。
- **Confidence**：High

### P2-06 手动 unjustify 后集成 hook/filter 残留

> 后续状态（2026-07-28）：已由 `task005` 修复。auto mode 外最后
> justified span 消失时，公共 unjustify 与外部删除路径都会卸载
> integrations；内部 reflow/isearch 继续使用不改变生命周期的核心反转。

- **Evidence**
  - `ekp-unjustify-region` 只反转文本属性：`ekp-region.el:402-445`。
  - integrations 只在 minor mode 关闭路径移除：`ekp-region.el:928-938`。
  - 实测全 buffer unjustify 后已经没有 justified span，但 save/isearch hooks 和 local copy filter 仍存在。
- **Inference**
  - 无 justified span 时多数 hook 会空跑，但 copy filter 仍占据协议槽；它与 P1-03 共享生命周期 owner。
- **Direction**
  - 由“buffer 是否仍有 justified span / auto mode 是否开启”派生 integration 生命周期，不要让手动命令和 mode 各自维护一套真相。
- **Confidence**：High

### P2-07 发布与文档治理没有闭环

> 后续状态（2026-07-28）：仓库内治理已由 `task010` 闭环。静态 gate
> 检查 action SHA、Windows/ERT 路径、`.phrase`、package/changelog
> 与 C ABI 版本；远端 CI、tag 和 content-addressed artifact 由发布
> checklist 在实际发版时验证，本轮没有冒充已发布。

- **Evidence**
  - 本地 `main` 比 `origin/main` ahead 31；本地 `v1.0.0` 指向 HEAD，远端没有该 tag。
  - `CHANGELOG.md` 与 package header 已声明 1.0.0。
  - `.phrase/` 被 `.gitignore` 忽略，但仓库协议又把它定义为需求/task/change/issue 的事实源。
  - 审计前 README 的 C 版本、词典数量、测试数量、GUI 工具入口均与实现不一致。
  - CI 使用 `purcell/setup-emacs@master`、运行时最新 MELPA `package-lint`、`macos-latest`，没有 Windows job。
- **Inference**
  - 如果 GitHub `origin` 是正式发布面，则 1.0.0 尚未真正发布；如果不是，仓库缺少明确的发布状态说明。
  - 被忽略的 `.phrase` 可以做本地过程数据，但不能同时作为可审计的版本化事实源。
- **Direction**
  - 在 push 前增加 release gate：commit/tag/changelog/version/CI/checksum 一致。
  - 明确 `.phrase` 是 tracked source of truth 还是 local workflow state，只能选一个。
  - pin CI action/工具版本，增加 Windows 构建 lane。
- **Confidence**：High

### P3-01 tokenizer 与断词缓存还有局部性能债

> **Resolved by task012 (2026-07-28):** nil cache 使用显式 miss
> sentinel；tokenizer 与密集插入改为片段单次拼接。8,000 字符耗时
> 分别从 3.133 s 降到 1.100 s、从 0.945 s 降到 0.013 s；完整 ERT
> 124/124、fuzz 300/300、warning-as-error 编译与 checkdoc 通过。

- **Evidence**
  - `ekp-split-to-boxes` 在字符循环中反复 `concat`：`ekp-utils.el:253-316`。
  - `ekp-hyphen-inserted` 每个断点重建字符串：`ekp-hyphen.el:216-224`。
  - `ekp-hyphen--positions` 用 `(or (gethash ...) (puthash ...))`；缓存值为 nil 时无法区分 miss。对无 pattern 的 `qzxq` 连续查询两次，`ekp-hyphen--compute` 实际调用两次。
- **Inference**
  - 极长 URL、长 token、重复 acronym 或大量零宽字符下会出现额外分配；正常段落不是当前最大瓶颈。
- **Direction**
  - 先把 benchmark 加入这些退化输入，再做线性 builder 与显式 cache sentinel。
- **Confidence**：High

### P3-02 核心函数与 `ekp-para` 承担了过多规则

> **Resolved by task014 (2026-07-28):** 既有 `ekp--dp-key` /
> `ekp--line-ideal-pixel` owner 保留；四处重复的行缘空格公式收敛到
> 一个内联纯规则，renderer/region 的五项 marker 词汇表收敛到一个
> 常量。直接规则测试与既有 roundtrip/C parity 共同锁定边界，未拆
> 文件、未新增热循环分配。

- **Evidence**
  - `ekp-para` 同时保存分箱、测宽、glue、禁则、悬挂、offset 和 DP cache：`ekp.el:215-246`。
  - `ekp--make-para`、1D/2D DP、renderer 都是长流程；space/protrusion 公式还必须在 DP、C 重建和 renderer 三处保持一致。
- **Inference**
  - 继续加 typography 选项时，最可能出现的错误不是算法本身，而是忘记更新 key、15 字段边界或渲染反变换。
- **Direction**
  - 不拆新文件。先在 `ekp.el` 内把“cache signature”“line metrics”“render marker protocol”变成三个可直接测试的规则 owner。
  - 只抽取能消除重复规则的纯函数；不要建立 helper ladder。
- **Confidence**：High

### P3-03 交互入口与临时标记的产品语义不够清楚

> **Resolved by task013 (2026-07-28):** 四个保护命令均通过真实
> interactive/public formatter 路径测试；minor mode 增加标准 EKP
> 菜单与 help，命令反馈及双语文档明确属性仅在当前 buffer session
> 有效。未引入持久化格式或全局快捷键。

- **Evidence**
  - minor mode keymap 只 remap `fill-paragraph`：`ekp-region.el:897-899`。
  - no-break/verbatim 命令只写入普通 text property：`ekp-region.el:568-595`；普通文件保存重开不会保留这些属性。
- **Inference**
  - 用户难以发现 justify/unjustify/protect 操作，也可能误以为手工保护会持久化。
- **Direction**
  - 先补 mode help、状态反馈和“仅当前 buffer session 有效”的文档；只有真实用户需要时再设计持久化语法映射。
- **Confidence**：Medium

## 4. 优化与扩展路线

### 4.1 第一优先级：恢复行为可信度

1. 完整 DP cache signature。
2. 保存失败的无条件恢复。
3. copy filter 组合与 integrations 单一生命周期。
4. 测试 fixture 隔离、错误 parshape 测试修正、负路径测试。

这四项完成前，不建议扩大 typography 功能面。

### 4.2 第二优先级：硬化 C 与发布边界

1. 15 字段 schema preflight、`int64_t`/checked arithmetic。
2. portable/native/debug 三种构建 profile。
3. 可诊断的 C fallback：用户路径仍回落，但 debug/CI 能看到失败原因。
4. GUI matrix 非零退出、Windows job、固定 CI 依赖。
5. 词典来源/许可/checksum manifest。

### 4.3 性能方向：只做有基准支撑的工作

| 方向 | 当前判断 | 启动条件 |
|---|---|---|
| tokenizer 线性 builder | 小而明确 | 长 token benchmark 显示明显非线性 |
| nil-aware hyphen cache | 低成本 | 可直接修复并加计数测试 |
| C paragraph-handle API | 潜在高收益、高复杂度 | range search profile 证明 15 数组重复 marshal 是主要瓶颈 |
| 更多线程内并行 | 不建议 | 单段 DP 有前向依赖，当前按段落并行边界正确 |
| 把测宽/分词移入 C | 不建议 | 会拆散字体事实源并放大 parity 成本 |

### 4.4 可选产品扩展

1. **精确 width search 模式**

   当前 ternary + local scan 明确不保证全局最优。可为小范围提供 exhaustive 选项，并保留当前快速默认。

2. **按段落选择断词语言**

   当前 `ekp-latin-lang` 是全局单值。若真实文档有多语言段落，可增加 buffer-local resolver；不要先做自动语言识别。

3. **可发布的诊断入口**

   把 `ekp-diagnose` 与 GUI fit check 收敛成一个用户入口：快速检查当前 buffer，需要完整矩阵时再加载 tests 工具。

4. **mode-native verbatim/no-break**

   Org/Markdown 优先从语法/face 派生保护，不把瞬时 text property 当持久存储。其他 mode 通过一个 buffer-local predicate 接入。

5. **发布包与词典可选化**

   词典占仓库约 6.1 MiB。若分发场景确有需要，可提供 core + language packs；没有安装/更新痛点前不要拆包。

### 4.5 明确不做

- 左缘悬挂与行中 glyph advance 压缩：当前 Emacs 显示模型不能可靠表达，已有文档说明。
- 把一个 coherent `ekp.el` 按“utils/common”标签拆成多个文件。
- 为了“以后也许需要”而新增协议层、adapter 或兼容 shim。
- 在没有 profile 数据前重写 C DP 或增加候选级并行。

## 5. 建议的验收门槛

### 核心行为

- 所有影响 DP 的参数修改后，不清缓存也与 fresh computation 一致。
- 保存成功、保存失败、用户中断后，buffer 的显示态、逻辑文本、modified state 和 marker state 都正确。
- 已有 copy filter 与 EKP filter 组合后，两方语义都保留。

### 测试

- 每个 ERT 可单独运行。
- randomized order 通过。
- 现有 94 ERT、300 fuzz、C parity 全通过。
- GUI matrix 任一 FAIL 返回非零。

### C 边界

- malformed vector 不越界、不继续带 pending exit 调用 API。
- 数值上限有文档、有检查、有 sanitizer 覆盖。
- portable build 不含 `-march=native`；native benchmark profile 单独启用。

### 发布

- package version、C ABI version、README、CHANGELOG、tag、远端 commit 对齐。
- CI action/tool 版本固定。
- 词典 manifest 能回答“来源 commit、文件 checksum、许可文件”。

## 6. Evidence / Inference / Unknown 总结

### Evidence

- 默认与 fresh-build 测试均 94/94；fuzz 300/300；GUI 数值矩阵 7/7。
- 四个 P1 问题均有代码路径与负控/独立复现。
- README/C ABI/test count/词典数量与仓库事实曾发生漂移。
- 本地 main/tag 与 origin 状态不一致。

### Inference

- C `int32_t` 极值会造成 signed overflow 风险，但常规字体输入不会接近该范围。
- alternative patterns 被忽略会降低相关语言质量；具体单词影响需要 golden corpus 定量。
- 交互可发现性不足与 text property 非持久化会造成用户困惑，尚无用户研究数据。

### Unknown

- Windows C 模块和 region UI 在当前版本是否完整通过。
- GitHub Actions 在尚未推送的 31 个本地提交上是否通过。
- 远端缺少 v1.0.0 是有意暂缓还是发布遗漏。
- 用户是否真正需要精确 width search、多语言 resolver 或 language packs。

## 7. 停止条件

本次任务在以下状态停止：

- 已覆盖代码、C、测试、GUI、文档、发布与扩展面；
- 已把问题按证据和优先级写入文档；
- 已给出优化方向与“不做什么”；
- 未修改运行代码，也未推送/发布；
- 具体修复应从 P1-01 开始，每次只闭环一个原子问题。
