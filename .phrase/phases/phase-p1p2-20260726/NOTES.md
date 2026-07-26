# P1+P2 功能阶段笔记(2026-07-26 起)

> 用户指令:高质量完成 P1(标点挤压、ragged 模式、no-break API)+ P2(悬挂、
> parshape/首行缩进、连续标点);代码块等特殊文本需正确处理;**通用机制优先,
> 万不得已才做场景特化**。

## 总体架构决策

1. **地基 = 逐间隙断行许可(breaks-allowed)+ 标点独立成盒**。
   禁则从"吞噬式附着"迁移为 DP 层的断点禁止;所有后续特性(NBSP、
   no-break 区间、行内 verbatim 原子、标点类别)都是这套机制的实例。
2. **Emacs 显示引擎约束(已确证)**:无法缩减字形 advance(无负宽
   display)→ CLREQ 行中标点挤压不可渲染;行首/行尾挤压视觉上等价于
   "悬挂"(protrusion)→ P1-1 + P2-4 + P2-6 统一为**边缘突出机制**,
   按字符类配比率(可扩展到拉丁连字符突出 = microtype)。
3. ragged-right/left/center:DP 侧 = 刚性 glue(stretch/shrink 数组置零)
   + 每行额外伸展量 R(badness 以 R 为 flexibility);渲染侧分派剩余量
   (右/左/对半)。C 只需 +1 标量。
4. parshape/首行缩进:每行宽依赖行号 → 复用 looseness 的 2D DP,
   elisp-only(C 自动旁路,同 looseness 先例)。
5. verbatim:段落级豁免(region 层谓词/属性)+ 行内原子
   (ekp-no-break 属性 → 禁断点 + 刚性 glue + 禁断词)。

## 关键实现事实(读码结论)

- tokenizer 附着逻辑在 ekp-utils.el `ekp--handle-cjk-char/latin-char`
  (开放标点 hold-and-prepend;闭合标点 append-to-prev)。
  **已知老 bug**:连续闭合标点(字。」)第二个独立成盒且断点未禁止 →
  」可出现行首;开放标点跨空格 hold 还会导致盒序与原文顺序不一致。
  迁移后两者都根治。
- `ekp--str-type` 返回 space/latin/cjk/cjk-punct → 拆成 cjk-open
  (opening-punct-p:general-category Ps/Pi)/ cjk-close(fw-punct-p
  且非 open)。“” 特例保持 'cjk。
- `ekp--glue-type` 新矩阵(保持旧拓扑等价):space→nws;
  before=open→nws(原盒内);after=close→nws(原盒内;close-close
  从 cws 改为 nws,属有意修正);latin-latin→lws;cjk-cjk→cws;
  cjk/latin 混→mws;其余含标点→cws。
- breaks-allowed 规则:`allowed[k] = !(tail(box[k-1])=open || head(box[k])=close)`,
  k∈[1,n-1];k=n(段末)恒可。存 bool-vector(elisp DP 用)+
  forbidden-positions int 向量(C 打包用,稀疏,仿 hyphen-positions)。
- DP 改动(elisp `ekp--dp-run-1d` + C `dp_process_position` 镜像):
  候选 k 需 allowed;不 allowed 时**不 throw**继续延伸;
  紧急兜底从 single-box 推广为 atomic-run(i 到 k 间无允许断点);
  多盒紧急行需记录 gaps 计数(渲染 normal 路径 clamp ≥0 自然溢出)。
- C 桥:`ekp-c-break-with-arrays` 11→12 参(forbidden-positions),
  batch 向量同步;`ekp-c-set-penalties` 后续 ragged 加 extra-stretch
  标量;protrusion 再加两数组(head/tail protrude px)。每次 API 变
  动 bump EKP_VERSION_MINOR + `ekp-c-module-required-version`。
- 隐性收益:「Hello / Hello」 之前整盒无法匹配断词正则(左右标点类
  不含 CJK 引号)→ 拆盒后可正常断词。
- 测试影响:tests/ekp-tests.el 里 split-* 结构测试要改为新盒契约;
  新增行为级禁则测试(任意宽度:行首无 close、行尾无 open、
  字。」不拆)。fuzz 断言与引擎无关,应保持 0 失败。

## 阶段与提交计划

- [ ] A 地基:标点成盒 + breaks-allowed + DP/C(1.2)+ 测试迁移
- [ ] B no-break API:ekp-no-break 属性、NBSP/WJ/2060/202F、刚性 glue
- [ ] C 对齐模式:ekp-alignment(justify|ragged-right|ragged-left|center)
      + C extra-stretch(1.3)
- [ ] D 突出/悬挂:ekp-protrusion(类→左右比率)+ C 两数组(1.4)
      + region 层宽度补偿
- [ ] E parshape:ekp-parshape + ekp-first-line-indent(2D,elisp-only)
- [ ] F verbatim:段落豁免(region 谓词/属性)+ 行内原子(含禁断词)
- [ ] G 文档(readme×2 DEVELOPER×2)+ GUI 目检 + 记忆更新

## 验证清单(每阶段)

byte-compile 零警告(error-on-warn)→ 47+ ERT → C 重建 + parity →
fuzz 300 → 提交。改 ekp_c/ 后必须 make clean && make。
Emacs: /Applications/Emacs.app/Contents/MacOS/Emacs
