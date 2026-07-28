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

## 阶段与提交计划(全部完成 2026-07-26)

- [x] A 地基 64eb2f3:标点成盒 + breaks-allowed + DP/C 1.2;顺带修复
      连续闭合标点行首漏洞、open-punct 跨空格盒序错乱、「Hello 断词失效;
      半角标点禁则(纯标点盒判定)
- [x] B ea96a6d:ekp-no-break 属性(刚性原子/禁断词)、NBSP/NNBSP/
      FIGURE SPACE/WJ/ZWNBSP、命令 ×2;零 C 改动
- [x] C 57a3abe:ekp-alignment 四模式 + ekp-ragged-stretch-pixel;
      C 1.3(set-penalties 第 7 参 extra-stretch,缺省归零)
- [x] D f6aa64b:ekp-protrusion 右缘悬挂(cjk-close/latin-close/hyphen
      比率);DP/渲染/C 重建三处 lw=width+release 同步;C 1.4
      (break-with-arrays 14 参);region 预留 protrusion-reserve;
      仅右缘(左缘无法渲染,文档已注明)
- [x] E 720b1cd:ekp-parshape + ekp-first-line-indent(t=2em 按段落
      CJK 字体);loose 2D 每行宽;C 旁路
- [x] F 4d9a018:ekp-verbatim 段落豁免 + ekp-region-skip-faces +
      buffer-local skip-predicate;行内原子沿用 ekp-no-break;核心零改动
- [x] G:readme×2 排版特性/verbatim 章节、DEVELOPER×2 §5.1;GUI 目检
      (悬挂+缩进+verbatim+auto-mode 齐行/ragged 两态截图确认)

最终状态:66 ERT 全绿,fuzz 300/300(每阶段跑),C 模块 1.4 两引擎
逐字节一致。行中挤压不可渲染(Emacs 无负宽 display)= 已知边界。

## 验证清单(每阶段)

byte-compile 零警告(error-on-warn)→ 47+ ERT → C 重建 + parity →
fuzz 300 → 提交。改 ekp_c/ 后必须 make clean && make。
Emacs: /Applications/Emacs.app/Contents/MacOS/Emacs
