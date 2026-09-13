# Emacs-KP: Knuth-Plass 排版算法 Emacs 实现

源码布局：将仓库根目录加入 `load-path`，然后 require 与包同名的入口。
入口 Commentary 列出公共 API 及用法；内部实现位于 `lisp/`。

[English Documentation](README.md) | [开发者指南](docs/architecture.zh-CN.md)

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

克隆仓库并将 `lisp/` 加入 `load-path`；`dictionaries/` 与 `native/` 保留在仓库根目录：

```elisp
(add-to-list 'load-path "/path/to/emacs-kp")
(require 'ekp)
(require 'ekp)   ; buffer/region 命令
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
cd native && make PROFILE=portable # 默认;产出 ekp.dylib/.so/.dll
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

[完整用法与接口契约](docs/manual.zh-CN.md)。

## 开发

克隆后运行 `make setup-hooks`。提交变更前运行 `make check`；`make structure-check` 用于快速检查目录、命名和引用。统一开发规则见 [AGENTS.md](AGENTS.md)。

[docs/architecture.zh-CN.md](docs/architecture.zh-CN.md) · [CHANGELOG.md](CHANGELOG.md)

`make check` 运行规范检查、编译及 [tests/acceptance.json](tests/acceptance.json) 中的公共验收场景。`make test` 运行更全面的回归测试；GUI 与性能验收使用独立入口。验收清单依据对外契约选择，不按当前能否通过筛选。
