# Emacs-KP: Knuth-Plass 排版算法 Emacs 实现

[English Documentation](./readme.md) | [开发者指南](./DEVELOPER_ZH.md)

Emacs-kp 实现了 Knuth-Plass 最优断行算法，并扩展支持 CJK（中日韩）与拉丁文混合排版。

## 演示


## 特性

- **全局最优断行**：使用 Knuth-Plass 算法寻找段落的全局最优布局。
- **CJK 支持**：完美支持中日韩与拉丁文的混合排版。
- **连字符断词**：使用 Frank Liang 算法和特定语言词典。
- **属性保留**：排版后保留字体、颜色等所有 Emacs 文本属性。
- **C 模块加速**：可选的多线程 C 模块提供 16-29 倍性能提升。
- **自动字体处理**：根据实际字体度量自动计算间距参数。

---

## 用户指南

### 快速开始

1. **安装依赖**：
   建议安装 C 编译器以构建高性能模块。

2. **配置与使用**：

```elisp
(add-to-list 'load-path "/path/to/emacs-kp")
(require 'ekp)

;; 基本用法：将文本按 600 像素宽度对齐
(ekp-pixel-justify "这是一段测试文本..." 600)

;; 范围对齐：寻找 400-800 像素范围内的最优宽度
(ekp-pixel-range-justify "测试文本" 400 800)
```

### 配置详情

#### 语言设置

**`ekp-latin-lang`** (默认: `"en_US"`)

用于断词的主要拉丁语言。支持的语言位于 `dictionaries/` 目录：
- `en_US`, `en_GB` - 英语
- `de_DE` - 德语
- `fr` - 法语
- `es` - 西班牙语
- 等等...

```elisp
(setq ekp-latin-lang "de_DE")
```

#### 间距参数

使用 `ekp-param-set` 配置间距（像素）。若不设置，将根据字体自动计算。

```elisp
(ekp-param-set lws-ideal lws-stretch lws-shrink
               mws-ideal mws-stretch mws-shrink
               cws-ideal cws-stretch cws-shrink)
```

| 参数组 | 说明 |
|:-------|:-----|
| `lws-*` | 拉丁词间距 (Latin Word Space) |
| `mws-*` | 中西文间距 (Mixed Word Space) |
| `cws-*` | CJK 字符间距 (CJK Word Space) |

#### K-P 算法参数

| 变量 | 默认值 | 说明 |
|:-----|:-------|:-----|
| `ekp-line-penalty` | 10 | 每行断行的基础惩罚 |
| `ekp-hyphen-penalty` | 50 | 连字符断词的惩罚 |
| `ekp-adjacent-fitness-penalty` | 100 | 相邻行松紧度不一致的惩罚 |
| `ekp-last-line-min-ratio` | 0.5 | 末行最小填充比例 |
| `ekp-looseness` | 0 | 目标行数偏移（±n 行） |

### C 动态模块 (推荐)

对于长文本，建议使用 C 模块以获得显著的性能提升。

#### 构建

```bash
cd ekp_c
make
```
*要求：C11 编译器，Emacs 27.1+*

#### 加载

```elisp
(require 'ekp-utils)

;; 加载并初始化 C 模块
(ekp-c-module-load)

;; 可选：为 C 模块加载断词字典
(ekp-c-load-dictionary "en_US")
```

加载后，`ekp-use-c-module` 默认为 `t`，所有排版函数将自动使用 C 模块进行加速。

---

## 算法与架构

关于内部架构、算法细节和 API 参考的详细说明，请参阅 **[开发者指南](./DEVELOPER_ZH.md)**。

## 致谢

- **核心算法**: ["Breaking Paragraphs into Lines"](https://gwern.net/doc/design/typography/tex/1981-knuth.pdf) by Donald E. Knuth and Michael F. Plass (1981)
- **断词算法**: 改编自 [Pyphen](https://github.com/Kozea/Pyphen)，使用 Liang 算法
- **词典**: [Hunspell 断词模式](https://github.com/Kozea/Pyphen)
