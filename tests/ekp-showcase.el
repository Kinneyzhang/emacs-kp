;;; ekp-showcase.el --- Interactive feature & performance showcase -*- lexical-binding: t; -*-

;;; Commentary:

;; One self-contained interactive demo for everything ekp does.
;;
;;   emacs -Q -L /path/to/emacs-kp -l tests/ekp-showcase.el -f ekp-showcase
;;
;; or, with the package on `load-path':  M-x ekp-showcase
;;
;; Keys (also shown by `?'):
;;   -/+ or ←/→   width −20 / +20 px (Shift variants: ±4 px)
;;   d            animated width sweep, reports ms/frame and fps
;;   a            cycle alignment: justify → ragged-right → center → ragged-left
;;   p            toggle hanging punctuation (protrusion)
;;   i            toggle first-line indent (2 em)
;;   s            toggle a wedge parshape (Elisp 2D path — watch the ms)
;;   c            toggle C engine (compare per-reflow time live)
;;   w            follow window width (real ekp-auto-justify-mode; resize away)
;;   g            re-justify now    r  reset all    q  quit

;;; Code:

(require 'ekp)
(require 'ekp-region)

(defvar ekp-showcase-min-width 140)
(defvar ekp-showcase-max-width 1200)

(defvar-local ekp-showcase--width 480)
(defvar-local ekp-showcase--last-ms nil)
(defvar-local ekp-showcase--parshape-on nil)

(defconst ekp-showcase--alignments
  '(justify ragged-right center ragged-left))

(defun ekp-showcase--sample ()
  "Build the demo text: prose, punctuation, atoms, NBSP, a code block."
  (let ((zh1 "汉字排版的质感藏在细节里:开启标点悬挂之后,行尾的句号。逗号,和问号?都会把墨迹之外的空白悬出齐边,右边缘因此在视觉上更加平直。避头尾规则保证「引号」与《书名号》永远紧贴内容,连续闭合标点如此。」也绝不拆行。")
        (mixed "中英混排是 Emacs 里最常见的场景:The quick brown fox jumps over the lazy dog, 而 internationalization 这样的长词在窄栏会按 Liang 模式断词,连字符同样参与悬挂。")
        (atoms (concat "行内原子演示:代码片段 "
                       (propertize "(ekp-pixel-justify STR W)"
                                   'ekp-no-break t
                                   'face '(:inherit fixed-pitch
                                           :background "#3a3f4b"
                                           :foreground "#98c379"))
                       " 永不拆散、空格保持字面宽度;不间断空格让 100"
                       (string #x00A0)
                       "000 与 3.14"
                       (string #x00A0)
                       "MB 这类数字单位锁在同一行。"))
        (code (propertize
               "(defun ekp-demo (text width)   ; ekp-verbatim: 代码块整段豁免\n  (insert (ekp-pixel-justify text width)))"
               'ekp-verbatim t
               'face '(:inherit fixed-pitch :foreground "#61afef")))
        (zh2 "把窗口拉宽或压窄(按 w 进入跟随窗口模式),或者按住 - 与 + 连续改变像素宽度,头行会实时显示这一次重排消耗的毫秒数:段落缓存与渲染缓存命中时,数字会明显变小。")
        (en "Purely Latin paragraphs justify with natural word spacing, hyphenation, and hanging periods. Try width sweeps with the d key and compare the C engine against pure Elisp with c."))
    (string-join (list zh1 mixed atoms code zh2 en) "\n\n")))

(defun ekp-showcase--keys-line ()
  "The always-visible key cheat sheet (header line)."
  (let ((key (lambda (k) (propertize k 'face 'help-key-binding))))
    (concat " " (funcall key "-") "/" (funcall key "+") " 宽度±20  "
            (funcall key "←") (funcall key "→") " ±4  "
            (funcall key "d") " 扫掠  "
            (funcall key "a") " 对齐  "
            (funcall key "p") " 悬挂  "
            (funcall key "i") " 缩进  "
            (funcall key "s") " parshape  "
            (funcall key "c") " 引擎  "
            (funcall key "w") " 跟随窗口  "
            (funcall key "r") " 重置  "
            (funcall key "q") " 退出")))

(defun ekp-showcase--state-line ()
  "The live state readout (mode line)."
  (format " EKP  宽 %dpx │ 对齐 %s │ 悬挂 %s │ 缩进 %s │ parshape %s │ 引擎 %s%s │ 重排 %s"
          ekp-showcase--width
          ekp-alignment
          (if ekp-protrusion "on" "off")
          (if ekp-first-line-indent "2em" "off")
          (if ekp-showcase--parshape-on "wedge" "off")
          (if (and ekp-use-c-module (ekp--c-available-p)) "C" "elisp")
          (if ekp-auto-justify-mode " [跟随窗口]" "")
          (if ekp-showcase--last-ms
              (format "%.1f ms" ekp-showcase--last-ms)
            "—")))

(defun ekp-showcase--apply-parshape ()
  "Recompute the wedge parshape for the current width."
  (setq-local ekp-parshape
              (when ekp-showcase--parshape-on
                (let ((w ekp-showcase--width))
                  (list (cons 0 w)
                        (cons 40 (max 120 (- w 80)))
                        (cons 80 (max 120 (- w 160)))
                        (cons 40 (max 120 (- w 80)))
                        (cons 0 w))))))

(defun ekp-showcase--refresh ()
  "Re-justify the buffer at the current width; record the time."
  (ekp-showcase--apply-parshape)
  (let ((inhibit-read-only t)
        (t0 (float-time)))
    (ekp-justify-region (point-min) (point-max) ekp-showcase--width)
    (setq ekp-showcase--last-ms (* 1000 (- (float-time) t0))))
  (setq mode-line-format (ekp-showcase--state-line))
  (force-mode-line-update))

(defun ekp-showcase-set-width (w)
  "Set demo width to W and re-justify."
  (interactive "n宽度(px): ")
  (if ekp-auto-justify-mode
      (message "跟随窗口模式下宽度由窗口决定;按 w 退出该模式")
    (setq ekp-showcase--width
          (max ekp-showcase-min-width (min ekp-showcase-max-width w)))
    (ekp-showcase--refresh)))

(defun ekp-showcase-wider ()      (interactive) (ekp-showcase-set-width (+ ekp-showcase--width 20)))
(defun ekp-showcase-narrower ()   (interactive) (ekp-showcase-set-width (- ekp-showcase--width 20)))
(defun ekp-showcase-wider-1 ()    (interactive) (ekp-showcase-set-width (+ ekp-showcase--width 4)))
(defun ekp-showcase-narrower-1 () (interactive) (ekp-showcase-set-width (- ekp-showcase--width 4)))

(defun ekp-showcase-sweep ()
  "Animate the width down and back up, then report ms/frame."
  (interactive)
  (when ekp-auto-justify-mode (ekp-auto-justify-mode -1))
  (let* ((start ekp-showcase--width)
         (low (max ekp-showcase-min-width (- start 240)))
         (widths (append (number-sequence start low -8)
                         (number-sequence low start 8)))
         (times nil))
    (dolist (w widths)
      (setq ekp-showcase--width w)
      (let ((t0 (float-time)))
        (ekp-showcase--refresh)
        (push (* 1000 (- (float-time) t0)) times))
      (sit-for 0.001))
    (let* ((n (length times))
           (avg (/ (apply #'+ times) n)))
      (message "扫掠 %d 帧:平均 %.1f ms/帧(≈%d fps 能力),最慢 %.1f ms — 再按一次 d 体验缓存命中后的速度"
               n avg (round (/ 1000.0 avg)) (apply #'max times)))))

(defun ekp-showcase-cycle-alignment ()
  (interactive)
  (setq-local ekp-alignment
              (or (cadr (memq ekp-alignment ekp-showcase--alignments))
                  (car ekp-showcase--alignments)))
  (ekp-showcase--refresh)
  (message "对齐:%s" ekp-alignment))

(defun ekp-showcase-toggle-protrusion ()
  (interactive)
  (setq-local ekp-protrusion (not ekp-protrusion))
  (ekp-showcase--refresh)
  (message "标点悬挂:%s" (if ekp-protrusion "开(看行尾标点探出齐边)" "关")))

(defun ekp-showcase-toggle-indent ()
  (interactive)
  (setq-local ekp-first-line-indent (if ekp-first-line-indent nil t))
  (ekp-showcase--refresh)
  (message "首行缩进:%s" (if ekp-first-line-indent "2em" "关")))

(defun ekp-showcase-toggle-parshape ()
  (interactive)
  (setq ekp-showcase--parshape-on (not ekp-showcase--parshape-on))
  (ekp-showcase--refresh)
  (message "parshape:%s" (if ekp-showcase--parshape-on
                             "楔形(Elisp 2D 路径,注意 ms 变化)" "关")))

(defun ekp-showcase-toggle-engine ()
  (interactive)
  (setq-local ekp-use-c-module (not ekp-use-c-module))
  (ekp-clear-caches)   ; force real recompute so the comparison is honest
  (ekp-showcase--refresh)
  (message "引擎:%s" (if (and ekp-use-c-module (ekp--c-available-p))
                         "C(并行)" "纯 Elisp")))

(defun ekp-showcase-toggle-follow ()
  (interactive)
  (if ekp-auto-justify-mode
      (progn (ekp-auto-justify-mode -1)
             (ekp-showcase--refresh)
             (message "退出跟随窗口模式"))
    (ekp-auto-justify-mode 1)
    (setq mode-line-format (ekp-showcase--state-line))
    (force-mode-line-update)
    (message "跟随窗口模式:拖动改变窗口/边框宽度试试(防抖 %.2fs)"
             ekp-auto-justify-resize-delay)))

(defun ekp-showcase-reset ()
  (interactive)
  (when ekp-auto-justify-mode (ekp-auto-justify-mode -1))
  (setq-local ekp-alignment 'justify)
  (setq-local ekp-protrusion nil)
  (setq-local ekp-first-line-indent nil)
  (setq ekp-showcase--parshape-on nil)
  (setq ekp-showcase--width 480)
  (ekp-showcase--refresh)
  (message "已重置"))

(defun ekp-showcase-help ()
  (interactive)
  (message (concat "-/+ 宽度±20  ←/→ ±4  d 扫掠动画  a 对齐  p 悬挂  "
                   "i 缩进  s parshape  c 引擎  w 跟随窗口  g 刷新  r 重置  q 退出")))

(defvar ekp-showcase-mode-map
  (let ((map (make-sparse-keymap)))
    (suppress-keymap map)
    (define-key map (kbd "-") #'ekp-showcase-narrower)
    (define-key map (kbd "+") #'ekp-showcase-wider)
    (define-key map (kbd "=") #'ekp-showcase-wider)
    (define-key map (kbd "<left>")  #'ekp-showcase-narrower-1)
    (define-key map (kbd "<right>") #'ekp-showcase-wider-1)
    (define-key map (kbd "d") #'ekp-showcase-sweep)
    (define-key map (kbd "a") #'ekp-showcase-cycle-alignment)
    (define-key map (kbd "p") #'ekp-showcase-toggle-protrusion)
    (define-key map (kbd "i") #'ekp-showcase-toggle-indent)
    (define-key map (kbd "s") #'ekp-showcase-toggle-parshape)
    (define-key map (kbd "c") #'ekp-showcase-toggle-engine)
    (define-key map (kbd "w") #'ekp-showcase-toggle-follow)
    (define-key map (kbd "g") #'ekp-showcase--refresh)
    (define-key map (kbd "W") #'ekp-showcase-set-width)
    (define-key map (kbd "r") #'ekp-showcase-reset)
    (define-key map (kbd "?") #'ekp-showcase-help)
    (define-key map (kbd "q") #'quit-window)
    map))

(define-derived-mode ekp-showcase-mode special-mode "EKP-Showcase"
  "Interactive showcase for ekp typesetting features."
  (setq-local truncate-lines t)
  (setq-local cursor-type 'bar)
  ;; keys stay pinned in the header line; live state lives in the
  ;; mode line (both always visible)
  (setq header-line-format (ekp-showcase--keys-line)))

;;;###autoload
(defun ekp-showcase ()
  "Open the interactive ekp feature & performance showcase."
  (interactive)
  (ignore-errors (ekp-c-module-load))
  (let ((buf (get-buffer-create "*ekp-showcase*")))
    (with-current-buffer buf
      ;; The width sweep re-justifies the whole buffer dozens of
      ;; times; recording that in undo history is pure garbage.
      (buffer-disable-undo)
      (let ((inhibit-read-only t))
        (erase-buffer)
        (ekp-showcase-mode)
        (when (and (display-graphic-p)
                   (find-font (font-spec :family "Cascadia Next SC")))
          (face-remap-add-relative 'default :family "Cascadia Next SC"))
        (insert (ekp-showcase--sample))
        (goto-char (point-min))
        (ekp-showcase--refresh)))
    (pop-to-buffer buf)
    (delete-other-windows)
    (ekp-showcase-help)))

(provide 'ekp-showcase)

;;; ekp-showcase.el ends here
