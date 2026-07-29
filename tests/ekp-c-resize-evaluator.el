;;; ekp-c-resize-evaluator.el --- C resize performance gate -*- lexical-binding: t; -*-

;;; Commentary:

;; One process measures one frozen-baseline or candidate round.  Compare mode
;; aggregates the interleaved JSONL records and enforces the task032 contract.

;;; Code:

(require 'cl-lib)
(require 'json)

(defvar ekp-use-c-module)
(defvar ekp-auto-justify-mode)
(defvar ekp-auto-justify-lazy-threshold)
(defvar ekp-buffer--auto-width)
(declare-function ekp-showcase--sample "ekp-showcase")
(declare-function ekp-buffer--paragraph-ranges "ekp-buffer")
(declare-function ekp-buffer--skip-paragraph-p "ekp-buffer")
(declare-function ekp-buffer--reflow "ekp-buffer")
(declare-function ekp--get-para "ekp")
(declare-function ekp-para-dp-cache "ekp")
(declare-function ekp-justify-region "ekp-buffer")
(declare-function ekp-clear-caches "ekp")
(declare-function ekp-c-module-load "ekp-utils")
(declare-function ekp--c-available-p "ekp")
(declare-function ekp-c-version "ext:ekp")

(defconst ekp-c-resize-evaluator--widths
  '(480 452 424 396 368 340 312 284 256 228 200 172))

(defconst ekp-c-resize-evaluator--warm-width 520)
(defconst ekp-c-resize-evaluator--target-ms 50.0)
(defconst ekp-c-resize-evaluator--minimum-improvement 20.0)

(defvar ekp-c-resize-evaluator--plan-ms 0.0)
(defvar ekp-c-resize-evaluator--module-ms 0.0)
(defvar ekp-c-resize-evaluator--install-ms 0.0)
(defvar ekp-c-resize-evaluator--clear-ms 0.0)

(defun ekp-c-resize-evaluator--fixtures ()
  "Return the fixed length matrix used by the evaluator."
  (let ((sample (ekp-showcase--sample)))
    `(("showcase" . ,sample)
      ("showcase-2x" . ,(concat sample "\n\n" sample)))))

(defun ekp-c-resize-evaluator--timed-call (counter function arguments)
  "Call FUNCTION with ARGUMENTS and add elapsed milliseconds to COUNTER."
  (let ((start (float-time)))
    (prog1 (apply function arguments)
      (set counter
           (+ (symbol-value counter)
              (* 1000.0 (- (float-time) start)))))))

(defun ekp-c-resize-evaluator--reset-profile ()
  "Reset per-reflow layer counters."
  (setq ekp-c-resize-evaluator--plan-ms 0.0
        ekp-c-resize-evaluator--module-ms 0.0
        ekp-c-resize-evaluator--install-ms 0.0
        ekp-c-resize-evaluator--clear-ms 0.0))

(defun ekp-c-resize-evaluator--layout-hash ()
  "Return a stable hash of the complete source and projection."
  (secure-hash
   'sha256
   (prin1-to-string (buffer-substring (point-min) (point-max)))))

(defun ekp-c-resize-evaluator--paragraphs ()
  "Return cached paragraph objects for the current logical buffer."
  (cl-loop for range in
           (ekp-buffer--paragraph-ranges (point-min) (point-max))
           for text = (buffer-substring (car range) (cdr range))
           unless (ekp-buffer--skip-paragraph-p text)
           collect (ekp--get-para text)))

(defun ekp-c-resize-evaluator--clear-dp (paragraphs)
  "Clear only width-dependent DP entries in PARAGRAPHS."
  (dolist (paragraph paragraphs)
    (clrhash (ekp-para-dp-cache paragraph))))

(defun ekp-c-resize-evaluator--profiled-call (function arguments)
  "Call FUNCTION with ARGUMENTS and return total plus layer timings."
  (let ((plan (symbol-function 'ekp-layout-plan))
        (module (symbol-function 'ekp-c-break-with-arrays))
        (install (symbol-function 'ekp-buffer--install-plan))
        (live-install (symbol-function 'ekp-buffer--install-live-prefix))
        (clear (symbol-function 'ekp-buffer--clear-projection))
        (start (float-time)))
    (ekp-c-resize-evaluator--reset-profile)
    (cl-letf (((symbol-function 'ekp-layout-plan)
               (lambda (&rest args)
                 (ekp-c-resize-evaluator--timed-call
                  'ekp-c-resize-evaluator--plan-ms plan args)))
              ((symbol-function 'ekp-c-break-with-arrays)
               (lambda (&rest args)
                 (ekp-c-resize-evaluator--timed-call
                  'ekp-c-resize-evaluator--module-ms module args)))
              ((symbol-function 'ekp-buffer--install-plan)
               (lambda (&rest args)
                 (ekp-c-resize-evaluator--timed-call
                  'ekp-c-resize-evaluator--install-ms install args)))
              ((symbol-function 'ekp-buffer--install-live-prefix)
               (lambda (&rest args)
                 (ekp-c-resize-evaluator--timed-call
                  'ekp-c-resize-evaluator--install-ms live-install args)))
              ((symbol-function 'ekp-buffer--clear-projection)
               (lambda (&rest args)
                 (ekp-c-resize-evaluator--timed-call
                  'ekp-c-resize-evaluator--clear-ms clear args))))
      (apply function arguments))
    `((total_ms . ,(* 1000.0 (- (float-time) start)))
      (plan_ms . ,ekp-c-resize-evaluator--plan-ms)
      (module_ms . ,ekp-c-resize-evaluator--module-ms)
      (install_ms . ,ekp-c-resize-evaluator--install-ms)
      (clear_ms . ,ekp-c-resize-evaluator--clear-ms))))

(defun ekp-c-resize-evaluator--measure-core-fixture (fixture)
  "Measure every uncached core width for FIXTURE."
  (with-temp-buffer
    (insert (cdr fixture))
    (let ((paragraphs (ekp-c-resize-evaluator--paragraphs))
          samples)
      (ekp-justify-region
       (point-min) (point-max) ekp-c-resize-evaluator--warm-width)
      (dolist (width ekp-c-resize-evaluator--widths)
        (ekp-c-resize-evaluator--clear-dp paragraphs)
        (let ((before gcs-done)
              (sample
               (ekp-c-resize-evaluator--profiled-call
                (symbol-function 'ekp-justify-region)
                (list (point-min) (point-max) width))))
          (unless (= before gcs-done)
            (error "GC occurred inside resize sample"))
          (push (append `((path . "core")
                          (fixture . ,(car fixture)) (width . ,width))
                        sample)
                samples)))
      (nreverse samples))))

(defun ekp-c-resize-evaluator--measure-resize-fixture (fixture)
  "Measure every uncached full resize width for FIXTURE."
  (with-temp-buffer
    (insert (cdr fixture))
    (let ((paragraphs (ekp-c-resize-evaluator--paragraphs))
          (ekp-auto-justify-mode t)
          (ekp-auto-justify-lazy-threshold most-positive-fixnum)
          samples)
      (ekp-buffer--reflow
       (current-buffer) ekp-c-resize-evaluator--warm-width)
      (dolist (width ekp-c-resize-evaluator--widths)
        (ekp-c-resize-evaluator--clear-dp paragraphs)
        (let ((before gcs-done)
              (sample
               (ekp-c-resize-evaluator--profiled-call
                (symbol-function 'ekp-buffer--reflow)
                (list (current-buffer) width))))
          (unless (= before gcs-done)
            (error "GC occurred inside resize sample"))
          (push (append `((path . "resize")
                          (fixture . ,(car fixture)) (width . ,width))
                        sample)
                samples)))
      (nreverse samples))))

(defun ekp-c-resize-evaluator--layout-core-fixture (fixture)
  "Return core projection hashes for every width of FIXTURE."
  (with-temp-buffer
    (insert (cdr fixture))
    (cl-loop for width in ekp-c-resize-evaluator--widths
             do (ekp-justify-region (point-min) (point-max) width)
             collect `((path . "core") (fixture . ,(car fixture))
                       (width . ,width)
                       (hash . ,(ekp-c-resize-evaluator--layout-hash))))))

(defun ekp-c-resize-evaluator--layout-resize-fixture (fixture)
  "Return full resize projection hashes for every width of FIXTURE."
  (with-temp-buffer
    (insert (cdr fixture))
    (let ((ekp-auto-justify-mode t)
          (ekp-auto-justify-lazy-threshold most-positive-fixnum))
      (cl-loop for width in ekp-c-resize-evaluator--widths
               do (ekp-buffer--reflow (current-buffer) width)
               collect `((path . "resize") (fixture . ,(car fixture))
                         (width . ,width)
                         (hash . ,(ekp-c-resize-evaluator--layout-hash)))))))

(defun ekp-c-resize-evaluator--layout-matrix (use-c)
  "Return the fixed layout matrix with USE-C selecting the engine."
  (let ((ekp-use-c-module use-c))
    (ekp-clear-caches)
    (let ((core
           (cl-mapcan #'ekp-c-resize-evaluator--layout-core-fixture
                      (ekp-c-resize-evaluator--fixtures))))
      (ekp-clear-caches)
      (append
       core
       (cl-mapcan #'ekp-c-resize-evaluator--layout-resize-fixture
                  (ekp-c-resize-evaluator--fixtures))))))

(defun ekp-c-resize-evaluator--measure-round ()
  "Measure one C round and return a JSON-compatible record."
  (require 'ekp)
  (require 'ekp-buffer)
  (require 'ekp-showcase)
  (ekp-c-module-load)
  (unless (ekp--c-available-p)
    (error "C module did not load"))
  (let ((gc-cons-threshold most-positive-fixnum)
        (label (or (getenv "EKP_RESIZE_LABEL") "unknown"))
        (round (string-to-number (or (getenv "EKP_RESIZE_ROUND") "0"))))
    (garbage-collect)
    (ekp-clear-caches)
    (let* ((core
            (cl-mapcan #'ekp-c-resize-evaluator--measure-core-fixture
                       (ekp-c-resize-evaluator--fixtures)))
           (_ (ekp-clear-caches))
           (resize
            (cl-mapcan #'ekp-c-resize-evaluator--measure-resize-fixture
                       (ekp-c-resize-evaluator--fixtures)))
           (samples (append core resize))
           (c-layouts (ekp-c-resize-evaluator--layout-matrix t))
           (elisp-layouts (ekp-c-resize-evaluator--layout-matrix nil)))
      `((label . ,label) (round . ,round)
        (module_version . ,(ekp-c-version))
        (c_elisp_parity . ,(equal c-layouts elisp-layouts))
        (layouts . ,c-layouts) (samples . ,samples)))))

(defun ekp-c-resize-evaluator--write-record (record)
  "Append JSON RECORD to `EKP_RESIZE_OUTPUT'."
  (let ((path (getenv "EKP_RESIZE_OUTPUT")))
    (unless path (error "EKP_RESIZE_OUTPUT is required"))
    (write-region (concat (json-encode record) "\n") nil path t 'silent)))

(defun ekp-c-resize-evaluator--read-jsonl (path)
  "Read JSON objects from PATH."
  (with-temp-buffer
    (insert-file-contents path)
    (mapcar (lambda (line)
              (json-parse-string line :object-type 'alist
                                 :array-type 'list))
            (split-string (buffer-string) "\n" t))))

(defun ekp-c-resize-evaluator--sample-values (records path key)
  "Return numeric KEY values from PATH samples in RECORDS."
  (cl-loop for record in records
           append (cl-loop for sample in (alist-get 'samples record)
                           when (equal (alist-get 'path sample) path)
                           collect (alist-get key sample))))

(defun ekp-c-resize-evaluator--percentile (values percentile)
  "Return nearest-rank PERCENTILE from VALUES."
  (let* ((sorted (sort (copy-sequence values) #'<))
         (rank (max 0 (1- (ceiling (* percentile (length sorted)))))))
    (nth rank sorted)))

(defun ekp-c-resize-evaluator--improvement (baseline candidate)
  "Return percentage improvement from BASELINE to CANDIDATE."
  (* 100.0 (/ (- baseline candidate) baseline)))

(defun ekp-c-resize-evaluator--all-parity-p (records)
  "Return non-nil when every RECORD matches its Elisp engine."
  (cl-every (lambda (record) (eq (alist-get 'c_elisp_parity record) t))
            records))

(defun ekp-c-resize-evaluator--cross-parity-p (baseline candidate)
  "Return non-nil when BASELINE and CANDIDATE layouts match exactly."
  (and (equal (alist-get 'layouts (car baseline))
              (alist-get 'layouts (car candidate)))
       (cl-every
        (lambda (record)
          (equal (alist-get 'layouts record)
                 (alist-get 'layouts (car baseline))))
        (append baseline candidate))))

(defun ekp-c-resize-evaluator--path-comparison (baseline candidate path)
  "Return metrics for PATH in BASELINE and CANDIDATE records."
  (let* ((base (ekp-c-resize-evaluator--sample-values
                baseline path 'total_ms))
         (cand (ekp-c-resize-evaluator--sample-values
                candidate path 'total_ms))
         (base-p50 (ekp-c-resize-evaluator--percentile base 0.50))
         (base-p95 (ekp-c-resize-evaluator--percentile base 0.95))
         (cand-p50 (ekp-c-resize-evaluator--percentile cand 0.50))
         (cand-p95 (ekp-c-resize-evaluator--percentile cand 0.95))
         (p50-gain (ekp-c-resize-evaluator--improvement base-p50 cand-p50))
         (p95-gain (ekp-c-resize-evaluator--improvement base-p95 cand-p95)))
    `((baseline_p50_ms . ,base-p50) (baseline_p95_ms . ,base-p95)
      (candidate_p50_ms . ,cand-p50) (candidate_p95_ms . ,cand-p95)
      (p50_improvement_pct . ,p50-gain)
      (p95_improvement_pct . ,p95-gain)
      (candidate_target_pass
       . ,(<= cand-p95 ekp-c-resize-evaluator--target-ms))
      (improvement_pass
       . ,(and (>= p50-gain ekp-c-resize-evaluator--minimum-improvement)
               (>= p95-gain ekp-c-resize-evaluator--minimum-improvement))))))

(defun ekp-c-resize-evaluator--comparison (baseline candidate)
  "Return core and resize metrics for BASELINE and CANDIDATE records."
  `((core . ,(ekp-c-resize-evaluator--path-comparison
              baseline candidate "core"))
    (resize . ,(ekp-c-resize-evaluator--path-comparison
                baseline candidate "resize"))))

(defun ekp-c-resize-evaluator--path-pass-p (metrics)
  "Return non-nil when one path's METRICS pass both performance gates."
  (and (eq (alist-get 'candidate_target_pass metrics) t)
       (eq (alist-get 'improvement_pass metrics) t)))

(defun ekp-c-resize-evaluator--report (baseline candidate)
  "Return the final evaluator report for BASELINE and CANDIDATE."
  (let* ((metrics (ekp-c-resize-evaluator--comparison baseline candidate))
         (parity (and (ekp-c-resize-evaluator--all-parity-p baseline)
                      (ekp-c-resize-evaluator--all-parity-p candidate)
                      (ekp-c-resize-evaluator--cross-parity-p
                       baseline candidate)))
         (pass (and parity
                    (ekp-c-resize-evaluator--path-pass-p
                     (alist-get 'core metrics))
                    (ekp-c-resize-evaluator--path-pass-p
                     (alist-get 'resize metrics)))))
    `((pass . ,pass) (layout_parity . ,parity)
      (core . ,(alist-get 'core metrics))
      (resize . ,(alist-get 'resize metrics)))))

(defun ekp-c-resize-evaluator--compare ()
  "Compare raw JSONL paths from the environment and enforce the contract."
  (let* ((baseline (ekp-c-resize-evaluator--read-jsonl
                    (getenv "EKP_RESIZE_BASELINE_JSONL")))
         (candidate (ekp-c-resize-evaluator--read-jsonl
                     (getenv "EKP_RESIZE_CANDIDATE_JSONL")))
         (report (ekp-c-resize-evaluator--report baseline candidate))
         (path (getenv "EKP_RESIZE_REPORT")))
    (when path
      (write-region (concat (json-encode report) "\n") nil path nil 'silent))
    (princ (concat (json-encode report) "\n"))
    (unless (eq (alist-get 'pass report) t)
      (kill-emacs 1))))

(if (equal (getenv "EKP_RESIZE_MODE") "compare")
    (ekp-c-resize-evaluator--compare)
  (ekp-c-resize-evaluator--write-record
   (ekp-c-resize-evaluator--measure-round)))

;;; ekp-c-resize-evaluator.el ends here
