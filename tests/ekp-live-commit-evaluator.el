;;; ekp-live-commit-evaluator.el --- Structural live-commit gate -*- lexical-binding: t; -*-

;;; Commentary:

;; One process measures one frozen-baseline or candidate round.  Compare mode
;; aggregates interleaved JSONL records and enforces the task030 contract.

;;; Code:

(require 'cl-lib)
(require 'json)

(defvar ekp-use-c-module)
(defvar ekp-auto-justify-native-append)
(defvar ekp-buffer--conflicts)
(defvar ekp-auto-justify-paragraph-limit)
(declare-function ekp-auto-justify-mode "ekp-buffer")
(declare-function ekp-buffer--logical-substring "ekp-buffer")
(declare-function ekp-buffer--window-pixel "ekp-buffer")
(declare-function ekp-c-module-load "ekp-utils")
(declare-function ekp-c-version "ext:ekp")
(declare-function ekp-clear-caches "ekp")
(declare-function ekp--c-available-p "ekp")
(declare-function ekp--measured-width "ekp")

(defconst ekp-live-commit-evaluator--widths '(64 80 96 128 160))
(defconst ekp-live-commit-evaluator--rows '(2 4 8 16))
(defconst ekp-live-commit-evaluator--target-ms 16.0)
(defconst ekp-live-commit-evaluator--minimum-improvement 20.0)
(defconst ekp-live-commit-evaluator--ordinary-target-ms 1.0)
(defconst ekp-live-commit-evaluator--maximum-regression 15.0)
(defconst ekp-live-commit-evaluator--default-gc-threshold
  gc-cons-threshold)
(defconst ekp-live-commit-evaluator--source-files
  '("ekp-utils.el" "ekp-hyphen.el" "ekp.el" "ekp-buffer.el")
  "Production source files loaded explicitly for every measured root.")
(defconst ekp-live-commit-evaluator--corpus
  (concat
   " extraordinary editing continues smoothly 中文拉丁混排"
   " while stable semantic rows remain exact and responsive"))

(defvar ekp-live-commit-evaluator--publish-calls 0)
(defvar ekp-live-commit-evaluator--row-crossings 0)
(defvar ekp-live-commit-evaluator--plan-calls 0)
(defvar ekp-live-commit-evaluator--module-calls 0)
(defvar ekp-live-commit-evaluator--append-calls 0)
(defvar ekp-live-commit-evaluator--append-hits 0)
(defvar ekp-live-commit-evaluator--cache-gets 0)
(defvar ekp-live-commit-evaluator--cache-hits 0)
(defvar ekp-live-commit-evaluator--plan-ms 0.0)
(defvar ekp-live-commit-evaluator--para-ms 0.0)
(defvar ekp-live-commit-evaluator--dp-ms 0.0)
(defvar ekp-live-commit-evaluator--append-ms 0.0)
(defvar ekp-live-commit-evaluator--append-dp-ms 0.0)
(defvar ekp-live-commit-evaluator--append-para-ms 0.0)
(defvar ekp-live-commit-evaluator--append-plan-ms 0.0)
(defvar ekp-live-commit-evaluator--module-ms 0.0)
(defvar ekp-live-commit-evaluator--install-ms 0.0)
(defvar ekp-live-commit-evaluator--clear-ms 0.0)
(defvar ekp-live-commit-evaluator--transaction-ms 0.0)
(defvar ekp-live-commit-evaluator--signature-ms 0.0)
(defvar ekp-live-commit-evaluator--cache-ms 0.0)

(defun ekp-live-commit-evaluator--load-source-root ()
  "Load every production file from `EKP_LIVE_COMMIT_CODE_ROOT'."
  (let ((root (getenv "EKP_LIVE_COMMIT_CODE_ROOT")))
    (unless root
      (error "EKP_LIVE_COMMIT_CODE_ROOT is required for measurement"))
    (dolist (file ekp-live-commit-evaluator--source-files)
      (load-file (expand-file-name file root)))))

(defun ekp-live-commit-evaluator--env-number (name fallback)
  "Return numeric environment variable NAME, or FALLBACK."
  (if-let ((value (getenv name)))
      (string-to-number value)
    fallback))

(defun ekp-live-commit-evaluator--env-numbers (name fallback)
  "Return comma-separated numeric environment variable NAME, or FALLBACK."
  (if-let ((value (getenv name)))
      (mapcar #'string-to-number (split-string value "," t "[ \t]+"))
    fallback))

(defun ekp-live-commit-evaluator--env-strings (name fallback)
  "Return comma-separated string environment variable NAME, or FALLBACK."
  (if-let ((value (getenv name)))
      (split-string value "," t "[ \t]+")
    fallback))

(defun ekp-live-commit-evaluator--reset-profile ()
  "Reset counters for one public command."
  (setq ekp-live-commit-evaluator--publish-calls 0
        ekp-live-commit-evaluator--row-crossings 0
        ekp-live-commit-evaluator--plan-calls 0
        ekp-live-commit-evaluator--module-calls 0
        ekp-live-commit-evaluator--append-calls 0
        ekp-live-commit-evaluator--append-hits 0
        ekp-live-commit-evaluator--cache-gets 0
        ekp-live-commit-evaluator--cache-hits 0
        ekp-live-commit-evaluator--plan-ms 0.0
        ekp-live-commit-evaluator--para-ms 0.0
        ekp-live-commit-evaluator--dp-ms 0.0
        ekp-live-commit-evaluator--append-ms 0.0
        ekp-live-commit-evaluator--append-dp-ms 0.0
        ekp-live-commit-evaluator--append-para-ms 0.0
        ekp-live-commit-evaluator--append-plan-ms 0.0
        ekp-live-commit-evaluator--module-ms 0.0
        ekp-live-commit-evaluator--install-ms 0.0
        ekp-live-commit-evaluator--clear-ms 0.0
        ekp-live-commit-evaluator--transaction-ms 0.0
        ekp-live-commit-evaluator--signature-ms 0.0
        ekp-live-commit-evaluator--cache-ms 0.0))

(defun ekp-live-commit-evaluator--timed-call (counter function arguments)
  "Call FUNCTION with ARGUMENTS and add elapsed milliseconds to COUNTER."
  (let ((started (float-time)))
    (prog1 (apply function arguments)
      (set counter
           (+ (symbol-value counter)
              (* 1000.0 (- (float-time) started)))))))

(defun ekp-live-commit-evaluator--counted-wrapper (counter function)
  "Return a wrapper incrementing COUNTER before calling FUNCTION."
  (lambda (&rest arguments)
    (set counter (1+ (symbol-value counter)))
    (apply function arguments)))

(defun ekp-live-commit-evaluator--timed-wrapper
    (time-counter call-counter function)
  "Return a timed FUNCTION wrapper using TIME-COUNTER and CALL-COUNTER."
  (lambda (&rest arguments)
    (when call-counter
      (set call-counter (1+ (symbol-value call-counter))))
    (ekp-live-commit-evaluator--timed-call
     time-counter function arguments)))

(defun ekp-live-commit-evaluator--cache-wrapper (function)
  "Return a live-cache FUNCTION wrapper that records hits."
  (lambda (key)
    (cl-incf ekp-live-commit-evaluator--cache-gets)
    (let ((started (float-time)))
      (prog1
          (let ((plan (funcall function key)))
            (when plan
              (cl-incf ekp-live-commit-evaluator--cache-hits))
            plan)
        (cl-incf ekp-live-commit-evaluator--cache-ms
                 (* 1000.0 (- (float-time) started)))))))

(defun ekp-live-commit-evaluator--row-crossed-wrapper (function)
  "Return a FUNCTION wrapper that counts true row crossings."
  (lambda ()
    (let ((crossed (funcall function)))
      (when crossed
        (cl-incf ekp-live-commit-evaluator--row-crossings))
      crossed)))

(defun ekp-live-commit-evaluator--append-wrapper (function)
  "Return a FUNCTION wrapper that times and records append-plan hits."
  (lambda (&rest arguments)
    (cl-incf ekp-live-commit-evaluator--append-calls)
    (let ((started (float-time)))
      (prog1
          (let ((plan (apply function arguments)))
            (when plan
              (cl-incf ekp-live-commit-evaluator--append-hits))
            plan)
        (cl-incf ekp-live-commit-evaluator--append-ms
                 (* 1000.0 (- (float-time) started)))))))

(defun ekp-live-commit-evaluator--instrument (function)
  "Call FUNCTION with live-commit layer instrumentation installed."
  (let ((publish (symbol-function 'ekp-buffer--publish-live-prefix))
        (plan (symbol-function 'ekp-layout-plan))
        (append-plan (symbol-function 'ekp-layout-plan-append))
        (para (symbol-function 'ekp--get-para))
        (dp (symbol-function 'ekp--dp-cache-para))
        (append-dp (symbol-function 'ekp--dp-cache-append))
        (append-para (symbol-function 'ekp--append-para))
        (append-layout (symbol-function 'ekp--layout-plan-from-para))
        (module (symbol-function 'ekp-c-break-with-arrays))
        (install (symbol-function 'ekp-buffer--install-live-prefix))
        (suffix (symbol-function 'ekp-buffer--clear-live-suffix))
        (projection (symbol-function 'ekp-buffer--clear-live-projection))
        (transaction (symbol-function 'ekp-buffer--start-live-edit))
        (signatures
         (symbol-function 'ekp-buffer--live-prefix-signatures))
        (row-crossed (symbol-function 'ekp-buffer--live-row-crossed-p))
        (cache (symbol-function 'ekp-buffer--live-cache-get)))
    (cl-letf
        (((symbol-function 'ekp-buffer--publish-live-prefix)
          (ekp-live-commit-evaluator--counted-wrapper
           'ekp-live-commit-evaluator--publish-calls publish))
         ((symbol-function 'ekp-layout-plan)
          (ekp-live-commit-evaluator--timed-wrapper
           'ekp-live-commit-evaluator--plan-ms
           'ekp-live-commit-evaluator--plan-calls plan))
         ((symbol-function 'ekp-layout-plan-append)
          (ekp-live-commit-evaluator--append-wrapper append-plan))
         ((symbol-function 'ekp--get-para)
          (ekp-live-commit-evaluator--timed-wrapper
           'ekp-live-commit-evaluator--para-ms nil para))
         ((symbol-function 'ekp--dp-cache-para)
          (ekp-live-commit-evaluator--timed-wrapper
           'ekp-live-commit-evaluator--dp-ms nil dp))
         ((symbol-function 'ekp--dp-cache-append)
          (ekp-live-commit-evaluator--timed-wrapper
           'ekp-live-commit-evaluator--append-dp-ms nil append-dp))
         ((symbol-function 'ekp--append-para)
          (ekp-live-commit-evaluator--timed-wrapper
           'ekp-live-commit-evaluator--append-para-ms nil append-para))
         ((symbol-function 'ekp--layout-plan-from-para)
          (ekp-live-commit-evaluator--timed-wrapper
           'ekp-live-commit-evaluator--append-plan-ms nil append-layout))
         ((symbol-function 'ekp-c-break-with-arrays)
          (ekp-live-commit-evaluator--timed-wrapper
           'ekp-live-commit-evaluator--module-ms
           'ekp-live-commit-evaluator--module-calls module))
         ((symbol-function 'ekp-buffer--install-live-prefix)
          (ekp-live-commit-evaluator--timed-wrapper
           'ekp-live-commit-evaluator--install-ms nil install))
         ((symbol-function 'ekp-buffer--clear-live-suffix)
          (ekp-live-commit-evaluator--timed-wrapper
           'ekp-live-commit-evaluator--clear-ms nil suffix))
         ((symbol-function 'ekp-buffer--clear-live-projection)
          (ekp-live-commit-evaluator--timed-wrapper
           'ekp-live-commit-evaluator--clear-ms nil projection))
         ((symbol-function 'ekp-buffer--start-live-edit)
          (ekp-live-commit-evaluator--timed-wrapper
           'ekp-live-commit-evaluator--transaction-ms nil transaction))
         ((symbol-function 'ekp-buffer--live-prefix-signatures)
          (ekp-live-commit-evaluator--timed-wrapper
           'ekp-live-commit-evaluator--signature-ms nil signatures))
         ((symbol-function 'ekp-buffer--live-row-crossed-p)
          (ekp-live-commit-evaluator--row-crossed-wrapper row-crossed))
         ((symbol-function 'ekp-buffer--live-cache-get)
          (ekp-live-commit-evaluator--cache-wrapper cache)))
      (funcall function))))

(defun ekp-live-commit-evaluator--self-insert (character)
  "Insert CHARACTER through the public command path."
  (let ((last-command-event character)
        (this-command #'self-insert-command))
    (call-interactively #'self-insert-command)
    (run-hooks 'post-command-hook)))

(defun ekp-live-commit-evaluator--event-kind ()
  "Return the current command's measured event kind."
  (if (> ekp-live-commit-evaluator--row-crossings 0)
      "structural-commit"
    "ordinary-key"))

(defun ekp-live-commit-evaluator--sample (character metadata)
  "Measure one CHARACTER insertion and attach scenario METADATA."
  (ekp-live-commit-evaluator--reset-profile)
  (let ((gcs-before gcs-done)
        (gc-before gc-elapsed)
        (started (float-time)))
    (ekp-live-commit-evaluator--self-insert character)
    (let ((total-ms (* 1000.0 (- (float-time) started)))
          (gc-events (- gcs-done gcs-before))
          (gc-ms (* 1000.0 (- gc-elapsed gc-before))))
      (append
       metadata
       `((event_kind . ,(ekp-live-commit-evaluator--event-kind))
         (char . ,(char-to-string character))
         (total_ms . ,total-ms)
         (plan_ms . ,ekp-live-commit-evaluator--plan-ms)
         (para_ms . ,ekp-live-commit-evaluator--para-ms)
         (dp_ms . ,ekp-live-commit-evaluator--dp-ms)
         (append_ms . ,ekp-live-commit-evaluator--append-ms)
         (append_dp_ms . ,ekp-live-commit-evaluator--append-dp-ms)
         (append_para_ms . ,ekp-live-commit-evaluator--append-para-ms)
         (append_plan_ms . ,ekp-live-commit-evaluator--append-plan-ms)
         (module_ms . ,ekp-live-commit-evaluator--module-ms)
         (publication_ms
          . ,(+ ekp-live-commit-evaluator--install-ms
                ekp-live-commit-evaluator--clear-ms))
         (publish_calls . ,ekp-live-commit-evaluator--publish-calls)
         (row_crossings . ,ekp-live-commit-evaluator--row-crossings)
         (plan_calls . ,ekp-live-commit-evaluator--plan-calls)
         (module_calls . ,ekp-live-commit-evaluator--module-calls)
         (append_calls . ,ekp-live-commit-evaluator--append-calls)
         (append_hits . ,ekp-live-commit-evaluator--append-hits)
         (cache_hits . ,ekp-live-commit-evaluator--cache-hits)
         (cache_misses
          . ,(- ekp-live-commit-evaluator--cache-gets
                ekp-live-commit-evaluator--cache-hits))
         (cache_ms . ,ekp-live-commit-evaluator--cache-ms)
         (transaction_ms . ,ekp-live-commit-evaluator--transaction-ms)
         (signature_ms . ,ekp-live-commit-evaluator--signature-ms)
         (gc_events . ,gc-events)
         (gc_ms . ,gc-ms))))))

(defun ekp-live-commit-evaluator--prefix-length (text target)
  "Return shortest prefix length of TEXT measuring at least TARGET pixels."
  (let ((low 1)
        (high (length text)))
    (while (< low high)
      (let ((middle (/ (+ low high) 2)))
        (if (>= (ekp--measured-width (substring text 0 middle)) target)
            (setq high middle)
          (setq low (1+ middle)))))
    low))

(defun ekp-live-commit-evaluator--fixture (width rows)
  "Return mixed prose spanning approximately ROWS lines at WIDTH."
  (let* ((unit ekp-live-commit-evaluator--corpus)
         (target (* width rows))
         (unit-width (max 1 (ekp--measured-width unit)))
         (copies (max 1 (ceiling (/ (float target) unit-width))))
         (text (apply #'concat (make-list (1+ copies) unit))))
    (substring text 0
               (ekp-live-commit-evaluator--prefix-length text target))))

(defun ekp-live-commit-evaluator--projection-hash ()
  "Return a stable hash of the current source plus display projection."
  (secure-hash
   'sha256
   (prin1-to-string (buffer-substring (point-min) (point-max)))))

(defun ekp-live-commit-evaluator--source-hash ()
  "Return a stable hash of the current logical source."
  (secure-hash
   'sha256
   (buffer-substring-no-properties (point-min) (point-max))))

(defun ekp-live-commit-evaluator--metadata (engine gc-mode width rows)
  "Return sample metadata for ENGINE, GC-MODE, WIDTH, and ROWS."
  `((engine . ,engine) (gc_mode . ,gc-mode)
    (width . ,width) (rows . ,rows)
    (live_append_backend
     . ,(if (bound-and-true-p ekp-auto-justify-native-append)
            "native-c"
          engine))))

(defun ekp-live-commit-evaluator--collect (metadata)
  "Collect a fixed structural-commit sample count for METADATA."
  (let* ((width (alist-get 'width metadata))
         (target (ekp-live-commit-evaluator--env-number
                  "EKP_LIVE_COMMIT_SAMPLES"
                  (if (= width 80) 8 2)))
         (ordinary-cap (ekp-live-commit-evaluator--env-number
                        "EKP_LIVE_ORDINARY_SAMPLES"
                        (if (= width 80) 32 8)))
        (stream (string-to-list ekp-live-commit-evaluator--corpus))
        (index 0)
        (commits 0)
        samples)
    (while (and (< commits target) (< index 4000))
      (let ((sample
             (ekp-live-commit-evaluator--sample
              (nth (% index (length stream)) stream) metadata)))
        (if (equal (alist-get 'event_kind sample) "structural-commit")
            (progn (cl-incf commits) (push sample samples))
          (when (< (- (length samples) commits) ordinary-cap)
            (push sample samples))))
      (cl-incf index))
    (unless (= commits target)
      (error "Only %d structural commits after %d keys" commits index))
    (nreverse samples)))

(defun ekp-live-commit-evaluator--run-cell (engine gc-mode width rows)
  "Measure one ENGINE, GC-MODE, WIDTH, and ROWS matrix cell."
  (with-temp-buffer
    (text-mode)
    (insert (ekp-live-commit-evaluator--fixture width rows))
    (goto-char (point-max))
    (cl-letf (((symbol-function 'ekp-buffer--window-pixel)
               (lambda (&optional _window) width)))
      (let ((ekp-auto-justify-paragraph-limit most-positive-fixnum))
        (ekp-auto-justify-mode 1)
        (ekp-live-commit-evaluator--self-insert ?x)
        (let* ((metadata
                (ekp-live-commit-evaluator--metadata
                 engine gc-mode width rows))
               (samples
                (ekp-live-commit-evaluator--instrument
                 (lambda ()
                   (ekp-live-commit-evaluator--collect metadata)))))
          `((engine . ,engine) (gc_mode . ,gc-mode)
            (width . ,width) (rows . ,rows)
            (source_hash . ,(ekp-live-commit-evaluator--source-hash))
            (projection_hash
             . ,(ekp-live-commit-evaluator--projection-hash))
            (conflicts . ,(length ekp-buffer--conflicts))
            (samples . ,samples)))))))

(defun ekp-live-commit-evaluator--run-cell-with-gc
    (engine gc-mode width rows)
  "Run an ENGINE cell under GC-MODE at WIDTH and ROWS."
  (let ((gc-cons-threshold
         (if (equal gc-mode "gc-excluded")
             most-positive-fixnum
           ekp-live-commit-evaluator--default-gc-threshold)))
    (garbage-collect)
    (ekp-live-commit-evaluator--run-cell engine gc-mode width rows)))

(defun ekp-live-commit-evaluator--measure-engine (engine)
  "Measure every matrix cell for ENGINE."
  (let ((ekp-use-c-module (equal engine "c"))
        scenarios)
    (dolist (gc-mode
             (ekp-live-commit-evaluator--env-strings
              "EKP_LIVE_COMMIT_GC_MODES"
              '("gc-excluded" "default-gc")))
      (dolist (width
               (ekp-live-commit-evaluator--env-numbers
                "EKP_LIVE_COMMIT_WIDTHS"
                ekp-live-commit-evaluator--widths))
        (dolist (rows
                 (ekp-live-commit-evaluator--env-numbers
                  "EKP_LIVE_COMMIT_ROWS"
                  ekp-live-commit-evaluator--rows))
          (ekp-clear-caches)
          (push
           (ekp-live-commit-evaluator--run-cell-with-gc
            engine gc-mode width rows)
           scenarios))))
    (nreverse scenarios)))

(defun ekp-live-commit-evaluator--measure-round ()
  "Measure one evaluator round and return a JSON-compatible record."
  (require 'ekp)
  (require 'ekp-buffer)
  (ekp-c-module-load)
  (unless (ekp--c-available-p)
    (error "C module did not load"))
  `((label . ,(or (getenv "EKP_LIVE_COMMIT_LABEL") "unknown"))
    (round . ,(ekp-live-commit-evaluator--env-number
               "EKP_LIVE_COMMIT_ROUND" 0))
    (module_version . ,(ekp-c-version))
    (scenarios
     . ,(cl-mapcan
         #'ekp-live-commit-evaluator--measure-engine
         (ekp-live-commit-evaluator--env-strings
          "EKP_LIVE_COMMIT_ENGINES" '("c" "elisp"))))))

(defun ekp-live-commit-evaluator--write-record (record)
  "Append JSON RECORD to `EKP_LIVE_COMMIT_OUTPUT'."
  (let ((path (getenv "EKP_LIVE_COMMIT_OUTPUT")))
    (unless path
      (error "EKP_LIVE_COMMIT_OUTPUT is required"))
    (write-region (concat (json-encode record) "\n") nil path t 'silent)))

(defun ekp-live-commit-evaluator--read-jsonl (path)
  "Read JSON objects from PATH."
  (with-temp-buffer
    (insert-file-contents path)
    (mapcar
     (lambda (line)
       (json-parse-string line :object-type 'alist :array-type 'list))
     (split-string (buffer-string) "\n" t))))

(defun ekp-live-commit-evaluator--samples (records)
  "Return all flat samples from RECORDS."
  (cl-loop for record in records
           append
           (cl-loop for scenario in (alist-get 'scenarios record)
                    append (alist-get 'samples scenario))))

(defun ekp-live-commit-evaluator--matching-samples
    (records event engine gc-mode &optional width)
  "Return RECORDS samples matching EVENT, ENGINE, GC-MODE, and WIDTH."
  (cl-remove-if-not
   (lambda (sample)
     (and (equal (alist-get 'event_kind sample) event)
          (equal (alist-get 'engine sample) engine)
          (equal (alist-get 'gc_mode sample) gc-mode)
          (or (null width) (= (alist-get 'width sample) width))))
   (ekp-live-commit-evaluator--samples records)))

(defun ekp-live-commit-evaluator--percentile (values percentile)
  "Return nearest-rank PERCENTILE from VALUES."
  (let* ((sorted (sort (copy-sequence values) #'<))
         (rank (max 0 (1- (ceiling (* percentile (length sorted)))))))
    (unless sorted
      (error "No samples for percentile"))
    (nth rank sorted)))

(defun ekp-live-commit-evaluator--statistics (samples key)
  "Return percentile statistics for numeric KEY in SAMPLES."
  (let ((values (mapcar (lambda (sample) (alist-get key sample)) samples)))
    `((p50 . ,(ekp-live-commit-evaluator--percentile values 0.50))
      (p95 . ,(ekp-live-commit-evaluator--percentile values 0.95))
      (p99 . ,(ekp-live-commit-evaluator--percentile values 0.99)))))

(defun ekp-live-commit-evaluator--improvement (baseline candidate)
  "Return percentage improvement from BASELINE to CANDIDATE."
  (if (zerop baseline)
      0.0
    (* 100.0 (/ (- baseline candidate) baseline))))

(defun ekp-live-commit-evaluator--comparison (baseline candidate)
  "Return timing and layer comparison for BASELINE and CANDIDATE samples."
  (let ((base-total
         (ekp-live-commit-evaluator--statistics baseline 'total_ms))
        (cand-total
         (ekp-live-commit-evaluator--statistics candidate 'total_ms)))
    `((baseline . ((total . ,base-total)
                   (plan . ,(ekp-live-commit-evaluator--statistics
                             baseline 'plan_ms))
                   (paragraph . ,(ekp-live-commit-evaluator--statistics
                                  baseline 'para_ms))
                   (dp . ,(ekp-live-commit-evaluator--statistics
                           baseline 'dp_ms))
                   (append . ,(ekp-live-commit-evaluator--statistics
                               baseline 'append_ms))
                   (append_dp . ,(ekp-live-commit-evaluator--statistics
                                  baseline 'append_dp_ms))
                   (append_para . ,(ekp-live-commit-evaluator--statistics
                                    baseline 'append_para_ms))
                   (append_plan . ,(ekp-live-commit-evaluator--statistics
                                    baseline 'append_plan_ms))
                   (module . ,(ekp-live-commit-evaluator--statistics
                               baseline 'module_ms))
                   (publication
                    . ,(ekp-live-commit-evaluator--statistics
                        baseline 'publication_ms))
                   (signature . ,(ekp-live-commit-evaluator--statistics
                                  baseline 'signature_ms))
                   (transaction . ,(ekp-live-commit-evaluator--statistics
                                    baseline 'transaction_ms))))
      (candidate . ((total . ,cand-total)
                    (plan . ,(ekp-live-commit-evaluator--statistics
                              candidate 'plan_ms))
                    (paragraph . ,(ekp-live-commit-evaluator--statistics
                                   candidate 'para_ms))
                    (dp . ,(ekp-live-commit-evaluator--statistics
                            candidate 'dp_ms))
                    (append . ,(ekp-live-commit-evaluator--statistics
                                candidate 'append_ms))
                    (append_dp . ,(ekp-live-commit-evaluator--statistics
                                   candidate 'append_dp_ms))
                    (append_para . ,(ekp-live-commit-evaluator--statistics
                                     candidate 'append_para_ms))
                    (append_plan . ,(ekp-live-commit-evaluator--statistics
                                     candidate 'append_plan_ms))
                    (module . ,(ekp-live-commit-evaluator--statistics
                                candidate 'module_ms))
                    (publication
                     . ,(ekp-live-commit-evaluator--statistics
                         candidate 'publication_ms))
                    (signature . ,(ekp-live-commit-evaluator--statistics
                                   candidate 'signature_ms))
                    (transaction . ,(ekp-live-commit-evaluator--statistics
                                     candidate 'transaction_ms))))
      (p95_improvement_pct
       . ,(ekp-live-commit-evaluator--improvement
           (alist-get 'p95 base-total) (alist-get 'p95 cand-total)))
      (p99_improvement_pct
       . ,(ekp-live-commit-evaluator--improvement
           (alist-get 'p99 base-total) (alist-get 'p99 cand-total))))))

(defun ekp-live-commit-evaluator--scenario-key (scenario)
  "Return the comparison key for SCENARIO."
  (mapcar (lambda (key) (alist-get key scenario))
          '(engine gc_mode width rows)))

(defun ekp-live-commit-evaluator--scenario-layout (scenario)
  "Return SCENARIO's source and projection identity."
  (list (alist-get 'source_hash scenario)
        (alist-get 'projection_hash scenario)
        (alist-get 'conflicts scenario)))

(defun ekp-live-commit-evaluator--layout-table (records)
  "Return a sorted layout identity table for RECORDS."
  (sort
   (cl-loop for record in records
            append
            (cl-loop for scenario in (alist-get 'scenarios record)
                     collect
                     (cons (ekp-live-commit-evaluator--scenario-key scenario)
                           (ekp-live-commit-evaluator--scenario-layout
                            scenario))))
   (lambda (left right)
     (string< (prin1-to-string left) (prin1-to-string right)))))

(defun ekp-live-commit-evaluator--engine-layouts (records engine)
  "Return ENGINE layout identities with engine removed from RECORDS."
  (sort
   (cl-loop for record in records
            append
            (cl-loop for scenario in (alist-get 'scenarios record)
                     when (equal (alist-get 'engine scenario) engine)
                     collect
                     (list
                      (cdr (ekp-live-commit-evaluator--scenario-key scenario))
                      (ekp-live-commit-evaluator--scenario-layout scenario))))
   (lambda (left right)
     (string< (prin1-to-string left) (prin1-to-string right)))))

(defun ekp-live-commit-evaluator--layout-parity-p (baseline candidate)
  "Return non-nil when all BASELINE and CANDIDATE layouts match."
  (and (equal (ekp-live-commit-evaluator--layout-table baseline)
              (ekp-live-commit-evaluator--layout-table candidate))
       (equal (ekp-live-commit-evaluator--engine-layouts baseline "c")
              (ekp-live-commit-evaluator--engine-layouts baseline "elisp"))
       (equal (ekp-live-commit-evaluator--engine-layouts candidate "c")
              (ekp-live-commit-evaluator--engine-layouts
               candidate "elisp"))))

(defun ekp-live-commit-evaluator--zero-work-p (records)
  "Return non-nil when every ordinary key in RECORDS does zero layout work."
  (cl-every
   (lambda (sample)
     (and (= (alist-get 'publish_calls sample) 0)
          (= (alist-get 'plan_calls sample) 0)
          (= (alist-get 'module_calls sample) 0)))
   (cl-remove-if-not
    (lambda (sample)
      (equal (alist-get 'event_kind sample) "ordinary-key"))
    (ekp-live-commit-evaluator--samples records))))

(defun ekp-live-commit-evaluator--valid-gc-excluded-p (records)
  "Return non-nil when excluded-GC samples in RECORDS contain no GC."
  (cl-every
   (lambda (sample)
     (or (not (equal (alist-get 'gc_mode sample) "gc-excluded"))
         (= (alist-get 'gc_events sample) 0)))
   (ekp-live-commit-evaluator--samples records)))

(defun ekp-live-commit-evaluator--conflict-free-p (records)
  "Return non-nil when every scenario in RECORDS has no conflicts."
  (cl-every
   (lambda (record)
     (cl-every
      (lambda (scenario)
        (= (alist-get 'conflicts scenario) 0))
      (alist-get 'scenarios record)))
   records))

(defun ekp-live-commit-evaluator--ordinary-p99 (records)
  "Return excluded-GC ordinary-key p99 for RECORDS."
  (let ((samples
         (cl-remove-if-not
          (lambda (sample)
            (and (equal (alist-get 'event_kind sample) "ordinary-key")
                 (equal (alist-get 'gc_mode sample) "gc-excluded")))
          (ekp-live-commit-evaluator--samples records))))
    (alist-get 'p99
               (ekp-live-commit-evaluator--statistics samples 'total_ms))))

(defun ekp-live-commit-evaluator--width-report
    (baseline candidate engine width)
  "Compare BASELINE and CANDIDATE structural commits for ENGINE at WIDTH."
  (let ((base
         (ekp-live-commit-evaluator--matching-samples
          baseline "structural-commit" engine "gc-excluded" width))
        (cand
         (ekp-live-commit-evaluator--matching-samples
          candidate "structural-commit" engine "gc-excluded" width)))
    (ekp-live-commit-evaluator--comparison base cand)))

(defun ekp-live-commit-evaluator--target-pass-p (report)
  "Return non-nil when WIDTH-80 REPORT meets target and improvement gates."
  (let* ((candidate (alist-get 'candidate report))
         (total (alist-get 'total candidate)))
    (and (<= (alist-get 'p95 total)
             ekp-live-commit-evaluator--target-ms)
         (<= (alist-get 'p99 total)
             ekp-live-commit-evaluator--target-ms)
         (>= (alist-get 'p95_improvement_pct report)
             ekp-live-commit-evaluator--minimum-improvement)
         (>= (alist-get 'p99_improvement_pct report)
             ekp-live-commit-evaluator--minimum-improvement))))

(defun ekp-live-commit-evaluator--non-regression-p
    (baseline candidate engine width)
  "Return non-nil when ENGINE at WIDTH avoids a material p95 regression."
  (let* ((report
          (ekp-live-commit-evaluator--width-report
           baseline candidate engine width))
         (base (alist-get 'p95
                          (alist-get 'total (alist-get 'baseline report))))
         (cand (alist-get 'p95
                          (alist-get 'total (alist-get 'candidate report)))))
    (<= cand (* base
                (+ 1.0
                   (/ ekp-live-commit-evaluator--maximum-regression
                      100.0))))))

(defun ekp-live-commit-evaluator--all-widths-pass-p
    (baseline candidate)
  "Return non-nil when no engine materially regresses at any width."
  (let ((widths
         (ekp-live-commit-evaluator--env-numbers
          "EKP_LIVE_COMMIT_WIDTHS"
          ekp-live-commit-evaluator--widths)))
    (cl-every
     (lambda (engine)
       (cl-every
        (lambda (width)
          (ekp-live-commit-evaluator--non-regression-p
           baseline candidate engine width))
        widths))
     '("c" "elisp"))))

(defun ekp-live-commit-evaluator--report (baseline candidate)
  "Return the final evaluator report for BASELINE and CANDIDATE."
  (let* ((c80 (ekp-live-commit-evaluator--width-report
               baseline candidate "c" 80))
         (elisp80 (ekp-live-commit-evaluator--width-report
                   baseline candidate "elisp" 80))
         (ordinary-p99
          (ekp-live-commit-evaluator--ordinary-p99 candidate))
         (parity (ekp-live-commit-evaluator--layout-parity-p
                  baseline candidate))
         (zero-work
          (and (ekp-live-commit-evaluator--zero-work-p baseline)
               (ekp-live-commit-evaluator--zero-work-p candidate)))
         (gc-valid
          (and (ekp-live-commit-evaluator--valid-gc-excluded-p baseline)
               (ekp-live-commit-evaluator--valid-gc-excluded-p candidate)))
         (conflict-free
          (and (ekp-live-commit-evaluator--conflict-free-p baseline)
               (ekp-live-commit-evaluator--conflict-free-p candidate)))
         (all-widths
          (ekp-live-commit-evaluator--all-widths-pass-p
           baseline candidate))
         (pass
          (and parity zero-work gc-valid conflict-free all-widths
               (<= ordinary-p99
                   ekp-live-commit-evaluator--ordinary-target-ms)
               (ekp-live-commit-evaluator--target-pass-p c80)
               (ekp-live-commit-evaluator--target-pass-p elisp80))))
    `((pass . ,pass) (layout_parity . ,parity)
      (ordinary_zero_work . ,zero-work)
      (gc_excluded_valid . ,gc-valid)
      (conflict_free . ,conflict-free)
      (all_widths_non_regression . ,all-widths)
      (ordinary_candidate_p99_ms . ,ordinary-p99)
      (width_80 . ((c . ,c80) (elisp . ,elisp80))))))

(defun ekp-live-commit-evaluator--compare ()
  "Compare raw JSONL paths from the environment and enforce the contract."
  (let* ((baseline
          (ekp-live-commit-evaluator--read-jsonl
           (getenv "EKP_LIVE_COMMIT_BASELINE_JSONL")))
         (candidate
          (ekp-live-commit-evaluator--read-jsonl
           (getenv "EKP_LIVE_COMMIT_CANDIDATE_JSONL")))
         (report
          (ekp-live-commit-evaluator--report baseline candidate))
         (path (getenv "EKP_LIVE_COMMIT_REPORT")))
    (when path
      (write-region (concat (json-encode report) "\n")
                    nil path nil 'silent))
    (princ (concat (json-encode report) "\n"))
    (unless (eq (alist-get 'pass report) t)
      (kill-emacs 1))))

(if (equal (getenv "EKP_LIVE_COMMIT_MODE") "compare")
    (ekp-live-commit-evaluator--compare)
  (ekp-live-commit-evaluator--load-source-root)
  (ekp-live-commit-evaluator--write-record
   (ekp-live-commit-evaluator--measure-round)))

;;; ekp-live-commit-evaluator.el ends here
