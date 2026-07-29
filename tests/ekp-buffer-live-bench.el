;;; ekp-buffer-live-bench.el --- Live edit benchmarks for EKP -*- lexical-binding: t; -*-

;;; Commentary:

;; Reproducible public-path latency measurements for `ekp-auto-justify-mode'.
;;
;;   emacs -Q --batch -L . -L tests -l tests/ekp-buffer-live-bench.el
;;
;; To benchmark the C backend, load it before this file.  Batch glyph
;; measurements are internally comparable but are not a replacement for
;; the graphical dynamic verification.  Bind `gc-cons-threshold' to
;; `most-positive-fixnum' to separate mutator latency from GC pauses.

;;; Code:

(require 'cl-lib)
(require 'ekp-buffer)

(defconst ekp-buffer-live-bench--width 80)

(defconst ekp-buffer-live-bench--base
  (concat
   "alpha beta gamma delta epsilon zeta eta theta iota kappa lambda "
   "中文混排让前面的完整语义行共同调整 while the active line stays natural"))

(defvar ekp-buffer-live-bench--expected nil)
(defvar ekp-buffer-live-bench--times nil)
(defvar ekp-buffer-live-bench--gc-events 0)
(defvar ekp-buffer-live-bench--gc-seconds 0.0)

(defun ekp-buffer-live-bench--percentile (values percentile)
  "Return PERCENTILE from numeric VALUES."
  (let* ((sorted (sort (copy-sequence values) #'<))
         (index (1- (ceiling (* percentile (length sorted))))))
    (nth (max 0 (min index (1- (length sorted)))) sorted)))

(defun ekp-buffer-live-bench--statistics ()
  "Return latency statistics for the current scenario."
  (let ((times (nreverse ekp-buffer-live-bench--times)))
    (list :edits (length times)
          :median (ekp-buffer-live-bench--percentile times 0.50)
          :p95 (ekp-buffer-live-bench--percentile times 0.95)
          :p99 (ekp-buffer-live-bench--percentile times 0.99)
          :max (apply #'max times)
          :gc-events ekp-buffer-live-bench--gc-events
          :gc-ms (* 1000 ekp-buffer-live-bench--gc-seconds))))

(defun ekp-buffer-live-bench--record-command (command expected)
  "Run public COMMAND, verify EXPECTED source, and record its latency."
  (let ((started (float-time))
        (gcs-before gcs-done)
        (gc-before gc-elapsed))
    (funcall command)
    (run-hooks 'post-command-hook)
    (push (* 1000 (- (float-time) started))
          ekp-buffer-live-bench--times)
    (cl-incf ekp-buffer-live-bench--gc-events
             (- gcs-done gcs-before))
    (cl-incf ekp-buffer-live-bench--gc-seconds
             (- gc-elapsed gc-before)))
  (setq ekp-buffer-live-bench--expected expected)
  (unless (equal (substring-no-properties (buffer-string)) expected)
    (error "Live benchmark source mismatch"))
  (when (overlays-in (point-min) (point-max))
    (error "Live benchmark created an overlay")))

(defun ekp-buffer-live-bench--insert (character)
  "Insert CHARACTER through `self-insert-command'."
  (ekp-buffer-live-bench--record-command
   (lambda ()
     (let ((last-command-event character))
       (call-interactively #'self-insert-command)))
   (concat ekp-buffer-live-bench--expected (string character))))

(defun ekp-buffer-live-bench--delete-backward ()
  "Delete one character through `delete-backward-char'."
  (ekp-buffer-live-bench--record-command
   (lambda () (call-interactively #'delete-backward-char))
   (substring ekp-buffer-live-bench--expected 0 -1)))

(defun ekp-buffer-live-bench--append-workload ()
  "Type enough mixed prose to cross several semantic boundaries."
  (dotimes (_ 3)
    (mapc #'ekp-buffer-live-bench--insert
          (string-to-list
           (concat
            " extraordinary editing continues smoothly 中文拉丁混排"
            " and all earlier complete lines may move together")))))

(defun ekp-buffer-live-bench--cache-workload ()
  "Revisit two historical paragraph states repeatedly."
  (dotimes (_ 80)
    (ekp-buffer-live-bench--insert ?x)
    (ekp-buffer-live-bench--delete-backward)))

(defun ekp-buffer-live-bench--motion-workload ()
  "Move point across semantic boundaries without changing projection."
  (let* ((state ekp-buffer--live-state)
         (transaction ekp-buffer--live-edit)
         (projection (buffer-substring (point-min) (point-max)))
         (active (ekp-buffer--live-state-active-index state))
         (signatures (ekp-buffer--live-state-signatures state))
         (prefix-end (ekp-buffer--live-state-prefix-end state))
         (prefix-position (marker-position prefix-end))
         (edit-end
          (and transaction
               (ekp-buffer--live-edit-edit-end transaction)))
         (edit-position (and edit-end (marker-position edit-end))))
    (dotimes (_ 48)
      (ekp-buffer-live-bench--record-command
       (lambda () (call-interactively #'backward-char))
       ekp-buffer-live-bench--expected))
    (dotimes (_ 48)
      (ekp-buffer-live-bench--record-command
       (lambda () (call-interactively #'forward-char))
       ekp-buffer-live-bench--expected))
    (unless (and (eq ekp-buffer--live-state state)
                 (= (ekp-buffer--live-state-active-index state) active)
                 (eq (ekp-buffer--live-state-signatures state) signatures)
                 (eq ekp-buffer--live-edit transaction)
                 (eq (ekp-buffer--live-state-prefix-end state) prefix-end)
                 (= (marker-position prefix-end) prefix-position)
                 (or (null edit-end)
                     (and (eq (ekp-buffer--live-edit-edit-end transaction)
                              edit-end)
                          (= (marker-position edit-end) edit-position)))
                 (equal-including-properties
                  (buffer-substring (point-min) (point-max))
                  projection))
      (error "Point motion changed the live projection"))))

(defun ekp-buffer-live-bench--boundary-workload ()
  "Create and remove hard boundaries through public commands."
  (dotimes (_ 24)
    (ekp-buffer-live-bench--record-command
     (lambda () (call-interactively #'newline))
     (concat ekp-buffer-live-bench--expected "\n"))
    (ekp-buffer-live-bench--delete-backward)))

(defun ekp-buffer-live-bench--instrument (workload)
  "Run WORKLOAD and return planner/cache counters."
  (let ((plan-calls 0)
        (cache-gets 0)
        (cache-hits 0)
        (original-plan (symbol-function 'ekp-layout-plan))
        (original-get (symbol-function 'ekp-buffer--live-cache-get)))
    (cl-letf (((symbol-function 'ekp-layout-plan)
               (lambda (&rest arguments)
                 (cl-incf plan-calls)
                 (apply original-plan arguments)))
              ((symbol-function 'ekp-buffer--live-cache-get)
               (lambda (key)
                 (cl-incf cache-gets)
                 (let ((plan (funcall original-get key)))
                   (when plan (cl-incf cache-hits))
                   plan))))
      (funcall workload))
    (list :plan-calls plan-calls
          :cache-hits cache-hits
          :cache-misses (- cache-gets cache-hits))))

(defun ekp-buffer-live-bench--scenario (name workload)
  "Run named live-edit WORKLOAD and return its measurements."
  (with-temp-buffer
    (text-mode)
    (insert ekp-buffer-live-bench--base)
    (goto-char (point-max))
    (cl-letf (((symbol-function 'ekp-buffer--window-pixel)
               (lambda (&optional _window)
                 ekp-buffer-live-bench--width)))
      (ekp-auto-justify-mode 1)
      (let ((ekp-buffer-live-bench--expected
            (substring-no-properties (buffer-string)))
            (ekp-buffer-live-bench--times nil)
            (ekp-buffer-live-bench--gc-events 0)
            (ekp-buffer-live-bench--gc-seconds 0.0))
        (garbage-collect)
        (unwind-protect
            (append (list :name name)
                    (ekp-buffer-live-bench--statistics-after
                     workload))
          (ekp-auto-justify-mode -1))))))

(defun ekp-buffer-live-bench--statistics-after (workload)
  "Run WORKLOAD with instrumentation and return combined statistics."
  (let ((counters (ekp-buffer-live-bench--instrument workload)))
    (append (ekp-buffer-live-bench--statistics)
            counters
            (list :source-ok t
                  :conflicts (length ekp-buffer--conflicts)
                  :cache-size (length ekp-buffer--live-plan-cache)))))

(defun ekp-buffer-live-bench--format (result)
  "Format live benchmark RESULT as one report row."
  (format
   "%-15s %5d %8.3f %8.3f %8.3f %8.3f %6d %6d %6d %3d %7.2f %5d %s"
   (plist-get result :name)
   (plist-get result :edits)
   (plist-get result :median)
   (plist-get result :p95)
   (plist-get result :p99)
   (plist-get result :max)
   (plist-get result :plan-calls)
   (plist-get result :cache-hits)
   (plist-get result :cache-misses)
   (plist-get result :gc-events)
   (plist-get result :gc-ms)
   (plist-get result :cache-size)
   (if (<= (plist-get result :p99) 16.0) "PASS" "MISS")))

;;;###autoload
(defun ekp-buffer-live-bench-run ()
  "Run live-edit scenarios and print percentile/caching evidence."
  (let ((scenarios
         `(("append" . ,#'ekp-buffer-live-bench--append-workload)
           ("cache-revisit" . ,#'ekp-buffer-live-bench--cache-workload)
           ("point-motion" . ,#'ekp-buffer-live-bench--motion-workload)
           ("hard-boundary" . ,#'ekp-buffer-live-bench--boundary-workload))))
    (princ
     (format "EKP live edit benchmark: engine=%s width=%dpx\n"
             (if (and ekp-use-c-module (ekp--c-available-p)) "C" "elisp")
             ekp-buffer-live-bench--width))
    (princ
     (concat
      "scenario        edits   median      p95      p99      max"
      "  plans   hits misses  gc   gc-ms cache frame\n"))
    (dolist (scenario scenarios)
      (princ
       (concat
        (ekp-buffer-live-bench--format
         (ekp-buffer-live-bench--scenario
          (car scenario) (cdr scenario)))
        "\n")))))

(ekp-buffer-live-bench-run)

(provide 'ekp-buffer-live-bench)

;;; ekp-buffer-live-bench.el ends here
