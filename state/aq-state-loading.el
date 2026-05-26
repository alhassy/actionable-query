;;; aq-state-loading.el --- Spinner UI and auto-refresh timer  -*- lexical-binding: t; -*-

;; Author: Musa Al-hassy <alhassy@gmail.com>

;;; Commentary:

;; Two timer-driven concerns kept together because they share the
;; buffer-local timer-cancellation idiom:
;;
;;   • Loading spinner — `aq--show-loading' erases the buffer and
;;     installs a 1-second `aq--loading-timer' that cycles the
;;     `aq--hourglass-frames'.  `aq--stop-loading' is the inverse;
;;     `aq--format-elapsed' is its companion display helper.
;;     The animation is wrapped in `save-excursion' so each tick does
;;     not yank the user's cursor back to `point-min'.
;;
;;   • Auto-refresh — `aq--parse-refresh-interval' turns "5 minutes"
;;     into seconds; `aq--setup-refresh-timer' installs a repeating
;;     fetch and a kill-buffer hook that cancels it.

;;; Code:

(require 'aq-state-cache)         ; `aq--last-fetch-start-time', `aq--auto-refresh-timer'

(declare-function aq--center-message "aq-interaction-help-echo")  ; forward ref

;;; ─── spinner ──────────────────────────────────────────────────────────────

(defvar-local aq--loading-timer nil
  "Repeating timer that animates the ⏳ spinner while data is in flight.")

(defconst aq--hourglass-frames ["⏳" "⌛"]
  "Frames cycled by the loading spinner.")

(defun aq--format-elapsed (seconds)
  "Render SECONDS (float) as a terse human duration.
Examples: 0.12 → \"120ms\", 1.4 → \"1.4s\", 123.4 → \"2m 03.4s\"."
  (cond
   ((< seconds 1)  (format "%dms" (round (* seconds 1000))))
   ((< seconds 60) (format "%.1fs" seconds))
   (t              (let* ((m (floor (/ seconds 60)))
                          (s (- seconds (* m 60))))
                     (format "%dm %04.1fs" m s)))))

(defun aq--stop-loading (buf)
  "Cancel the loading spinner timer for BUF, if any."
  (when (buffer-live-p buf)
    (with-current-buffer buf
      (when aq--loading-timer
        (cancel-timer aq--loading-timer)
        (setq aq--loading-timer nil)))))

(defun aq--show-loading (buf)
  "Erase BUF, insert a centered animated hourglass, and start a 1-second spin timer."
  (aq--stop-loading buf)
  (with-current-buffer buf
    (let ((inhibit-read-only t))
      (erase-buffer)
      (insert (aq--center-message
               (propertize "⏳ Loading…" 'face '(:height 0.9 :foreground "gray50")))))
    (let ((frame 0))
      (setq aq--loading-timer
            (run-with-timer
             1 1
             (lambda ()
               (when (buffer-live-p buf)
                 (with-current-buffer buf
                   (setq frame (% (1+ frame) (length aq--hourglass-frames)))
                   (let* ((glyph (aref aq--hourglass-frames frame))
                          (inhibit-read-only t))
                     ;; Wrap in `save-excursion' — the spinner ticks every
                     ;; second, and without this each tick yanks point back
                     ;; to `point-min', dragging the user's cursor home
                     ;; whenever the view window is selected.
                     (save-excursion
                       (goto-char (point-min))
                       (when (re-search-forward "[⏳⌛]" nil t)
                         (replace-match glyph)))))))))
    ;; Ensure the spinner is cancelled if the buffer is killed mid-fetch.
    (add-hook 'kill-buffer-hook
              (lambda () (aq--stop-loading (current-buffer)))
              nil :local))))

;;; ─── auto-refresh ─────────────────────────────────────────────────────────

(defun aq--parse-refresh-interval (spec)
  "Parse SPEC like \"5 minutes\" or \"1 day\" into seconds.  Returns nil on failure."
  (when (stringp spec)
    (when (string-match "\\([0-9]+\\)[ \t]+\\(minute\\|hour\\|day\\)s?" spec)
      (let ((n    (string-to-number (match-string 1 spec)))
            (unit (match-string 2 spec)))
        (* n (pcase unit
               ("minute" 60)
               ("hour"   3600)
               ("day"    86400)))))))

(defun aq--setup-refresh-timer (buf async-fn deliver interval)
  "Install a repeating timer in BUF that calls (ASYNC-FN DELIVER) every INTERVAL seconds.
Returns the timer object.  Cancels itself if BUF is killed."
  (let ((timer (run-with-timer interval interval
                               (lambda ()
                                 (when (buffer-live-p buf)
                                   (with-current-buffer buf
                                     (setq aq--last-fetch-start-time (float-time))
                                     (funcall async-fn deliver)))))))
    (with-current-buffer buf
      (add-hook 'kill-buffer-hook
                (lambda ()
                  (when aq--auto-refresh-timer
                    (cancel-timer aq--auto-refresh-timer)
                    (setq aq--auto-refresh-timer nil)))
                nil :local))
    timer))

(provide 'aq-state-loading)
;;; aq-state-loading.el ends here
