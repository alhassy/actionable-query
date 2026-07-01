;;; interaction-bulk.el --- Row marking and bulk-action transient  -*- lexical-binding: t; -*-

;; Author: Musa Al-hassy <alhassy@gmail.com>

;;; Commentary:

;; Two parallel mark stores:
;;
;;   • `aq--marked-rows' — a buffer-local list, used by views opened in
;;     their own dedicated buffer.
;;
;;   • `(aq-region-ctx-marked-rows ctx)' — slot on the per-region
;;     context, used by views spliced into a host (e.g. an org file).
;;     Each region keeps its own marks, isolating bulk operations.
;;
;; `m' toggles a mark at point, `U' clears, `B' constructs an ad-hoc
;; transient over ACTIONS and applies the chosen action to every
;; marked row (or the row at point when nothing is marked).
;; `aq--mark-overlay' draws the leading `>' glyph; `aq--clear-mark-overlays'
;; tears them down.

;;; Code:

(require 'cl-lib)
(require 'transient)
(require 'state-region-ctx)    ; `aq-region-ctx', `aq--ctx-at-point'

(declare-function vtable-current-object "vtable")

(defvar-local aq--marked-rows nil
  "List of vtable objects marked for bulk action in this buffer.")

(defun aq--mark-overlay ()
  "Place a `>' overlay at the start of the current line.  Returns the overlay."
  (let ((ov (make-overlay (line-beginning-position) (+ 2 (line-beginning-position)))))
    (overlay-put ov 'actionable-query-mark t)
    (overlay-put ov 'before-string (propertize "> " 'face '(:foreground "red" :weight bold)))
    ov))

(defun aq--clear-mark-overlays (&optional ctx)
  "Remove actionable-query mark overlays.
With CTX (an `aq-region-ctx'), removes only the overlays it owns.
Without CTX, removes every actionable-query mark overlay in the buffer
— legacy behaviour preserved for the dedicated-buffer path."
  (if ctx
      (progn
        (mapc #'delete-overlay (aq-region-ctx-mark-overlays ctx))
        (setf (aq-region-ctx-mark-overlays ctx) nil))
    (remove-overlays (point-min) (point-max) 'actionable-query-mark t)))

(defun actionable-query-mark-row (&optional arg)
  "Toggle the bulk mark on the vtable row at point.
With a numeric prefix ARG, mark that many consecutive rows forward.
When point sits in a spliced region, marks live on its `aq-region-ctx';
otherwise on the buffer-local `aq--marked-rows'."
  (interactive "p")
  (let ((ctx (aq--ctx-at-point)))
    (dotimes (_ (or arg 1))
      (when-let ((obj (vtable-current-object)))
        (if ctx
            (if (memq obj (aq-region-ctx-marked-rows ctx))
                (progn
                  (setf (aq-region-ctx-marked-rows ctx)
                        (delq obj (aq-region-ctx-marked-rows ctx)))
                  ;; Drop only overlays on this line — same idea as the
                  ;; line-bounded `remove-overlays' below, but scoped to
                  ;; the ctx's own list so other regions' marks are safe.
                  (let* ((bol  (line-beginning-position))
                         (eol  (line-end-position))
                         (here (cl-remove-if-not
                                (lambda (ov) (and (>= (overlay-start ov) bol)
                                                  (<= (overlay-end ov)   eol)))
                                (aq-region-ctx-mark-overlays ctx))))
                    (mapc #'delete-overlay here)
                    (setf (aq-region-ctx-mark-overlays ctx)
                          (cl-set-difference (aq-region-ctx-mark-overlays ctx)
                                             here))))
              (push obj (aq-region-ctx-marked-rows ctx))
              (push (aq--mark-overlay) (aq-region-ctx-mark-overlays ctx)))
          (if (memq obj aq--marked-rows)
              (progn
                (setq aq--marked-rows (delq obj aq--marked-rows))
                (remove-overlays (line-beginning-position) (line-end-position)
                                 'actionable-query-mark t))
            (push obj aq--marked-rows)
            (aq--mark-overlay)))
        (forward-line 1)))
    (message "%d row(s) marked"
             (length (if ctx (aq-region-ctx-marked-rows ctx) aq--marked-rows)))))

(defun actionable-query-unmark-all ()
  "Unmark all bulk-marked rows in the current scope (region or buffer)."
  (interactive)
  (let ((ctx (aq--ctx-at-point)))
    (if ctx
        (progn
          (setf (aq-region-ctx-marked-rows ctx) nil)
          (aq--clear-mark-overlays ctx))
      (setq aq--marked-rows nil)
      (aq--clear-mark-overlays))
    (message "All marks removed")))

(defun actionable-query-bulk-action (fn)
  "Run FN on every marked row in the current scope (or the row at point).
Clears marks afterwards."
  (let* ((ctx     (aq--ctx-at-point))
         (marked  (if ctx (aq-region-ctx-marked-rows ctx) aq--marked-rows))
         (targets (or marked
                      (when-let ((obj (vtable-current-object))) (list obj)))))
    (dolist (obj targets) (funcall fn obj))
    (if ctx
        (progn
          (setf (aq-region-ctx-marked-rows ctx) nil)
          (aq--clear-mark-overlays ctx))
      (setq aq--marked-rows nil)
      (aq--clear-mark-overlays))
    (message "Bulk action applied to %d row(s)" (length targets))))

(defun actionable-query-bulk-action-interactive ()
  "Show the bulk-action transient for currently marked rows."
  (interactive)
  (call-interactively 'aq--bulk-transient))

(defun aq--install-bulk (actions)
  "Bind m/U/B for bulk marking using ACTIONS for the `B' transient."
  (local-set-key (kbd "m") #'actionable-query-mark-row)
  (local-set-key (kbd "U") #'actionable-query-unmark-all)
  (local-set-key
   (kbd "B")
   (lambda ()
     (interactive)
     (let ((targets (or aq--marked-rows
                        (when-let ((obj (vtable-current-object))) (list obj)))))
       (unless targets (user-error "Nothing marked"))
       (eval `(transient-define-prefix aq--bulk-transient ()
                ,(format "Bulk action on %d row(s)" (length targets))
                [:class transient-column
                        ,@(cl-loop for (key desc fn) in actions
                                   collect (let ((f fn) (ts targets))
                                             `(,key ,desc
                                                    (lambda ()
                                                      (interactive)
                                                      (actionable-query-bulk-action ',f)))))]) t)
       (with-selected-window (get-buffer-window (current-buffer))
         (call-interactively 'aq--bulk-transient))))))

(provide 'interaction-bulk)
;;; interaction-bulk.el ends here
