;;; aq-interaction-edit.el --- Edit the current cell when its column is `:editable'  -*- lexical-binding: t; -*-

;; Author: Musa Al-hassy <alhassy@gmail.com>

;;; Commentary:

;; A column marked `:editable t' in `:columns' also needs a `:setter
;; (lambda (object new-value) …)' — the getter has no inverse vtable
;; can infer, so the call site supplies one explicitly.  `aq--coerce-
;; columns' (in aq-interaction-filters.el) records these setters in
;; the buffer-local `aq--editable-setters', keyed by column name.
;;
;; `e' prompts with `read-string', pre-filled with the cell's current
;; displayed value, then calls the setter and `vtable-revert' to redraw.
;; Reverting the whole table --- rather than just the edited row via
;; `vtable-update-object' --- is deliberate: a `:setter' may mutate
;; sibling rows too (e.g. a Celsius/Fahrenheit pair that keep each
;; other in sync), so every row needs a chance to repaint.
;;
;; `aq--install-cell-edit-highlight' adds a cheap visual cue: a
;; post-command hook moves a single overlay onto the editable cell
;; under point, so the user can see at a glance which cells respond
;; to `e' (or a view's own RET action) before pressing anything.

;;; Code:

(declare-function vtable-current-table   "vtable")
(declare-function vtable-current-column  "vtable")
(declare-function vtable-current-object  "vtable")
(declare-function vtable-columns         "vtable")
(declare-function vtable-column-p        "vtable")
(declare-function vtable-column-name     "vtable")
(declare-function vtable-revert          "vtable")
(declare-function vtable--clear-cache    "vtable")
(declare-function vtable--get-value      "vtable")
(defvar aq--editable-setters)            ; defvar-local in aq-interaction-filters

(defun aq--edit-current-cell-1 (setters)
  "Edit the cell at point using SETTERS, an alist (column-index . setter-fn).
Signals a `user-error' when the current column has no setter in SETTERS.
The dedicated-buffer path passes the buffer-local `aq--editable-setters';
the splice path passes the copy carried on the region's `aq-region-ctx',
so one implementation serves both."
  (let* ((table  (vtable-current-table))
         (idx    (vtable-current-column))
         (col    (and idx (elt (vtable-columns table) idx)))
         (object (vtable-current-object))
         (name   (and (vtable-column-p col) (vtable-column-name col)))
         (setter (and idx (alist-get idx setters nil nil #'eql))))
    (unless setter
      (user-error "Column %s is not editable" (or name "?")))
    (let* ((current (vtable--get-value object idx col table))
           (new     (read-string (format "%s: " name) (format "%s" current))))
      (funcall setter object new)
      ;; `vtable-revert' alone reuses cached line renderings --- the
      ;; interactive `vtable-revert-command' clears the cache first for
      ;; the same reason; without this, a `:setter' mutating a sibling
      ;; row's plist in place never shows up on screen.
      (vtable--clear-cache table)
      (vtable-revert))))

(defun aq--edit-current-cell ()
  "Edit the cell at point, if its column is `:editable' (dedicated-buffer `e').
Signals a `user-error' when the current column has no registered setter."
  (interactive)
  (aq--edit-current-cell-1 aq--editable-setters))

(defun aq--install-cell-edit ()
  "Bind `e' to `aq--edit-current-cell' buffer-locally."
  (local-set-key (kbd "e") #'aq--edit-current-cell))

(defface aq-editable-cell-face
  '((t :underline t :inherit highlight))
  "Face applied to the editable cell under point.")

(defvar-local aq--editable-cell-overlay nil
  "Single reused overlay for highlighting the editable cell under point.
Moved (not recreated) on every command --- cheaper than allocating a
fresh overlay per move, and there is only ever at most one to show.")

(defun aq--editable-cell-bounds ()
  "Return (BEG . END) of the editable cell's text run under point, or nil.
Point must sit on a column index present in `aq--editable-setters'.
Uses the same `vtable-column' text property `vtable-current-column'
reads, so the bounds always match what `aq--edit-current-cell' would
act on."
  (when-let* ((idx (vtable-current-column))
              ((alist-get idx aq--editable-setters nil nil #'eql)))
    (let ((beg (point)) (end (point)))
      (while (and (> beg (point-min))
                  (eql (get-text-property (1- beg) 'vtable-column) idx))
        (setq beg (1- beg)))
      (while (and (< end (point-max))
                  (eql (get-text-property end 'vtable-column) idx))
        (setq end (1+ end)))
      (cons beg end))))

(defun aq--update-cell-edit-highlight ()
  "Move the editable-cell overlay onto point's cell, or hide it.
Installed on `post-command-hook'; cheap no-op when point hasn't moved
off the previously highlighted cell."
  (let ((bounds (aq--editable-cell-bounds)))
    (if bounds
        (progn
          (unless aq--editable-cell-overlay
            (setq aq--editable-cell-overlay (make-overlay (car bounds) (cdr bounds)))
            (overlay-put aq--editable-cell-overlay 'face 'aq-editable-cell-face))
          (move-overlay aq--editable-cell-overlay (car bounds) (cdr bounds)))
      (when aq--editable-cell-overlay
        (delete-overlay aq--editable-cell-overlay)))))

(defun aq--install-cell-edit-highlight ()
  "Install the post-command hook that highlights the editable cell under point."
  (add-hook 'post-command-hook #'aq--update-cell-edit-highlight nil :local))

(provide 'aq-interaction-edit)
;;; aq-interaction-edit.el ends here
