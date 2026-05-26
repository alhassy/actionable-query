;;; aq-interaction-row-reorder.el --- M-<up>/M-<down> row reordering  -*- lexical-binding: t; -*-

;; Author: Musa Al-hassy <alhassy@gmail.com>

;;; Commentary:

;; Buffer-local installer for row reordering: M-<up> and M-<down>
;; swap the row at point with its neighbour by removing then
;; re-inserting via vtable's own primitives.

;;; Code:

(require 'cl-lib)
(require 'vtable)

(defun aq--move-row (direction)
  "Move the vtable row at point up (DIRECTION = -1) or down (DIRECTION = 1)."
  (when-let* ((table     (vtable-current-table))
              (obj       (vtable-current-object))
              (objs      (vtable-objects table))
              (idx       (cl-position obj objs))
              (target    (+ idx direction))
              ((and (>= target 0) (< target (length objs))))
              (neighbour (nth target objs)))
    (vtable-remove-object table obj)
    (vtable-insert-object table obj neighbour (< direction 0))
    (vtable-goto-object obj)))

(defun aq--install-row-reorder ()
  "Bind M-<up>/M-<down> to reorder vtable rows, buffer-locally."
  (local-set-key (kbd "M-<up>")   (lambda () (interactive) (aq--move-row -1)))
  (local-set-key (kbd "M-<down>") (lambda () (interactive) (aq--move-row  1))))

(provide 'aq-interaction-row-reorder)
;;; aq-interaction-row-reorder.el ends here
