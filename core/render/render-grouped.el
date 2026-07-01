;;; render-grouped.el --- Multi-table grouped rendering  -*- lexical-binding: t; -*-

;; Author: Musa Al-hassy <alhassy@gmail.com>

;;; Commentary:

;; A grouped view is one whose deliver callback hands actionable-query
;; an alternating plist `("Group A" list-A "Group B" list-B …)' rather
;; than a flat list of objects.  `aq--grouped-p' is the recogniser;
;; `aq--render-grouped' walks the alist form and emits one styled
;; title + `make-vtable' per group, all in the same buffer.

;;; Code:

(require 'cl-lib)
(require 'vtable)
(require 'state-cache)         ; `aq--obj-id'
(require 'state-dismissal)     ; `aq--dismissed-items'
(require 'interaction-keys)    ; `aq--actions->vtable', `aq--vtable-keymap'

(defun aq--grouped-p (result)
  "Return non-nil if RESULT is a grouped plist (alternating title/objects pairs).
Heuristic: non-empty list whose first element is a string and second is a list."
  (and (consp result)
       (stringp (car result))
       (consp (cadr result))))

(defun aq--render-grouped (groups columns actions rest-kwargs view-name)
  "Render GROUPS as multiple vtables, each preceded by a styled title.
GROUPS is an alist ((\"Title\" . objects) …).
COLUMNS, ACTIONS, REST-KWARGS, VIEW-NAME are forwarded to each `make-vtable'."
  (let ((dismissed (aq--dismissed-items view-name)))
    (dolist (group groups)
      (let* ((title   (car group))
             (objects (cdr group))
             (inhibit-read-only t))
        (goto-char (point-max))
        (insert "\n")
        (insert (propertize title
                            'face '(:height 1.1 :weight bold :foreground "slate blue")))
        (insert "\n")
        (apply #'make-vtable
               :objects (cl-remove-if
                         (lambda (o) (member (aq--obj-id o) dismissed))
                         objects)
               :columns columns
               :actions (aq--actions->vtable actions)
               :keymap aq--vtable-keymap
               rest-kwargs)))))

(provide 'render-grouped)
;;; render-grouped.el ends here
