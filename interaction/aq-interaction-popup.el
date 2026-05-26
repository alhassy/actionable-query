;;; aq-interaction-popup.el --- Transient `?' row-action popup  -*- lexical-binding: t; -*-

;; Author: Musa Al-hassy <alhassy@gmail.com>

;;; Commentary:

;; `aq--install-popup' builds an ad-hoc transient prefix from a view's
;; ACTIONS list — each (key desc fn) becomes a popup entry that calls
;; FN on `aq--current-row', the row captured at popup-time.  The
;; transient also surfaces the standard structural commands
;; (m/U/B/R/M-up/M-down/...) and vtable's own niceties ({, }, S, g, q),
;; so the user has a single discoverable entry point via `?'.

;;; Code:

(require 'cl-lib)
(require 'transient)
(require 'aq-interaction-bulk)         ; mark/unmark/bulk commands
(require 'aq-interaction-row-reorder)  ; `aq--move-row'

(declare-function vtable-current-object         "vtable")
(declare-function vtable-previous-column        "vtable")
(declare-function vtable-next-column            "vtable")
(declare-function vtable-narrow-current-column  "vtable")
(declare-function vtable-widen-current-column   "vtable")
(declare-function vtable-sort-by-current-column "vtable")
(declare-function vtable-revert-command         "vtable")

(defvar-local aq--current-row nil
  "The vtable row captured when `?' is pressed.")

(defun aq--install-popup (popup-actions)
  "Build a transient from POPUP-ACTIONS and bind it to `?' buffer-locally.
Each action is (KEY DESCRIPTION FUNCTION); FUNCTION receives the current row."
  (eval `(transient-define-prefix aq--transient-popup ()
           "Neato row actions"
           ["Row Actions"
            :class transient-column
            ,@(cl-loop for (key desc fn) in popup-actions
                       collect (let ((f fn))
                                 `(,key ,desc
                                        (lambda ()
                                          (interactive)
                                          (funcall ',f aq--current-row)))))]
           [["Structural Commands"
             ("m"   "Mark row for bulk action"         actionable-query-mark-row)
             ("U"   "Unmark all"                       actionable-query-unmark-all)
             ("B"   "Bulk action on marked rows"       actionable-query-bulk-action-interactive)
             ("R"   "Resurrect snoozed items"          (lambda () (interactive) (call-interactively (key-binding (kbd "R")))))
             ("M-<up>"    "Move row up"                ,(lambda () (interactive) (aq--move-row -1)))
             ("M-<down>"  "Move row down"              ,(lambda () (interactive) (aq--move-row  1)))
             ("M-<left>"  "Navigate to prev column"    vtable-previous-column)
             ("M-<right>" "Navigate to next column"    vtable-next-column)]
            ["Vtable"
             ("{"   "Narrow column"       vtable-narrow-current-column)
             ("}"   "Widen column"        vtable-widen-current-column)
             ("S"   "Toggle sort"         vtable-sort-by-current-column)
             ("g"   "Refresh table"       vtable-revert-command)
             ("="   "Filter column"       (lambda () (interactive) (call-interactively (key-binding (kbd "=")))))
             ("q"   "Quit buffer"         quit-window)]]
           ["" ("C-g" "Dismiss popup" transient-quit-one)]) t)
  (local-set-key
   (kbd "?")
   (lambda ()
     (interactive)
     (setq aq--current-row (vtable-current-object))
     (with-selected-window (get-buffer-window (current-buffer))
       (call-interactively 'aq--transient-popup)))))

(provide 'aq-interaction-popup)
;;; aq-interaction-popup.el ends here
