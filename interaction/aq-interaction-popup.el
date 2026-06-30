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

(defvar aq--popup-default-groups
  '(("View"
     ("g"   "Refresh table"       vtable-revert-command)
     ("="   "Filter column"       (lambda () (interactive) (call-interactively (key-binding (kbd "=")))))
     ("R"   "Resurrect snoozed items"    (lambda () (interactive) (call-interactively (key-binding (kbd "R")))))
     ("q"   "Quit buffer"         quit-window))
    ("Structural Commands"
     ("S"   "Toggle sort"                vtable-sort-by-current-column)
     ("{"   "Narrow column"              vtable-narrow-current-column)
     ("}"   "Widen column"               vtable-widen-current-column)
     ("M-<up>"    "Move row up"          (lambda () (interactive) (aq--move-row -1)))
     ("M-<down>"  "Move row down"        (lambda () (interactive) (aq--move-row  1)))
     ("M-<left>"  "Navigate to prev column" vtable-previous-column)
     ("M-<right>" "Navigate to next column" vtable-next-column)
     ("m"   "Mark row for bulk action"   actionable-query-mark-row)
     ("U"   "Unmark all"                 actionable-query-unmark-all)
     ("B"   "Bulk action on marked rows" actionable-query-bulk-action-interactive))
    ("Favourites"
     ("h"   "Toggle heart on this row"   (lambda () (interactive) (call-interactively (key-binding (kbd "h")))))
     ("H"   "Show hearted-only / all"    (lambda () (interactive) (call-interactively (key-binding (kbd "H"))))))
    ("Org Agenda Commands"
     ("RET"     "Jump to Org tree"   aq-nav-goto-row-heading)
     ("t"       "Cycle TODO state"   aq-agenda-todo)
     ("C-c C-s" "Schedule"           aq-agenda-schedule)
     ("C-c C-d" "Set deadline"       aq-agenda-deadline)
     (","       "Set priority"       aq-agenda-set-priority)
     (":"       "Set tags"           aq-agenda-set-tags)
     ("E"       "Set effort"         aq-agenda-set-effort)
     ("I"       "Clock in"           aq-agenda-clock-in)
     ("O"       "Clock out"          aq-agenda-clock-out)
     ("$"       "Archive entry"      aq-agenda-archive)
     ("C-k"     "Kill subtree"       aq-agenda-kill-subtree)))
  "Default `?'-popup command groups, each (TITLE . (KEY DESC CMD)...).
A default entry is hidden when the view redefines its KEY via `:actions'
(those land in the \"Row Actions\" group instead) --- so `?' shows every
default binding *except* the ones a view has taken over.")

(defun aq--install-popup (popup-actions)
  "Build a transient from POPUP-ACTIONS and bind it to `?' buffer-locally.
Each action is (KEY DESCRIPTION FUNCTION); FUNCTION receives the current row.
The default groups (View / Structural / Org Agenda) are shown too, but any
default entry whose KEY a view has redefined is dropped --- it already
appears under \"Row Actions\" with the view's own binding."
  (let* ((claimed (mapcar #'car popup-actions))   ; keys the view redefined
         (groups
          (cl-loop for (title . entries) in aq--popup-default-groups
                   for kept = (cl-remove-if (lambda (e) (member (car e) claimed)) entries)
                   when kept
                   collect (vconcat (list title) kept))))
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
             ,(apply #'vector groups)
             ["" ("C-g" "Dismiss popup" transient-quit-one)]) t))
  (local-set-key
   (kbd "?")
   (lambda ()
     (interactive)
     (setq aq--current-row (vtable-current-object))
     (with-selected-window (get-buffer-window (current-buffer))
       (call-interactively 'aq--transient-popup)))))

(provide 'aq-interaction-popup)
;;; aq-interaction-popup.el ends here
