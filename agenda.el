;;; agenda.el --- Org-agenda command parity and :org-ql preset for actionable-query  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Musa Al-hassy

;; Author: Musa Al-hassy <alhassy@gmail.com>

;; This file is part of actionable-query.

;;; Commentary:

;; Provides three things on top of actionable-query.el:
;;
;; 1. `aq-agenda-install-keys' — installs the full suite of org-agenda-style
;;    keybindings (clock in/out, schedule, deadline, TODO, priority, tags,
;;    effort, archive, kill subtree) buffer-locally.  Called automatically
;;    by `aq--install-standard-hooks' when `:org' is present.
;;
;; 2. `aq-notify-macos' — helper to fire a macOS notification from
;;    Emacs; used as the default body for the `:async-notifier' keyword.
;;
;; 3. `:org-ql' preset keyword — wraps an org-ql sexp into an
;;    actionable-query view with sane default columns and all agenda keys
;;    wired up out of the box.

;;; Code:

(require 'org)
(require 'org-clock)
(require 'vtable)

;;; ─── macOS notification helper ───────────────────────────────────────────────

(defun aq-notify-macos (title &optional body)
  "Emit a macOS Notification Center alert with TITLE and optional BODY.
Fires asynchronously via `osascript'; never blocks Emacs."
  (call-process "osascript" nil 0 nil
                "-e" (format "display notification %S with title %S"
                             (or body "") title)))

;;; ─── marker helpers ──────────────────────────────────────────────────────────

(defun aq-agenda--current-marker ()
  "Return the `org-hd-marker' or `org-marker' on the current line, or nil."
  (or (get-text-property (line-beginning-position) 'org-hd-marker)
      (get-text-property (line-beginning-position) 'org-marker)))

(defun aq-agenda--ensure-marker (obj title-string)
  "Return a live Org marker for OBJ, creating a heading if none exists.
TITLE-STRING is the heading text to use when creating.  The new heading
lands at the end of `org-default-notes-file' with a CREATED timestamp."
  (let* ((table (actionable-query-resolve-org-markers
                 (list obj) (lambda (_) title-string)))
         (marker (gethash obj table)))
    (or (and (markerp marker) (marker-buffer marker) marker)
        ;; Nothing found — create a minimal TODO heading.
        (with-current-buffer (find-file-noselect org-default-notes-file)
          (save-excursion
            (goto-char (point-max))
            (unless (bolp) (insert "\n"))
            (insert (format "* TODO %s\n  :PROPERTIES:\n  :CREATED: %s\n  :END:\n"
                            title-string
                            (format-time-string "[%Y-%m-%d %a %H:%M]")))
            (org-back-to-heading t)
            (point-marker))))))

(defun aq-agenda--marker-or-error ()
  "Return the Org marker for the current row, or signal `user-error'."
  (or (aq-agenda--current-marker)
      (user-error "No Org entry linked to this row — add `:org' to your view")))

;;; ─── display helper (mirrors org-agenda-show-new-time) ──────────────────────

(defun aq-agenda-show-new-time (marker stamp &optional prefix)
  "Show STAMP right-aligned on the line whose `org-marker' is MARKER.
PREFIX defaults to \" S\" (scheduled).  Uses a `display' text property
so it disappears on the next vtable revert without any cleanup."
  (save-excursion
    (catch 'found
      (goto-char (point-min))
      (while (not (eobp))
        (when (equal marker (or (get-text-property (line-beginning-position) 'org-marker)
                                (get-text-property (line-beginning-position) 'org-hd-marker)))
          (let* ((label (concat (or prefix " S") " => " stamp " "))
                 (col   (max 1 (- (window-max-chars-per-line) (length label))))
                 (inhibit-read-only t))
            (remove-text-properties (line-beginning-position) (line-end-position) '(display nil))
            (save-excursion
              (org-move-to-column col t)
              (add-text-properties
               (1- (point)) (line-end-position)
               (list 'display
                     (propertize label 'face '(secondary-selection default))))))
          (throw 'found t))
        (forward-line 1)))))

;;; ─── post-mutation display refresh ──────────────────────────────────────────

(defun aq-agenda--update-line (_marker)
  "Refresh the vtable after a mutation.  MARKER is accepted but unused;
a full `vtable-revert' is the safest correct choice for now."
  (when-let ((tbl (vtable-current-table)))
    (vtable--clear-cache tbl)
    (vtable-revert)))

;;; ─── agenda commands ─────────────────────────────────────────────────────────

(defun aq-agenda-clock-in ()
  "Clock in to the Org entry linked to the current vtable row."
  (interactive)
  (let ((marker (aq-agenda--marker-or-error)))
    (org-with-remote-undo (marker-buffer marker)
      (with-current-buffer (marker-buffer marker)
        (widen)
        (goto-char marker)
        (org-clock-in)))
    (aq-agenda--update-line marker)))

(defun aq-agenda-clock-out ()
  "Clock out of the currently running Org clock."
  (interactive)
  (unless (org-clocking-p)
    (user-error "No running clock"))
  (org-with-remote-undo (marker-buffer org-clock-marker)
    (org-clock-out))
  (aq-agenda--update-line org-clock-marker))

(defun aq-agenda-schedule (arg)
  "Schedule the Org entry linked to the current vtable row.
With prefix ARG, remove the scheduled timestamp."
  (interactive "P")
  (let ((marker (aq-agenda--marker-or-error)))
    (org-with-remote-undo (marker-buffer marker)
      (let (ts)
        (with-current-buffer (marker-buffer marker)
          (widen)
          (goto-char marker)
          (setq ts (org-schedule arg)))
        (when (stringp ts)
          (aq-agenda-show-new-time marker ts " S"))))
    (aq-agenda--update-line marker)))

(defun aq-agenda-deadline (arg)
  "Set a deadline on the Org entry linked to the current vtable row.
With prefix ARG, remove the deadline."
  (interactive "P")
  (let ((marker (aq-agenda--marker-or-error)))
    (org-with-remote-undo (marker-buffer marker)
      (let (ts)
        (with-current-buffer (marker-buffer marker)
          (widen)
          (goto-char marker)
          (setq ts (org-deadline arg)))
        (when (stringp ts)
          (aq-agenda-show-new-time marker ts " D"))))
    (aq-agenda--update-line marker)))

(defun aq-agenda-todo (&optional arg)
  "Cycle the TODO state of the Org entry linked to the current vtable row."
  (interactive "P")
  (let ((marker (aq-agenda--marker-or-error)))
    (org-with-remote-undo (marker-buffer marker)
      (with-current-buffer (marker-buffer marker)
        (widen)
        (goto-char marker)
        (org-todo arg)))
    (aq-agenda--update-line marker)))

(defun aq-agenda-set-priority (&optional arg)
  "Set the priority of the Org entry linked to the current vtable row."
  (interactive "P")
  (let ((marker (aq-agenda--marker-or-error)))
    (org-with-remote-undo (marker-buffer marker)
      (with-current-buffer (marker-buffer marker)
        (widen)
        (goto-char marker)
        (org-priority arg)))
    (aq-agenda--update-line marker)))

(defun aq-agenda-set-tags ()
  "Edit the tags of the Org entry linked to the current vtable row."
  (interactive)
  (let ((marker (aq-agenda--marker-or-error)))
    (org-with-remote-undo (marker-buffer marker)
      (with-current-buffer (marker-buffer marker)
        (widen)
        (goto-char marker)
        (org-set-tags-command)))
    (aq-agenda--update-line marker)))

(defun aq-agenda-set-effort ()
  "Set the Effort property of the Org entry linked to the current vtable row."
  (interactive)
  (let ((marker (aq-agenda--marker-or-error)))
    (org-with-remote-undo (marker-buffer marker)
      (with-current-buffer (marker-buffer marker)
        (widen)
        (goto-char marker)
        (org-set-effort)))
    (aq-agenda--update-line marker)))

(defun aq-agenda-archive ()
  "Archive the Org entry linked to the current vtable row."
  (interactive)
  (let ((marker (aq-agenda--marker-or-error)))
    (when (yes-or-no-p "Archive this entry?")
      (org-with-remote-undo (marker-buffer marker)
        (with-current-buffer (marker-buffer marker)
          (widen)
          (goto-char marker)
          (org-archive-subtree-default)))
      (when-let* ((tbl (vtable-current-table))
                  (obj (vtable-current-object)))
        (vtable-remove-object tbl obj)))))

(defun aq-agenda-kill-subtree ()
  "Cut the Org subtree linked to the current vtable row and remove from table."
  (interactive)
  (let ((marker (aq-agenda--marker-or-error)))
    (when (yes-or-no-p "Kill this Org subtree?")
      (org-with-remote-undo (marker-buffer marker)
        (with-current-buffer (marker-buffer marker)
          (widen)
          (goto-char marker)
          (org-cut-subtree)))
      (when-let* ((tbl (vtable-current-table))
                  (obj (vtable-current-object)))
        (vtable-remove-object tbl obj)))))

;;; ─── keybinding installer ────────────────────────────────────────────────────

(defun aq-agenda-install-keys ()
  "Install org-agenda-compatible keybindings buffer-locally.
Called automatically by `aq--install-standard-hooks' when `:org' is set."
  (local-set-key (kbd "I")       #'aq-agenda-clock-in)
  (local-set-key (kbd "O")       #'aq-agenda-clock-out)
  (local-set-key (kbd "C-c C-s") #'aq-agenda-schedule)
  (local-set-key (kbd "C-c C-d") #'aq-agenda-deadline)
  (local-set-key (kbd "t")       #'aq-agenda-todo)
  (local-set-key (kbd ",")       #'aq-agenda-set-priority)
  (local-set-key (kbd ":")       #'aq-agenda-set-tags)
  (local-set-key (kbd "E")       #'aq-agenda-set-effort)
  (local-set-key (kbd "$")       #'aq-agenda-archive)
  (local-set-key (kbd "C-k")     #'aq-agenda-kill-subtree))

;;; ─── :org-ql preset keyword ──────────────────────────────────────────────────

(defvar aq-org-ql-default-columns
  `((:name "TODO"
     :width 8
     :getter ,(lambda (el) (org-element-property :todo-keyword el)))
    (:name "Pri"
     :width 3
     :getter ,(lambda (el)
                (when-let (p (org-element-property :priority el))
                  (char-to-string p))))
    (:name "Headline"
     :width 55
     :getter ,(lambda (el) (org-element-property :raw-value el)))
    (:name "Scheduled"
     :width 12
     :getter ,(lambda (el)
                (when-let (s (org-element-property :scheduled el))
                  (org-timestamp-format s "%Y-%m-%d"))))
    (:name "Tags"
     :width 20
     :getter ,(lambda (el)
                (string-join (org-element-property :tags el) ":"))))
  "Default vtable columns for `:org-ql' views.")

(defvar aq-org-ql-default-actions
  `(("RET" "Go to heading"
     ,(lambda (el)
        (when-let ((m (org-element-property :org-hd-marker el)))
          (switch-to-buffer-other-window (marker-buffer m))
          (goto-char m)
          (org-reveal)))))
  "Default vtable actions for `:org-ql' views.")

(actionable-query-defview-def-keyword :org-ql (query-form)
  "Wrap an org-ql sexp as an actionable-query view.
QUERY-FORM is an unevaluated org-ql predicate sexp, e.g. (todo \"TODO\").
The preset supplies:
  :objects  — 0-arg thunk running `org-ql-select' across `org-agenda-files'
  :org      — extracts `:org-hd-marker' from each result element
  :columns  — TODO, priority, headline, scheduled, tags (see `aq-org-ql-default-columns')
  :actions  — RET opens the heading (see `aq-org-ql-default-actions')

All `aq-agenda-install-keys' commands (I, O, C-c C-s, t, …) are available
automatically because `:org' is wired up."
  `(:objects (lambda ()
               (require 'org-ql)
               (org-ql-select (org-agenda-files)
                              ',query-form
                              :action 'element-with-markers))
    :org     (lambda (el) (org-element-property :org-hd-marker el))
    :columns aq-org-ql-default-columns
    :actions aq-org-ql-default-actions))

(provide 'agenda)
;;; agenda.el ends here
