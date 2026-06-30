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
  "Return the Org marker for the current line, or nil.
Checks the `org-hd-marker'/`org-marker' line properties first (set by
`aq--install-org-marker' on standalone views), then falls back to a
`:marker' on the row's `vtable-object' --- the splice path doesn't run
the org-marker post-command hook, but the object is always on the line."
  (or (get-text-property (line-beginning-position) 'org-hd-marker)
      (get-text-property (line-beginning-position) 'org-marker)
      (when-let* ((obj (get-text-property (line-beginning-position) 'vtable-object))
                  ((consp obj))
                  (m (plist-get obj :marker))
                  ((markerp m)))
        m)))

(defvar aq--org-serializer)             ; from actionable-query.el (buffer-local)

(defun aq--parse-org-serializer-spec (spec)
  "Parse an `:org-serializer' SPEC into (TITLE PROPS BODY).
SPEC is either a STRING (just the title) or a list
\(TITLE :K V … BODY) where the :K V pairs become PROPS (an alist of
\(\"K\" . \"V\") with stringified keys/values) and an optional trailing
non-keyword STRING becomes BODY.  Returns (TITLE PROPS BODY); PROPS and
BODY may be nil.  TITLE is nil when SPEC is malformed."
  (cond
   ((stringp spec) (list spec nil nil))
   ((consp spec)
    (let ((title (car spec))
          (rest  (cdr spec))
          props body)
      (while (keywordp (car rest))
        (push (cons (substring (symbol-name (car rest)) 1)   ; ":ZOOM" → "ZOOM"
                    (format "%s" (cadr rest)))
              props)
        (setq rest (cddr rest)))
      (when (stringp (car rest)) (setq body (car rest)))
      (list (and (stringp title) title) (nreverse props) body)))
   (t (list nil nil nil))))

(defun aq--row-serializer-spec (obj)
  "Run the view's `:org-serializer' on row OBJ, returning its raw SPEC or nil."
  (when (and (bound-and-true-p aq--org-serializer) (consp obj))
    (funcall aq--org-serializer obj)))

(defun aq-agenda--ensure-marker (obj title-string)
  "Return a live Org marker for OBJ, creating a heading if none exists.
TITLE-STRING is the fallback headline.  When the view supplied
`:org-serializer', its SPEC for OBJ drives the new tree's title,
properties, and body (see `aq--parse-org-serializer-spec') --- a calendar
view uses this to serialize the SCHEDULED time, Zoom link, attendees, etc.
The new heading lands at the end of `org-default-notes-file'."
  (let* ((table (actionable-query-resolve-org-markers
                 (list obj) (lambda (_) title-string)))
         (marker (gethash obj table))
         ;; Resolve the spec *here*, in the view buffer where the serializer
         ;; is bound (we switch buffers to create the heading below).
         (parsed (aq--parse-org-serializer-spec (aq--row-serializer-spec obj)))
         (title  (or (nth 0 parsed) title-string))
         (props  (nth 1 parsed))
         (body   (nth 2 parsed)))
    (or (and (markerp marker) (marker-buffer marker) marker)
        (with-current-buffer (find-file-noselect org-default-notes-file)
          (save-excursion
            (goto-char (point-max))
            (unless (bolp) (insert "\n"))
            ;; Headline first; a SCHEDULED/DEADLINE planning line (if the view
            ;; passes one as a property named SCHEDULED) is handled by Org's
            ;; own machinery via `org-entry-put', which places it correctly.
            (insert (format "* TODO %s\n" title))
            (when (and (stringp body) (not (string-empty-p body)))
              (insert "  " (string-trim body) "\n"))
            (org-back-to-heading t)
            (dolist (kv props)
              (org-entry-put (point) (car kv) (cdr kv)))
            (org-entry-put (point) "CREATED"
                           (format-time-string "[%Y-%m-%d %a %H:%M]"))
            (point-marker))))))

(defun aq-agenda--row-title (obj)
  "Best-effort heading text for the current row.
Resolution order:
  1. the TITLE from the view's `:org-serializer' SPEC, if any;
  2. OBJ's `:heading'/`:title'/`:subject' plist keys;
  3. the visible text of the row at point.
So a view can override how a markerless row is titled, and even a row with
no titley key still yields something to name a created heading after."
  (or (let ((title (nth 0 (aq--parse-org-serializer-spec
                           (aq--row-serializer-spec obj)))))
        (and (stringp title) (not (string-empty-p title)) title))
      (and (consp obj)
           (or (plist-get obj :heading)
               (plist-get obj :title)
               (plist-get obj :subject)))
      (let ((line (string-trim
                   (buffer-substring-no-properties (line-beginning-position)
                                                   (line-end-position)))))
        (unless (string-empty-p line) line))))

(defun aq-agenda--marker-or-create ()
  "Return the Org marker for the current row, creating a tree if none exists.
A main point of actionable-query is that org-agenda-style keys work on any
row --- so a markerless row (e.g. a gcal/Gerrit item with no Org heading)
gets a fresh `* TODO <title>' heading minted at the end of
`org-default-notes-file', and that marker is stitched back onto the row's
object so later commands act on the same heading."
  (or (aq-agenda--current-marker)
      (let* ((obj   (get-text-property (line-beginning-position) 'vtable-object))
             (title (or (aq-agenda--row-title obj)
                        (user-error "No Org entry, and no title to create one from")))
             (marker (aq-agenda--ensure-marker obj title)))
        ;; Stitch the new marker onto the row object so subsequent commands /
        ;; the org-marker line property find it without re-creating.
        (when (consp obj)
          (plist-put obj :marker marker)
          (let ((inhibit-read-only t))
            (put-text-property (line-beginning-position) (line-end-position)
                               'org-hd-marker marker)))
        marker)))

;; Backward-compatible alias: callers historically used the -or-error name,
;; which now creates-on-miss rather than signalling.
(defalias 'aq-agenda--marker-or-error #'aq-agenda--marker-or-create)

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

(defun aq-agenda--goto-marker-row (marker)
  "Move point to the row whose `:marker' points at the same heading as MARKER.
Compares by buffer + position, not `eq' --- a table re-query mints a fresh
marker for the same heading, so identity wouldn't match."
  (when (markerp marker)
    (goto-char (point-min))
    (let (found)
      (while (and (not found) (not (eobp)))
        (let* ((obj (get-text-property (line-beginning-position) 'vtable-object))
               (m   (and (consp obj) (plist-get obj :marker))))
          (if (and (markerp m)
                   (eq (marker-buffer m) (marker-buffer marker))
                   (= (marker-position m) (marker-position marker)))
              (setq found t)
            (forward-line 1))))
      found)))

(defun aq-agenda--update-line (marker)
  "Refresh the vtable after a mutation, keeping point on MARKER's row.
Uses `vtable-revert-command' (re-queries the `:objects-function') rather
than `vtable-revert' (mere redraw) so a reschedule/retag/etc. actually
re-pulls the changed data.  After the revert the row may have moved (e.g.
re-sorted by its new time), so we seek back to it by MARKER.  Binds
`inhibit-read-only' since view buffers are read-only --- otherwise the
rewrite silently no-ops."
  (when-let ((tbl (vtable-current-table)))
    (let ((inhibit-read-only t))
      (vtable--clear-cache tbl)
      (vtable-revert-command)
      (aq-agenda--goto-marker-row marker))))

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
