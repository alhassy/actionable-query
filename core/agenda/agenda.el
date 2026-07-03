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

(defun aq--parse-org-title-spec (spec)
  "Parse an `:org-upsert' title SPEC into (TITLE PROPS BODY).
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

(defun aq--find-org-tree-by-key (key &optional scope-marker)
  "Return a marker on the first heading matching KEY, or nil.
Matches a heading whose `:AQ-KEY:' property equals KEY (the durable join,
survives title edits) or, failing that, whose text contains KEY (the
content-join --- e.g. a Jira ID written into the heading).

Without SCOPE-MARKER the whole `org-default-notes-file' is searched.  With
SCOPE-MARKER (a live marker on a parent heading) the search is restricted to
that heading's subtree --- so a child key like `FWD-1@2026-07-03' is found only
under its own parent, never a namesake elsewhere."
  (when (and (stringp key) (not (string-empty-p key)))
    (if scope-marker
        (when (and (markerp scope-marker) (marker-buffer scope-marker))
          (org-with-point-at scope-marker
            (org-back-to-heading t)
            (catch 'found
              ;; `org-map-entries' with MATCH nil and SCOPE `tree' walks the
              ;; subtree at point (the parent heading + its descendants).
              (org-map-entries
               (lambda () (when (equal (org-entry-get (point) "AQ-KEY") key)
                            (throw 'found (point-marker))))
               nil 'tree)
              nil)))
      (when (and org-default-notes-file (file-exists-p org-default-notes-file))
        (with-current-buffer (find-file-noselect org-default-notes-file)
          (save-excursion
            (goto-char (point-min))
            (catch 'found
              ;; Pass 1 — durable `:AQ-KEY:' property match.
              (org-map-entries
               (lambda () (when (equal (org-entry-get (point) "AQ-KEY") key)
                            (throw 'found (point-marker)))))
              ;; Pass 2 — the KEY appears literally in a heading's text.
              (goto-char (point-min))
              (when (re-search-forward (concat "^\\*+ .*" (regexp-quote key)) nil t)
                (org-back-to-heading t)
                (throw 'found (point-marker)))
              nil)))))))

(defun actionable-query--create-org-tree (title props body &optional key parent-marker)
  "Create a `TODO TITLE' heading and return its marker.
PROPS is an alist of (\"NAME\" . \"VALUE\") set via `org-entry-put'; BODY, when a
non-blank string, is inserted under the heading.  KEY, when given, is stamped as
the `:AQ-KEY:' property so a later `aq--find-org-tree-by-key' re-finds this tree
even if TITLE is edited.  A `CREATED' property is always stamped.

Without PARENT-MARKER the heading is a top-level `* ' at the end of
`org-default-notes-file'.  With PARENT-MARKER (a live marker on a parent
heading) it is created as the last child of that parent --- one level deeper,
inserted after the parent's existing subtree."
  (let* ((parent-buf (and (markerp parent-marker) (marker-buffer parent-marker)))
         (buf (or parent-buf (find-file-noselect org-default-notes-file))))
    (with-current-buffer buf
      (save-excursion
        (if parent-marker
            ;; Child: go to end of the parent's subtree, open a heading one
            ;; level deeper.  `org-insert-heading' + `org-demote' keeps the
            ;; star count correct relative to the parent.
            (let (child-stars)
              (goto-char parent-marker)
              (org-back-to-heading t)
              (setq child-stars (make-string (1+ (org-current-level)) ?*))
              (org-end-of-subtree t t)
              (unless (bolp) (insert "\n"))
              (insert (format "%s TODO %s\n" child-stars title)))
          (goto-char (point-max))
          (unless (bolp) (insert "\n"))
          (insert (format "* TODO %s\n" title)))
        (when (and (stringp body) (not (string-empty-p body)))
          (insert "  " (string-trim body) "\n"))
        (org-back-to-heading t)
        (dolist (kv props) (org-entry-put (point) (car kv) (cdr kv)))
        (when (and (stringp key) (not (string-empty-p key)))
          (org-entry-put (point) "AQ-KEY" key))
        (org-entry-put (point) "CREATED" (format-time-string "[%Y-%m-%d %a %H:%M]"))
        (point-marker)))))

(cl-defun actionable-query-upsert-org-tree (&key key title-spec)
  "Return a live Org marker for a row, creating its tree if none exists (upsert).
This is the workhorse a view's `:org-upsert' lambda calls: find-or-create in
one shot.  KEY is the durable join key (e.g. a Jira ID).  First
`aq--find-org-tree-by-key' looks for an existing heading (by `:AQ-KEY:' property,
else by KEY appearing in the heading text); if found, that marker is returned and
nothing is written.  Otherwise a heading is minted from TITLE-SPEC (a STRING or
\(TITLE :K V … BODY), see `aq--parse-org-title-spec') with KEY stamped as
`:AQ-KEY:' so the next call re-finds it.  Either way a marker comes back."
  (or (aq--find-org-tree-by-key key)
      (pcase-let ((`(,title ,props ,body) (aq--parse-org-title-spec title-spec)))
        (actionable-query--create-org-tree (or title key "Untitled") props body key))))

(cl-defun actionable-query-upsert-org-child (&key parent-key parent-title-spec
                                                  child-key child-title-spec)
  "Upsert a nested CHILD tree under a PARENT tree; return the CHILD marker.
Two-level find-or-create: first `actionable-query-upsert-org-tree' finds/mints
the parent (by PARENT-KEY / PARENT-TITLE-SPEC, top level).  Then, scoped to the
parent's subtree, find a child whose `:AQ-KEY:' is CHILD-KEY; if none, create it
as a child of the parent from CHILD-TITLE-SPEC, stamped with CHILD-KEY.

The intended use (Gerrit/Jira): PARENT-KEY is the bare Jira ID (one tree per
ticket) and CHILD-KEY is `JIRA@YYYY-MM-DD' (one child per ticket per day) --- so
re-visiting the same ticket the same day re-finds the same dated child rather
than duplicating it."
  (let ((parent (actionable-query-upsert-org-tree
                 :key parent-key :title-spec parent-title-spec)))
    (or (aq--find-org-tree-by-key child-key parent)
        (pcase-let ((`(,title ,props ,body) (aq--parse-org-title-spec child-title-spec)))
          (actionable-query--create-org-tree
           (or title child-key "Untitled") props body child-key parent)))))

(defun aq-row-scheduled-on-or-after-today-p (obj org-fn)
  "Non-nil when OBJ's Org heading (resolved via ORG-FN) is SCHEDULED today or later.
ORG-FN is the view's `:org-upsert' fn (returns a marker or a content-join key);
we resolve it through `aq-org-marker-of'.  A row that is unscheduled, or scheduled
in the past (overdue), returns nil --- those still warrant attention, so a section
holding them should stay visible."
  (when-let* ((m  (aq-org-marker-of obj org-fn))
              ((markerp m))
              ((marker-buffer m))
              (ts (org-with-point-at m (org-get-scheduled-time (point)))))
    (>= (time-to-days ts) (org-today))))

(declare-function aq--ctx-at-point "state-region-ctx")
(declare-function aq-region-ctx-org-upsert "state-region-ctx")
(defvar aq--org-upsert)                 ; buffer-local, from actionable-query.el

(defun aq--row-upsert-fn ()
  "The active view's `:org-upsert' fn: the buffer-local `aq--org-upsert' in a
dedicated view buffer, else the one on the region's `aq-region-ctx' in a spliced
host buffer --- so RET / C-c C-s / I mint-or-find identically either way."
  (or (and (bound-and-true-p aq--org-upsert) aq--org-upsert)
      (when-let ((ctx (and (fboundp 'aq--ctx-at-point) (aq--ctx-at-point))))
        (aq-region-ctx-org-upsert ctx))))

(defun aq-agenda--marker-or-create ()
  "Return the Org marker for the current row, creating a tree if none exists.
A main point of actionable-query is that org-agenda-style keys work on any
row --- so a markerless row (e.g. a gcal/Gerrit item with no Org heading) is
resolved through the view's `:org-upsert' fn, which finds its existing tree or
mints a fresh one (see `actionable-query-upsert-org-tree').  The resulting
marker is stitched back onto the row so later commands act on the same heading."
  (or (aq-agenda--current-marker)
      (let* ((obj    (get-text-property (line-beginning-position) 'vtable-object))
             (upsert (aq--row-upsert-fn))
             ;; `:org-upsert' may return a marker (it did find-or-create itself)
             ;; or a content-join key string; `aq-org-marker-of' normalises both.
             (marker (and upsert obj (aq-org-marker-of obj upsert))))
        (unless (and (markerp marker) (marker-buffer marker))
          (user-error "No Org entry for this row (and its `:org-upsert' produced none)"))
        ;; Stitch the marker onto the row object so subsequent commands / the
        ;; org-marker line property find it without re-resolving.
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

;; NB: the `:org-ql' preset lives in `aq-org-ql.el' --- and is deliberately
;; NOT redefined here.  This file once carried a raw-`org-ql-select' variant
;; (elements, not plists) that, loaded after `aq-org-ql.el', silently clobbered
;; the plist-based preset --- so every `(plist-get o :heading)' column rendered
;; `nil'.  `aq-org-ql-default-columns'/`-default-actions' below stay only as
;; the element-shaped column set the tests still reference.

(provide 'agenda)
;;; agenda.el ends here
