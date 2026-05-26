;;; actionable-query.el --- Turn any query into an interactive, actionable view  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Musa Al-hassy

;; Author: Musa Al-hassy <alhassy@gmail.com>
;; Version: 0.1
;; Package-Requires: ((emacs "29.1") (org-ql "0.8") (transient "0.5"))
;; Keywords: org, vtable, dashboard, rss, convenience
;; URL: https://github.com/alhassy/actionable-query

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; `actionable-query' turns any data source — shell output, HTTP
;; responses, RSS feeds, bookmark lists, curated static lists, JSON
;; payloads, you name it — into an interactive, actionable vtable view
;; (that is also registered in `org-ql-views').

;; Think of it as `dired' for files and `proced' for processes, but
;; generalised: define a query, render its results as a vtable, and bind
;; your own actions.  The `actionable-query-defview' macro is the entry
;; point — see its docstring for full keyword reference.

;; `vtable' renders a table.  It doesn't answer: how do I fetch data
;; asynchronously without freezing Emacs?  How do I let users snooze rows
;; and have that survive a session restart?  How do I surface all my
;; keybindings discoverably — not buried in `:actions' docs?  How do I
;; group results from multiple async sources into one buffer?  How do I
;; wire a row to an Org heading so RET/TAB/I/o just work?
;;
;; `actionable-query' is the ergonomic layer on top of `vtable' that
;; answers all of these — the same way `dired' is to raw file I/O and
;; `proced' is to raw process listings.  Think of it as "`dired' for
;; anything": define a data source, and get an interactive, named,
;; session-aware dashboard for free.
;;
;; See README.org for a tour of worked examples (RSS feeds, YouTube
;; favourites, git-log standups, `gh search' CLI output, bookmark
;; dashboards, JSON+hierarchy.el, Gnus, gcalcli).
;;
;; `actionable-query-defview' is the entry point — one macro that registers
;; a named view in `org-ql-views', openable anytime with `M-x org-ql-view'.
;;
;; Usage (static → sync → async):
;;
;;   ;; Static list — simplest possible pattern.
;;   (actionable-query-defview "My awesome view"
;;     :objects '("Item 1" "Item 2" "Item 3")
;;     :actions '(("RET" "Greet" (lambda (it) (message-box "You're looking at %s" it))))
;;     :help-echo (lambda (it) (format "Are you really going to do %s today?" it)))
;;
;;   ;; Async — 1-arg :objects shows ⏳ until the callback fires; `g' re-fetches.
;;   (actionable-query-defview "Hacker News"
;;     :auto-refresh "30 minutes"
;;     :objects (lambda (cb) (aq--fetch-rss "https://news.ycombinator.com/rss" cb))
;;     :actions '(("RET" "Open in browser" (lambda (o) (browse-url (plist-get o :url))))))
;;
;; Batteries included — everything below comes for free:
;;
;; Async & live data
;;   `:objects' 1-arg lambda    → ⏳ spinner on open; `g' re-fires the fetch.
;;   Grouped delivery           → callback delivers ("Group A" objects-A "Group B" objects-B …);
;;                                multiple titled vtables render in one buffer automatically.
;;   `:auto-refresh "N units"'  → repeating timer (minutes / hours / days), no boilerplate.
;;   `aq--fetch-rss' / `aq--fetch-atom'
;;                              → one call fetches and parses; returns normalised plists
;;                                (:title :url :date :description :categories).
;;   `aq--cli-async'            → async shell command with a custom stdout parser.
;;
;; Persistence (saved across sessions via `savehist')
;;   `:snooze-period'           → dismissed rows re-appear after tomorrow / next-week / forever;
;;                                `R' resurrects all snoozed items.
;;   Hearting (`h' / `H')      → per-view favourites; `H' toggles a hearted-only filter.
;;
;; Discoverability & interaction
;;   `?'                        → transient popup listing every action with its description.
;;   `m' / `U' / `B'           → bulk-mark rows, then apply any action to all of them.
;;   `M-↑' / `M-↓'            → reorder rows manually.
;;   `='                        → filter any column by regex; `C-u =' clears all filters.
;;   `:help-echo (lambda (row) …)'
;;                              → cursor movement messages the return value.
;;
;; Org integration
;;   `:org (lambda (row) …)'   → returning a live Org marker wires RET/TAB/I/o to that
;;                                heading; standard `org-agenda-mode-map' nav just works.
;;
;; DRY for families of similar views
;;   `actionable-query-defview-def-keyword'
;;                              → register a preset keyword (e.g. `:gerrit-query') that
;;                                expands to a bundle of default kwargs; explicit call-site
;;                                keys always override.
;;
;; The implementation is split into self-contained modules, all loaded
;; transitively from this file.  See the (require ...) chain below for
;; the dependency DAG, and the README for a per-folder description:
;;
;;   render/      — vtable advice, grouped layout, host-buffer splice.
;;   data/        — RSS/Atom/CLI fetchers.
;;   state/       — region-ctx struct, async caches, snooze + hearts, spinner.
;;   interaction/ — keymaps, row-reorder, popup, bulk, filters, help-echo.

;;; Code:

;;; ─── self-locating sub-module load-path ─────────────────────────────────────
;;
;; Every module lives under one of `render/', `data/', `state/', or
;; `interaction/'.  Add each to `load-path' (relative to *this* file's
;; directory) so the `(require ...)` chain below resolves regardless
;; of how the package was installed — direct `load-file' from init.org,
;; `package.el', `straight.el', whatever.

(let ((root (file-name-directory (or load-file-name buffer-file-name))))
  (dolist (sub '("render" "data" "state" "interaction"))
    (add-to-list 'load-path (expand-file-name sub root))))

(require 'cl-lib)
(require 'org-ql-view)
(require 'vtable)
(require 'transient)

;; Sub-modules — load order follows the acyclic dependency DAG.
;; Leaves first; orchestration last.
(require 'aq-render-vtable-patches)    ; advice on vtable--insert-header-line
(require 'aq-data-rss)                 ; RSS/Atom + CLI async helpers
(require 'aq-state-region-ctx)         ; aq-region-ctx struct, aq--message
(require 'aq-state-cache)              ; async object cache, aq--obj-id
(require 'aq-state-dismissal)          ; snooze + heart + footers
(require 'aq-state-loading)            ; spinner + auto-refresh timer
(require 'aq-interaction-row-reorder)  ; M-<up> / M-<down>
(require 'aq-interaction-help-echo)    ; help-echo + org-marker hooks
(require 'aq-interaction-bulk)         ; mark / unmark / bulk transient
(require 'aq-interaction-popup)        ; transient `?' popup
(require 'aq-interaction-filters)      ; `=' column filters
(require 'aq-interaction-keys)         ; vtable + region keymaps
(require 'aq-render-grouped)           ; multi-table grouped layout
(require 'aq-render-splice)            ; splice into host buffer

;;; ─── C-x C-e: eval actionable-query-defview and open the view immediately ──

(defun aq--eval-last-sexp (orig-fn &rest args)
  "Around `eval-last-sexp': if the form is `actionable-query-defview', open the view after eval."
  (let (view-name)
    (save-excursion
      (backward-sexp)
      (when-let* ((form (sexp-at-point))
                  (_    (eq (car-safe form) 'actionable-query-defview))
                  (head (cadr form)))
        (setq view-name
              (cond
               ;; (defview SYM "Title" …) — symbol head, name is caddr
               ((and (symbolp head) (not (keywordp head))) (caddr form))
               ;; (defview "Title" …) — string head
               ((stringp head) head)
               ;; (defview :keyword …) — anonymous, no view to open
               (t nil)))))
    (apply orig-fn args)
    (when (stringp view-name)
      (org-ql-view view-name))))

(advice-add #'eval-last-sexp :around #'aq--eval-last-sexp)

;;; ─── action augmentation ────────────────────────────────────────────────────

(defun aq--ensure-action (actions key label fn)
  "Prepend (KEY LABEL FN) to ACTIONS unless KEY is already present."
  (if (cl-find key actions :key #'car :test #'string=)
      actions
    (cons (list key label fn) actions)))

(defun aq--augment-actions (actions view-name snooze)
  "Return ACTIONS with the standard `r' snooze and `w' copy entries
prepended when absent.  VIEW-NAME is passed through for the snooze
dismiss/undismiss machinery; SNOOZE is the resolved snooze-period
symbol (e.g., `tomorrow')."
  (aq--ensure-action
   (aq--ensure-action
    actions
    "r"
    (format "Snooze %s" (aq--snooze-label snooze))
    (lambda (o)
      (aq--dismiss view-name (aq--obj-id o) snooze)
      (vtable-remove-object (vtable-current-table) o)
      (aq--message "Snoozed %s!" (aq--snooze-label snooze))))
   "w"
   "Copy to kill-ring"
   (lambda (o)
     (let ((s (format "%s" o)))
       (kill-new s)
       (aq--message "Copied: %s" s)))))

(defun aq--default-help-echo (o)
  "Generic `:help-echo' fallback: format object as a string, or nil when empty."
  (let ((s (format "%s" o)))
    (unless (string-empty-p s) s)))

;;; ─── async deliver glue ────────────────────────────────────────────────────

(defmacro aq--with-agenda-mode (&rest body)
  "Erase buffer, enter `org-agenda-mode', restore actionable-query buffer-locals, run BODY.
`org-agenda-mode' wipes buffer-local variables; this macro saves and restores
the six AQ locals that must survive the call."
  (declare (indent 0))
  `(let ((--refresh-fn  aq--refresh-fn)
         (--start-time  aq--last-fetch-start-time)
         (--all         aq--all-objects)
         (--total       aq--total-objects)
         (--post-hook   aq--post-deliver-hook)
         (--hearted-p   aq--show-hearted-only))
     (erase-buffer)
     (org-agenda-mode)
     (setq aq--refresh-fn            --refresh-fn
           aq--last-fetch-start-time --start-time
           aq--all-objects           --all
           aq--total-objects         --total
           aq--post-deliver-hook     --post-hook
           aq--show-hearted-only     --hearted-p)
     ,@body))

(defun aq--make-deliver (buf view-name actions snooze
                                real-columns group-by-fn
                                prose-thunk help-echo-fn
                                rest-kwargs-sans-columns
                                has-prose-bottom
                                org-fn)
  "Return the DELIVER callback used by async `:objects' fns.
Drops the loading spinner, optionally groups via GROUP-BY-FN,
applies snooze + filters, and either re-renders an existing vtable
(refresh path) or builds one from scratch (first-delivery path).
PROSE-THUNK, if non-nil, is funcalled at prose insertion points.
HAS-PROSE-BOTTOM suppresses the \"Last fetched at …\" footer.
ORG-FN, if non-nil, is passed to `aq--install-org-marker' after grouped render."
  (lambda (raw-objects)
    (unless (and (buffer-live-p buf)
                 (buffer-local-value 'aq--fetch-aborted buf))
      (aq--stop-loading buf)
      (when (buffer-live-p buf)
      (with-current-buffer buf
        ;; Apply :group-by if supplied and result is flat.
        (when (and group-by-fn
                   (listp raw-objects)
                   (not (aq--grouped-p raw-objects)))
          (let ((groups (make-hash-table :test #'equal)))
            (dolist (o raw-objects)
              (push o (gethash (funcall group-by-fn o) groups)))
            (setq raw-objects
                  (cl-loop for k being the hash-keys of groups
                           using (hash-values v)
                           append (list k (nreverse v))))))
        (let* ((inhibit-read-only t)
               (grouped     (aq--grouped-p raw-objects))
               (groups-alist (when grouped (cl-loop for (k v) on raw-objects by #'cddr collect (cons k v)))))
          (if grouped
              (setq aq--all-objects   (apply #'append (mapcar #'cdr groups-alist))
                    aq--total-objects (length aq--all-objects))
            (setq aq--all-objects   raw-objects
                  aq--total-objects (length raw-objects)))
          (if grouped
              (let ((groups groups-alist)
                    (cols   (aq--coerce-columns real-columns)))
                (aq--with-agenda-mode
                  (when prose-thunk (funcall prose-thunk))
                  (aq--render-grouped
                   groups cols actions
                   rest-kwargs-sans-columns
                   view-name)
                  (aq--install-help-echo
                   (or help-echo-fn #'aq--default-help-echo))
                  (when org-fn (aq--install-org-marker org-fn))))
            (let* ((dismissed    (aq--dismissed-items view-name))
                   (coerced-cols (when real-columns (aq--coerce-columns real-columns)))
                   (table        (vtable-current-table))
                   (objects
                    (aq--apply-filters
                     (cl-remove-if
                      (lambda (o) (member (aq--obj-id o) dismissed))
                      raw-objects)
                     aq--active-filters
                     coerced-cols)))
              (if table
                  ;; Re-render into existing vtable (e.g. `g' refresh).
                  (progn
                    (when coerced-cols
                      (setf (vtable-columns table)
                            (vtable--compute-columns
                             (progn
                               (setf (vtable-columns table) coerced-cols)
                               table))))
                    (setf (vtable-objects table) objects)
                    (vtable--clear-cache table)
                    (vtable-revert))
                ;; org-agenda-mode resets buffer-locals — the macro saves and restores them.
                (aq--with-agenda-mode)
                (when prose-thunk (funcall prose-thunk))
                (apply #'make-vtable
                       :objects objects
                       :actions (aq--actions->vtable actions)
                       :keymap aq--vtable-keymap
                       (append
                        (when coerced-cols
                          (list :columns coerced-cols))
                        rest-kwargs-sans-columns)))
              ;; Land on the first row only on first delivery — `g' / `R'
              ;; / auto-refresh come back through here with `table' bound,
              ;; and we want point to stay where the user left it.
              (unless table
                (aq--goto-first-row))))
          (let* ((now     (float-time))
                 (elapsed (when aq--last-fetch-start-time
                            (- now aq--last-fetch-start-time))))
            (setq aq--last-fetch-time       now
                  aq--last-fetch-start-time nil)
            (when elapsed
              (puthash view-name elapsed aq--last-elapsed-cache))
            (puthash view-name raw-objects aq--object-cache)
            (aq--update-dismissed-footer
             view-name snooze aq--total-objects)
            (unless has-prose-bottom
              (aq--upsert-footer
               'actionable-query-fetch-footer
               (format "Last fetched at %s%s — press `g' to refresh."
                       (format-time-string "%-I:%M:%S%p")
                       (if elapsed
                           (format " (took %s)" (aq--format-elapsed elapsed))
                         ""))
               '(:height 0.8 :foreground "gray50"))))
            (mapc #'funcall aq--post-deliver-hook)))))))

(defvar-local aq--fetch-aborted nil
  "Non-nil when the user pressed Q to abort the current in-flight fetch.
The deliver callback checks this flag and no-ops if set; reset on next `g'.")

(defun aq-abort-fetch ()
  "Abort the in-flight async fetch for this view.
The spinner stops immediately; the deliver callback will silently no-op
when it eventually fires.  Press `g' to retry."
  (interactive)
  (setq aq--fetch-aborted t)
  (aq--stop-loading (current-buffer))
  (let ((inhibit-read-only t))
    (erase-buffer)
    (insert (aq--center-message
             (propertize "Fetch aborted — press g to retry."
                         'face '(:height 0.9 :foreground "gray50"))))))

(defun aq--install-standard-hooks (view-name actions help-echo-fn &optional org-fn)
  "Run the standard post-render install sequence for a actionable-query view.
Installs row-reorder, column-filter, popup, bulk, help-echo, and
(when ORG-FN is non-nil) org-marker hooks and agenda keys into the current buffer.
HELP-ECHO-FN falls back to `aq--default-help-echo' when nil."
  (aq--install-row-reorder)
  (aq--install-column-filter view-name)
  (aq--install-popup actions)
  (aq--install-bulk actions)
  (aq--install-help-echo (or help-echo-fn #'aq--default-help-echo))
  (local-set-key (kbd "Q") #'aq-abort-fetch)
  (when org-fn
    (aq--install-org-marker org-fn)
    (when (fboundp 'aq-agenda-install-keys)
      (aq-agenda-install-keys))))

(defun aq--goto-first-row ()
  "Move point to the first vtable data row in this buffer.
No-op when point is already on a row or when no vtable is visible.
Used to ensure help-echo fires on open even when `:prose' pushed
the table below `point-min'."
  (unless (vtable-current-object)
    (let ((pos (save-excursion
                 (goto-char (point-min))
                 (text-property-search-forward 'vtable-object nil nil))))
      (when pos (goto-char (prop-match-beginning pos))))))

;;; ─── defcustoms + fallbacks ─────────────────────────────────────────────────

(defconst aq--default-row-colors '("gray97" "white")
  "Subtle alternating row colors used when the caller omits :row-colors.")

(defcustom actionable-query-defview-default-snooze-period 'tomorrow
  "Default `:snooze-period' when a view omits it.
One of `tomorrow', `next-week', `forever'."
  :type '(choice (const tomorrow) (const next-week) (const forever))
  :group 'actionable-query)

(defcustom actionable-query-defview-default-use-header-line nil
  "Default `:use-header-line' when a view omits it."
  :type 'boolean :group 'actionable-query)

(defcustom actionable-query-defview-default-row-colors aq--default-row-colors
  "Default `:row-colors' palette when a view omits it."
  :type '(repeat string) :group 'actionable-query)

(defcustom actionable-query-auto-fetch-slow-threshold 1.0
  "Fetches slower than this many seconds suppress auto-fetch on the next visit.
The message buffer will instead prompt the user to press \\`g\\' to fetch manually.
Set to nil to always auto-fetch regardless of how long the last fetch took."
  :type '(choice (const :tag "Never suppress" nil) number)
  :group 'actionable-query)

(defconst aq--fallback-columns
  '((:name "Item" :width 80
           :getter    (lambda (o &rest _) (format "%s" o))
           :displayer (lambda (v w _)
                        (truncate-string-to-width (format "%s" v) w))))
  "Single-column spec used when a view omits `:columns'.")

;;; ─── preset keywords ────────────────────────────────────────────────────────
;;
;; `actionable-query-defview-def-keyword' lets a client register a keyword (e.g.
;; `:gerrit-query') whose use at a `actionable-query-defview' call site expands
;; to a bundled plist of default kwargs.  Explicit call-site kwargs
;; override the bundled defaults (first match wins in `plist-get').

(cl-defstruct aq--preset
  keyword docstring fn)

(defvar aq--preset-keywords (make-hash-table :test 'eq)
  "Map keyword → `aq--preset' struct.
Populated by `actionable-query-defview-def-keyword'; consulted by `actionable-query-defview'.")

(defmacro actionable-query-defview-def-keyword (keyword arglist &rest body)
  "Register KEYWORD as a `actionable-query-defview' preset.

At each `actionable-query-defview' call site that uses KEYWORD, the *form*
bound to KEYWORD (unevaluated) is funcalled through BODY with
ARGLIST's single parameter — BODY returns a plist of default
keyword/value pairs to splice into the view's expansion.  Explicit
keywords at the call site override these defaults.

KEYWORD must be a keyword symbol (its name begins with `:').
ARGLIST must bind exactly one parameter.  BODY's last form must
evaluate to a plist — typically built with backquote so that the
preset's parameter names appear inside `:objects'-style lambdas
and are resolved at view-open time."
  (declare (indent 2) (doc-string 3))
  (unless (keywordp keyword)
    (error "actionable-query-defview-def-keyword: first argument must be a keyword, got %S"
           keyword))
  (unless (and (listp arglist) (= 1 (length arglist)))
    (error "actionable-query-defview-def-keyword %S: ARGLIST must bind exactly one parameter"
           keyword))
  (let* ((docstring (and (stringp (car body)) (car body)))
         (body      (if (stringp (car body)) (cdr body) body)))
    `(puthash ',keyword
              (make-aq--preset
               :keyword   ',keyword
               :docstring ,docstring
               :fn        (lambda ,arglist ,@body))
              aq--preset-keywords)))

(defun aq--expand-presets (kwargs view-name)
  "Return KWARGS with any registered preset keywords expanded.
Each preset's single-parameter function is called with the *form*
the user wrote — not its value — so that expressions like
`#'foo' and `(list a b)' are spliced literally into the expansion
and evaluated when the view opens.
User-supplied kwargs appear first in the returned plist so they
override preset defaults (`plist-get' returns the first match)."
  (let ((defaults '()))
    (cl-loop for (k v) on kwargs by #'cddr
             for preset = (gethash k aq--preset-keywords)
             when preset
             do (let ((plist (funcall (aq--preset-fn preset) v)))
                  (unless (and (listp plist) (cl-evenp (length plist)))
                    (error "actionable-query-defview %S: preset %S returned non-plist %S"
                           view-name k plist))
                  (setq defaults (append defaults plist))))
    (let ((filtered (cl-loop for (k v) on kwargs by #'cddr
                             unless (gethash k aq--preset-keywords)
                             append (list k v))))
      (append filtered defaults))))

;;; ─── the macro ──────────────────────────────────────────────────────────────

(defmacro actionable-query-defview (name-or-sym &rest vtable-kwargs)
  "Register a vtable-based view in `org-ql-views'.

Two calling conventions are supported:

  (actionable-query-defview \"Title\" …)         ; string name only (original form)
  (actionable-query-defview my-cmd \"Title\" …)  ; symbol + string name

In the second form a command MY-CMD is defined via `defalias'.  Calling
it with no argument opens the view's dedicated buffer (same as `M-x my-cmd').
Calling it with `:insert t' inserts the cached view content at point in the
current buffer.  Calling it with `:insert \\='fetch-latest' fires a fresh fetch
and inserts the result at point once the data arrives.

VTABLE-KWARGS are keyword args forwarded verbatim to `make-vtable',
except these, which are intercepted:
  `:help-echo'     — (lambda (row) …) messaged on each cursor movement.
  `:prose'         — form evaluated and inserted before the table.
  `:prose-bottom'  — form evaluated and inserted after the table.
  `:actions'       — list of triples (\"key\" \"desc\" fn).  Each triple is
                     registered as a vtable action (key fn) AND collected
                     into a `?' transient popup with the description shown.
  `:objects'       — one of three shapes, dispatched at runtime:
                       • a plain list  → static objects, passed to `make-vtable' as-is.
                       • a 0-arg fn    → sync thunk; called on each open/refresh.
                       • a 1-arg fn    → async; called with a CALLBACK argument.
                                         Call (funcall callback objects) when data is ready.
                                         The buffer shows ⏳ Loading… until the callback fires.
                                         `g' re-fires the fetch from scratch.
                                         If callback receives (\"Title\" objects …), multiple
                                         titled vtables are rendered (grouped mode).
  `:snooze-period' — symbol: `tomorrow' (default), `next-week', `forever'.
                     Controls how long dismissed articles stay hidden.
  `:auto-refresh'  — string like \"5 minutes\", \"1 hour\", \"1 day\".
                     Installs a repeating timer to re-fetch automatically.
  `:group-by'      — (lambda (obj) string).  Partitions objects by the
                     returned string and renders one titled vtable per group.
  `:hearting'      — t to enable h/H heart-toggle and hearted-only filter.
                     Hearts are persisted via savehist.  On open, defaults to
                     hearted-only when any hearts exist for this view.
  `:org'           — (lambda (it) …) returning an org marker or nil.
                     On each cursor move, the lambda is called with the current
                     row object; when it returns a live marker, `org-marker' and
                     `org-hd-marker' are set on that line — enabling standard
                     `org-agenda-mode-map' navigation (RET, TAB, I, o) to jump
                     to the linked Org heading.

Additional keywords registered via `actionable-query-defview-def-keyword' expand
to bundled default kwargs.  An explicit keyword at the call site
overrides anything the preset would have defaulted."
  (declare (indent defun))
  (let* (;; Three legal heads:
         ;;   (defview SYM "title" …)   ← symbol + string title
         ;;   (defview "title" …)       ← string title only
         ;;   (defview …)               ← anonymous (throwaway, no `org-ql-views' entry)
         (sym           (and (symbolp name-or-sym) (not (keywordp name-or-sym)) name-or-sym))
         (name          (cond
                          (sym                        (car vtable-kwargs))
                          ((stringp name-or-sym)      name-or-sym)
                          ((keywordp name-or-sym)     nil)
                          (t (error "actionable-query-defview: head must be a symbol, string, or keyword; got %S"
                                    name-or-sym))))
         (vtable-kwargs (cond
                          (sym                        (cdr vtable-kwargs))
                          ((stringp name-or-sym)      vtable-kwargs)
                          (t                          (cons name-or-sym vtable-kwargs))))
         (actionable-query-keys        '(:help-echo :prose :prose-bottom :actions :objects
                                         :snooze-period :auto-refresh :group-by :hearting :org
                                         :async-notifier :insert))
         (vtable-keys       '(:columns :objects-function :getter :formatter :displayer
                                       :use-header-line :face :actions :keymap
                                       :separator-width :divider :divider-width
                                       :sort-by :ellipsis :row-colors :column-colors))
         ;; Validate against the *pre*-expansion plist so error
         ;; messages cite the keyword the user actually wrote.
         (_validate         (cl-loop for (k _) on vtable-kwargs by #'cddr
                                     unless (or (memq k actionable-query-keys)
                                                (memq k vtable-keys)
                                                (gethash k aq--preset-keywords))
                                     do (error "actionable-query-defview %S: unsupported keyword %s"
                                               name k)))
         ;; Now splice in any registered preset defaults.  User values
         ;; still win because `aq--expand-presets' places them first.
         (vtable-kwargs     (aq--expand-presets vtable-kwargs name))
         (help-echo-form    (plist-get vtable-kwargs :help-echo))
         (prose-form        (plist-get vtable-kwargs :prose))
         (prose-bottom-form (plist-get vtable-kwargs :prose-bottom))
         (actions-form      (plist-get vtable-kwargs :actions))
         (objects-form      (plist-get vtable-kwargs :objects))
         (snooze-form       (or (plist-get vtable-kwargs :snooze-period)
                                (list 'quote actionable-query-defview-default-snooze-period)))
         (refresh-form      (plist-get vtable-kwargs :auto-refresh))
         (group-by-form     (plist-get vtable-kwargs :group-by))
         (hearting-form     (plist-get vtable-kwargs :hearting))
         (org-form          (plist-get vtable-kwargs :org))
         (notifier-form     (plist-get vtable-kwargs :async-notifier))
         (insert-form       (plist-get vtable-kwargs :insert))
         (rest-kwargs       (let ((rk (cl-loop for (k v) on vtable-kwargs by #'cddr
                                               unless (memq k actionable-query-keys)
                                               append (list k v))))
                              (unless (plist-member rk :columns)
                                (setq rk (append (list :columns
                                                       (list 'quote aq--fallback-columns))
                                                 rk)))
                              (unless (plist-member rk :use-header-line)
                                (setq rk (append (list :use-header-line
                                                       actionable-query-defview-default-use-header-line)
                                                 rk)))
                              (unless (plist-member rk :row-colors)
                                (setq rk (append '(:row-colors actionable-query-defview-default-row-colors) rk)))
                              rk)))
    (ignore _validate)
    `(let ((view-fn
            (lambda (&rest call-kwargs)
              (interactive)
              (let* ((insert-mode    (plist-get call-kwargs :insert))
                     (help-echo-fn   (or (plist-get call-kwargs :help-echo)      ,help-echo-form))
                     (actions-spec   (or (plist-get call-kwargs :actions)        ,actions-form))
                     (objects-spec   (or (plist-get call-kwargs :objects)        ,objects-form))
                     (group-by-fn    (or (plist-get call-kwargs :group-by)       ,group-by-form))
                     (org-fn         (or (plist-get call-kwargs :org)            ,(when org-form `(lambda (it) ,org-form))))
                     (notifier-fn    (or (plist-get call-kwargs :async-notifier) ,(when notifier-form `(lambda () ,notifier-form))))
                     (insert-target (and insert-mode (current-buffer)))
                     ;; Use a marker so subsequent inserts (later sections of a
                     ;; composite view, async deliveries from earlier sections)
                     ;; don't leave this position pointing inside freshly
                     ;; spliced content.  An integer would silently rot.
                     (insert-pos    (and insert-mode (point-marker)))
                     (bufname   ,(if name
                                     `(format "%s%s*" org-ql-view-buffer-name-prefix ,name)
                                   `"*aq-anon*"))
                     (buf       ,(if name `(get-buffer-create bufname) `(generate-new-buffer bufname)))
                     (snooze    (or (plist-get call-kwargs :snooze-period) ,snooze-form))
                     (actions   (aq--augment-actions actions-spec ,name snooze))
                     (obj-spec  objects-spec)
                     (async-fn  (and (functionp obj-spec)
                                     (let ((max (cdr (func-arity obj-spec))))
                                       (when (or (eq max 'many) (>= max 1))
                                         obj-spec))))
                     ;; When the call site asks to splice an async view, paint a
                     ;; ⏳ placeholder in the host *now* so the slot is reserved
                     ;; in source order.  Without this, every section's prose
                     ;; lands first and the vtables clump at the end as their
                     ;; deliveries race.  The placeholder owns marker pair that
                     ;; `aq--resolve-placeholder' splices into on deliver.
                     (placeholder (when (and insert-mode insert-target async-fn)
                                    (aq--insert-pending-placeholder
                                     insert-target
                                     :label (format "fetching %s…"
                                                    ,(or name "view"))))))
               (with-current-buffer buf
                 (setq aq--marked-rows   nil
                       aq--active-filters nil
                       aq--all-objects    nil
                       aq--total-objects  nil)
                 (aq--clear-mark-overlays)
                 (let ((inhibit-read-only t))
                   (erase-buffer)
                   (org-agenda-mode)
                   ,@(when prose-form `(,prose-form))
                   (cond
                    (async-fn
                     ;; Skip the view-buffer hourglass when a host placeholder is
                     ;; in play — the spinner is rendered in the host instead,
                     ;; and the view buffer is purely a staging area whose
                     ;; loading UI the user never sees.
                     (unless placeholder
                       (aq--show-loading buf)))
                    ;; Composite views (e.g. `oag-morning-standup') pass
                    ;; `:objects '()' to opt out of vtable rendering — they
                    ;; orchestrate child views in `:prose' instead.  Without
                    ;; this guard, an empty "Item" header gets dangled at the
                    ;; bottom of every composite buffer.
                    ((and (not (functionp obj-spec)) (null obj-spec))
                     nil)
                    (t
                     (make-vtable
                      :objects (if (functionp obj-spec) (funcall obj-spec) obj-spec)
                      :actions (aq--actions->vtable actions)
                      :keymap aq--vtable-keymap
                      ,@rest-kwargs)))
                   ,@(when prose-bottom-form
                       `((goto-char (point-max)) (insert "\n\n") ,prose-bottom-form)))
                 (setq-local org-ql-view-title ,name)
                 (aq--install-standard-hooks ,name actions help-echo-fn org-fn)
                 (aq--goto-first-row)
                 (when async-fn
                   (let* ((real-columns ,(plist-get rest-kwargs :columns))
                          (deliver
                           (aq--make-deliver
                            buf ,name actions snooze
                            real-columns group-by-fn
                            ,(when prose-form `(lambda () ,prose-form))
                            help-echo-fn
                            (list ,@(cl-loop for (k v) on rest-kwargs by #'cddr
                                             unless (memq k '(:columns))
                                             append (list k v)))
                            ,(if prose-bottom-form t nil)
                            org-fn)))
                     ,@(when hearting-form
                         `((unless aq--all-objects
                             (when (aq--hearted-ids ,name)
                               (setq aq--show-hearted-only t)))
                           (setq aq--post-deliver-hook
                                 (list (lambda ()
                                         (aq--install-hearting
                                          ,name (lambda () aq--all-objects))
                                         (when aq--show-hearted-only
                                           (when-let ((tbl (vtable-current-table)))
                                             (setf (vtable-objects tbl)
                                                   (cl-remove-if-not
                                                    (lambda (o) (aq--heart-p ,name o))
                                                    aq--all-objects))
                                             (vtable--clear-cache tbl)
                                             (vtable-revert)))
                                         (aq--update-heart-footer ,name))))))
                     (when notifier-fn
                       (add-hook 'aq--post-deliver-hook notifier-fn nil :local))
                     ;; Install the splice hook BEFORE cached delivery so the
                     ;; one-shot fires even when data is already in cache.
                     ;; Without this, composite views (e.g. oag-morning-standup)
                     ;; silently render empty sections on every call after the
                     ;; first.  Markers in the target buffer absorb subsequent
                     ;; inserts, so multiple sections splice into the right
                     ;; slots even when their delivers interleave.
                     (when placeholder
                       (aq--insert-view-on-deliver-with-placeholder
                        buf placeholder
                        :help-echo-fn help-echo-fn
                        :view-name    ,name
                        :actions      actions
                        :async-fn     async-fn))
                     (when-let ((cached (gethash ,name aq--object-cache)))
                       (funcall deliver cached))
                     (let* ((threshold  actionable-query-auto-fetch-slow-threshold)
                            (last-secs  (gethash ,name aq--last-elapsed-cache))
                            ;; `'fetch-latest' is an explicit demand for
                            ;; fresh data — typically from a composite view
                            ;; whose own `g' key is not reachable.  Bypass
                            ;; the slow-fetch gate in that case; the user
                            ;; already opted in by opening the composite.
                            (too-slow-p (and threshold last-secs
                                             (> last-secs threshold)
                                             (not (eq insert-mode 'fetch-latest)))))
                       (if too-slow-p
                           (message "This view took %s last time, so not auto-fetched; press `g' to fetch latest."
                                    (aq--format-elapsed last-secs))
                         (setq aq--fetch-aborted nil
                               aq--last-fetch-start-time (float-time))
                         (funcall async-fn deliver)))
                     ,(when refresh-form
                        `(unless aq--auto-refresh-timer
                           (let ((interval (aq--parse-refresh-interval ,refresh-form)))
                             (when interval
                               (setq aq--auto-refresh-timer
                                     (aq--setup-refresh-timer
                                      buf async-fn deliver interval))))))
                     (setq aq--refresh-fn
                           (lambda ()
                             ,(when refresh-form
                                `(when aq--auto-refresh-timer
                                   (cancel-timer aq--auto-refresh-timer)
                                   (setq aq--auto-refresh-timer nil)))
                             (setq aq--fetch-aborted nil)
                             (aq--show-loading buf)
                             (setq aq--last-fetch-start-time (float-time))
                             (funcall async-fn deliver)
                             ,(when refresh-form
                                `(let ((interval (aq--parse-refresh-interval ,refresh-form)))
                                   (when interval
                                     (setq aq--auto-refresh-timer
                                           (aq--setup-refresh-timer
                                            buf async-fn deliver interval)))))))
                     (local-set-key (kbd "g") #'actionable-query-refresh-current-view)
                     (local-set-key
                      (kbd "R")
                      (lambda ()
                        (interactive)
                        (aq--undismiss-all ,name)
                        (aq--show-loading buf)
                        (setq aq--last-fetch-start-time (float-time))
                        (funcall async-fn deliver)
                        (message "Snoozed items resurrected."))))
                   (unless async-fn
                     (setq aq--refresh-fn
                           (lambda ()
                             (vtable-revert-command)
                             (aq--update-dismissed-footer ,name nil nil)))
                     (local-set-key (kbd "g") #'actionable-query-refresh-current-view)
                     (local-set-key
                      (kbd "R")
                      (lambda ()
                        (interactive)
                        (aq--undismiss-all ,name)
                        (vtable-revert-command)
                        (aq--update-dismissed-footer ,name nil nil)
                        (message "Snoozed items resurrected."))))
                   (goto-char (point-min)))
                 (if (not insert-mode)
                     (pop-to-buffer buf org-ql-view-display-buffer-action)
                   ;; Async views own a `placeholder' — its resolver runs on
                   ;; deliver and splices the rendered content in via
                   ;; `aq--resolve-placeholder', so we skip the synchronous
                   ;; path here.  Without a placeholder we're in the
                   ;; non-async / cached-content branch — splice now.
                   (unless placeholder
                     (aq--splice-view-into buf insert-target insert-pos
                                           :help-echo-fn help-echo-fn
                                           :view-name    ,name
                                           :actions      actions
                                           :async-fn     async-fn))))))))
          ,@(when name
              ;; Register the view in `org-ql-views' so `M-x' completion finds
              ;; it and so `(name)' lookups across the codebase work.
              `((setf (alist-get ,name org-ql-views nil nil #'string=) view-fn)))
          ,@(when (and sym name)
              `((defalias ',sym view-fn)
                (put ',sym 'function-documentation
                     (format "Open the \"%s\" actionable-query view, or insert it at point.
Plist call form: (NAME :insert t/'fetch-latest [:help-echo \\='\\='] [:actions \\='\\='] \\='\\=')." ,name))))
          ;; `C-x C-e' on the macro form Does The Right Thing: opens the
          ;; dedicated buffer (or splices, if `:insert' is set).
          (funcall view-fn ,@(when insert-form `(:insert ,insert-form))))))

(require 'actionable-query-scheduler)
(require 'agenda)

(provide 'actionable-query)
;;; actionable-query.el ends here
