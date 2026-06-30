;;; aq-interaction-filters.el --- Per-column regex filters bound to `='  -*- lexical-binding: t; -*-

;; Author: Musa Al-hassy <alhassy@gmail.com>

;;; Commentary:

;; Filters are an alist of (column-name . regex) entries.  Each `='
;; press prompts for a regex on the current column; an empty answer
;; clears that one filter; `C-u =' clears every filter in scope.
;;
;; Two scopes share the same key:
;;
;;   • Buffer scope   — `aq--active-filters' (defvar-local).
;;   • Region scope   — `(aq-region-ctx-filters ctx)' for spliced views.
;;
;; The dispatch goes through `aq--filter-prompt-and-apply', which
;; checks `aq--ctx-at-point' to pick the right slot.  Re-rendering
;; flows through `aq--refilter-single-table' for both grouped (multi-
;; vtable) and ungrouped views.

;;; Code:

(require 'cl-lib)
(require 'aq-state-cache)         ; `aq--obj-id', `aq--object-cache', `aq--total-objects'
(require 'aq-state-region-ctx)    ; `aq-region-ctx', `aq--ctx-at-point', `aq--message'
(require 'aq-state-dismissal)     ; `aq--dismissed-items', `aq--update-dismissed-footer'

(declare-function vtable-current-table   "vtable")
(declare-function vtable-current-column  "vtable")
(declare-function vtable-columns         "vtable")
(declare-function vtable-end-of-table    "vtable")
(declare-function vtable--clear-cache    "vtable")
(declare-function vtable--get-value      "vtable")
(declare-function vtable-revert          "vtable")
(declare-function aq--grouped-p          "aq-render-grouped")

(defvar-local aq--active-filters nil
  "Alist of (column-name . regex) for active column filters.")

(defvar-local aq--editable-setters nil
  "Alist of (column-index . setter-fn) populated by `aq--coerce-columns'.
Keyed by index, not name --- column `:name' is cosmetic and several
columns may share one (e.g. multiple blank \"\" labels in a one-line
layout), so name would silently clobber entries for distinct columns.
SETTER-FN is `(lambda (object new-value) …)', called by
`aq--edit-current-cell' when the column is `:editable'.")

(defvar-local aq--all-objects nil
  "Full unfiltered object list from the last async delivery; used by column filters.")

(defun aq--coerce-columns (cols)
  "Coerce COLS (strings, plists, or vtable-column structs) to vtable-column objects.
Plists may carry `:editable' and `:setter' — `vtable-column' has no such
slots, so they're stripped here before `make-vtable-column' and recorded
in `aq--editable-setters' (keyed by column index) for `aq--edit-current-cell'."
  (cl-loop for col in cols
           for idx from 0
           collect
           (cond ((stringp col) (make-vtable-column :name col))
                 ((listp col)
                  (let* ((editable (plist-get col :editable))
                         (setter   (plist-get col :setter))
                         (clean    (cl-loop for (k v) on col by #'cddr
                                            unless (memq k '(:editable :setter))
                                            append (list k v))))
                    (when editable
                      (setf (alist-get idx aq--editable-setters nil nil #'eql) setter))
                    (apply #'make-vtable-column clean)))
                 (t col))))

(defun aq--apply-filters (objects filters columns)
  "Return OBJECTS filtered by FILTERS, an alist of (column-name . regex).
COLUMNS is the vtable column list (vtable-column structs)."
  (if (null filters)
      objects
    (cl-remove-if-not
     (lambda (obj)
       (cl-every
        (lambda (filter)
          (let* ((col-name (car filter))
                 (regex    (cdr filter))
                 (col      (cl-find col-name columns
                                    :key #'vtable-column-name
                                    :test #'string=))
                 (idx      (and col (cl-position col columns)))
                 ;; vtable--get-value is internal; fall back to getter if absent.
                 (val      (when idx
                             (if (fboundp 'vtable--get-value)
                                 (vtable--get-value obj idx col nil)
                               (funcall (vtable-column-getter col) obj idx nil)))))
            (or (null val)
                (string-match-p regex (format "%s" val)))))
        filters))
     objects)))

(defun aq--refilter-single-table (table objects dismissed filters)
  "Apply FILTERS + DISMISSED to OBJECTS, update TABLE, and revert it.
Returns the filtered list."
  (let ((filtered (aq--apply-filters
                   (cl-remove-if (lambda (o) (member (aq--obj-id o) dismissed))
                                 objects)
                   filters
                   (vtable-columns table))))
    (setf (vtable-objects table) filtered)
    (vtable--clear-cache table)
    (ignore-errors (vtable-revert))
    filtered))

(defun aq--apply-filter-to-view (view-name &optional ctx)
  "Re-render the table(s) for VIEW-NAME honouring its active filters.

When CTX (an `aq-region-ctx') is supplied, operates only on the
single vtable inside `(begin .. end)` of CTX and reads filters from
`(aq-region-ctx-filters ctx)'.  In that mode the buffer-wide scan
for grouped views is skipped — spliced regions only ever carry one
vtable today.

Without CTX, falls back to the legacy buffer-wide path that reads
the buffer-local `aq--active-filters' — this keeps the dedicated-
buffer path identical to before."
  (let* ((dismissed (aq--dismissed-items view-name))
         (raw       (gethash view-name aq--object-cache))
         (filters   (if ctx (aq-region-ctx-filters ctx) aq--active-filters))
         (inhibit-read-only t))
    (if ctx
        ;; — Region path: one vtable inside `(begin .. end)'. ────────────
        ;; aq--total-objects is nil in a host buffer; skip the dismissed footer.
        (save-excursion
          (goto-char (aq-region-ctx-begin ctx))
          (when-let* ((table    (vtable-current-table))
                      ;; In a host buffer aq--all-objects is nil; fall back to cache.
                      (src      (or aq--all-objects raw))
                      (filtered (aq--refilter-single-table table src dismissed filters)))
            (when filters
              (aq--message "Filtered down to %d result%s"
                           (length filtered)
                           (if (= (length filtered) 1) "" "s")))))
      ;; — Buffer path: legacy behaviour. ───────────────────────────────
      (cond
       ((aq--grouped-p raw)
        (let* ((alist  (cl-loop for (k v) on raw by #'cddr collect (cons k v)))
               (tables (save-excursion
                         (goto-char (point-min))
                         (let (acc)
                           (while (not (eobp))
                             (if-let ((tbl (vtable-current-table)))
                                 (progn (push tbl acc) (vtable-end-of-table))
                               (forward-char 1)))
                           (nreverse acc))))
               (total  0))
          (cl-mapc
           (lambda (group tbl)
             (let ((filtered (aq--refilter-single-table tbl (cdr group) dismissed filters)))
               (setq total (+ total (length filtered)))))
           alist tables)
          (when filters
            (aq--message "Filtered down to %d result%s" total (if (= total 1) "" "s")))))
       (t
        (let* ((table    (vtable-current-table))
               (filtered (aq--refilter-single-table table aq--all-objects dismissed filters)))
          (when filters
            (aq--message "Filtered down to %d result%s"
                         (length filtered)
                         (if (= (length filtered) 1) "" "s"))))))
      (aq--update-dismissed-footer view-name nil aq--total-objects))))

(defun aq--filter-prompt-and-apply (view-name &optional clear-arg)
  "Shared body for `=' filter binding — works in both region and buffer scope.
When point sits inside an `aq-region-ctx', filters are read/written on the
ctx; otherwise the buffer-local `aq--active-filters'.  CLEAR-ARG non-nil
clears all filters in the current scope (i.e. `C-u =')."
  (let* ((ctx   (aq--ctx-at-point))
         (table (vtable-current-table))
         (cols  (vtable-columns table)))
    (if clear-arg
        (progn
          (if ctx
              (setf (aq-region-ctx-filters ctx) nil)
            (setq aq--active-filters nil))
          (aq--apply-filter-to-view view-name ctx))
      (let* ((col     (ignore-errors (vtable-current-column)))
             (colname (if (vtable-column-p col)
                          (vtable-column-name col)
                        (vtable-column-name (car cols))))
             (regex   (read-string (format "Filter %s by regex (empty = clear): "
                                           colname))))
        (cond
         (ctx
          (if (string-empty-p regex)
              (setf (aq-region-ctx-filters ctx)
                    (assoc-delete-all colname (aq-region-ctx-filters ctx) #'string=))
            (setf (alist-get colname (aq-region-ctx-filters ctx) nil nil #'string=)
                  regex)))
         (t
          (if (string-empty-p regex)
              (setq aq--active-filters
                    (assoc-delete-all colname aq--active-filters #'string=))
            (setf (alist-get colname aq--active-filters nil nil #'string=) regex))))
        (aq--apply-filter-to-view view-name ctx)))))

(defun aq--install-column-filter (view-name)
  "Bind `=' to filter the current column by regex, `C-u =' to clear all filters.
The binding dispatches via `aq--filter-prompt-and-apply', which honours any
`aq-region-ctx' under point — same key, region- or buffer-scoped."
  (local-set-key
   (kbd "=")
   (lambda (arg)
     (interactive "P")
     (aq--filter-prompt-and-apply view-name arg))))

(provide 'aq-interaction-filters)
;;; aq-interaction-filters.el ends here
