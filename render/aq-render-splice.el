;;; aq-render-splice.el --- Splice a view's rendered content into a host buffer  -*- lexical-binding: t; -*-

;; Author: Musa Al-hassy <alhassy@gmail.com>

;;; Commentary:

;; The splice machinery — what makes `(my-view t)' inside an org file
;; render the table inline rather than popping a dedicated buffer.
;;
;; Three concerns:
;;
;;   • Per-region dispatch commands — `aq-region-refresh',
;;     `aq-region-filter', `aq-region-popup', `aq-region-row-up',
;;     `aq-region-row-down'.  Each grabs `aq--ctx-at-point' and
;;     dispatches to the matching ctx-aware helper.  Bound from
;;     `aq--region-keymap' (in `interaction/keys.el').
;;
;;   • Host-buffer hook installers — `aq--install-host-help-echo'
;;     installs the post-command hook that drives `:help-echo' for
;;     spliced regions; both are buffer-locally idempotent.
;;
;;   • The splice itself — `aq--splice-view-into' takes a view buffer
;;     and inserts its rendered content (verbatim, with `face' rewritten
;;     to `font-lock-face') into the target buffer, layering
;;     `aq--region-keymap' over each row and tagging the span with an
;;     `aq-region-ctx' text-property.  `aq--insert-view-on-deliver' is
;;     the async wrapper that defers the splice to the next deliver.

;;; Code:

(require 'cl-lib)
(require 'transient)
(require 'aq-state-region-ctx)         ; `aq-region-ctx*', `aq--ctx-at-point', `aq--rerender-region', `aq--suppress-help-echo-until'
(require 'aq-state-dismissal)          ; `aq--post-deliver-hook'
(require 'aq-interaction-row-reorder)  ; `aq--move-row'
(require 'aq-interaction-bulk)         ; mark/unmark/bulk
(require 'aq-interaction-popup)        ; `aq--current-row'
(require 'aq-interaction-help-echo)    ; `aq--center-message', `aq--last-help-echo-obj'
(require 'aq-interaction-keys)         ; `aq--region-keymap', `aq--actions->region-keymap', `aq--install-host-action-keys'
(require 'aq-interaction-edit)         ; `aq--edit-current-cell-1'
(require 'point-async)                 ; generic ⏳ slot primitive
(require 'aq-state-dismissal)          ; `aq--post-deliver-hook', heart helpers, `aq--undismiss-all'

(declare-function vtable-current-object         "vtable")
(declare-function vtable-current-table          "vtable")
(declare-function vtable-objects                "vtable")
(declare-function vtable-remove-object          "vtable")
(declare-function vtable-sort-by-current-column "vtable")
(declare-function aq--filter-prompt-and-apply   "aq-interaction-filters")
(declare-function aq--edit-current-cell-1       "aq-interaction-edit")
(declare-function aq--toggle-heart              "aq-state-dismissal")
(declare-function aq--heart-p                   "aq-state-dismissal")
(declare-function aq--undismiss-all             "aq-state-dismissal")
(declare-function aq--obj-id                    "actionable-query")
(declare-function aq--obj-label                 "aq-state-cache")
(declare-function aq--install-host-standard-keys "aq-interaction-keys")
(defvar aq--editable-setters)          ; defvar-local in aq-interaction-filters

;;; ─── per-region dispatch commands ────────────────────────────────────────────
;;
;; Six bindings — `g', `=', `m', `U', `B', `M-up', `M-down', `?' — share one
;; shape: grab `aq--ctx-at-point', then dispatch to the ctx-aware helper.
;; They live on the spliced region's `keymap' text-property (composed with
;; vtable's own keymap that drives `:actions'), so they fire only when point
;; sits inside a splice — outside, the host buffer's major-mode bindings win.

(defun aq-region-refresh ()
  "Refresh the spliced view at point.  Bound to `g' inside a splice region."
  (interactive)
  (if-let ((ctx (aq--ctx-at-point)))
      (aq--rerender-region ctx)
    (user-error "Not inside a spliced view")))

(defun aq-region-filter (arg)
  "Filter the column at point by regex.  `C-u =' clears all filters in this region."
  (interactive "P")
  (if-let ((ctx (aq--ctx-at-point)))
      (aq--filter-prompt-and-apply (aq-region-ctx-view-name ctx) arg)
    (user-error "Not inside a spliced view")))

(defun aq-region-popup ()
  "Show the actions popup for the row at point.  Bound to `?'.

Mirrors `aq--install-popup' but reads its action list from the
`aq-region-ctx' under point, so co-existing splices each get their
own menu.  `aq--current-row' is `setq'-stashed (not let-bound) because
transient is asynchronous — the closures fire after this fn has
returned."
  (interactive)
  (if-let ((ctx (aq--ctx-at-point)))
      (progn
        (setq aq--current-row (vtable-current-object))
        (eval `(transient-define-prefix aq--region-transient-popup ()
                 "Actionable-query row actions"
                 ["Row Actions"
                  :class transient-column
                  ,@(cl-loop for (key desc fn) in (aq-region-ctx-actions ctx)
                             collect (let ((f fn))
                                       `(,key ,desc
                                              (lambda ()
                                                (interactive)
                                                (funcall ',f aq--current-row)))))]
                 [["Structural"
                   ("m"  "Mark for bulk action"  actionable-query-mark-row)
                   ("U"  "Unmark all"            actionable-query-unmark-all)
                   ("B"  "Bulk action"           actionable-query-bulk-action-interactive)
                   ("M-<up>"   "Move row up"     ,(lambda () (interactive) (aq--move-row -1)))
                   ("M-<down>" "Move row down"   ,(lambda () (interactive) (aq--move-row  1)))]
                  ["Vtable"
                   ("g"  "Refresh table"         aq-region-refresh)
                   ("="  "Filter column"         aq-region-filter)
                   ("S"  "Toggle sort"           vtable-sort-by-current-column)]]
                 ["" ("C-g" "Dismiss" transient-quit-one)]) t)
        (call-interactively 'aq--region-transient-popup))
    (user-error "Not inside a spliced view")))

(defun aq-region-row-up ()
  "Move row at point up.  Bound to `M-<up>' inside a splice region."
  (interactive)
  (aq--move-row -1))

(defun aq-region-row-down ()
  "Move row at point down.  Bound to `M-<down>' inside a splice region."
  (interactive)
  (aq--move-row  1))

(defun aq-region-edit-cell ()
  "Edit the cell at point in the spliced view under point.  Bound to `e'.
Reads the region's editable setters from its `aq-region-ctx' --- the
buffer-local `aq--editable-setters' the dedicated buffer relies on is
absent in a host buffer."
  (interactive)
  (if-let ((ctx (aq--ctx-at-point)))
      ;; A bare `vtable-revert' redraws only the table's own region, leaving
      ;; the spliced view as raw org text in the host buffer (and never
      ;; recomputes sibling cells off the edit).  Re-render the whole region
      ;; instead --- the same repaint every other splice command relies on.
      (aq--edit-current-cell-1 (aq-region-ctx-editable-setters ctx)
                               (lambda () (aq--rerender-region ctx)))
    (user-error "Not inside a spliced view")))

(defun aq-region-toggle-heart ()
  "Toggle the heart on the row at point, scoped to this region's view.  Bound to `h'.
When the region is showing hearted-only and the row is un-hearted, drop it
from the live table; otherwise re-render so a freshly-hearted row appears
under an active filter."
  (interactive)
  (if-let ((ctx (aq--ctx-at-point)))
      (when-let ((o (vtable-current-object)))
        (let ((now (aq--toggle-heart (aq-region-ctx-view-name ctx) o)))
          (aq--message "%s %s" (if now "❤️  Hearted:" "🩶 Un-hearted:") (aq--obj-label o))
          (if (and (aq-region-ctx-show-hearted-only ctx) (not now))
              (vtable-remove-object (vtable-current-table) o)
            (when (aq-region-ctx-show-hearted-only ctx)
              (aq--rerender-region ctx)))))
    (user-error "Not inside a spliced view")))

(defun aq-region-toggle-hearted-only ()
  "Toggle showing only hearted rows in this region.  Bound to `H'.
Filtering on prunes non-hearted rows from the live table in place;
filtering off re-renders the region to restore the full row set (the host
buffer keeps no separate all-objects store the way a dedicated view does)."
  (interactive)
  (if-let ((ctx (aq--ctx-at-point)))
      (let ((on (not (aq-region-ctx-show-hearted-only ctx)))
            (view (aq-region-ctx-view-name ctx)))
        (setf (aq-region-ctx-show-hearted-only ctx) on)
        (if (not on)
            (aq--rerender-region ctx)        ; rebuild the full set
          (let ((table (vtable-current-table)))
            (dolist (o (copy-sequence (vtable-objects table)))
              (unless (aq--heart-p view o)
                (vtable-remove-object table o)))))
        (aq--message "%s" (if on "❤️  Showing hearted only --- H to show all"
                            "Showing all rows")))
    (user-error "Not inside a spliced view")))

(defun aq-region-resurrect ()
  "Un-snooze this region's view and re-render the region in place.  Bound to `R'.
The view's own resurrect closure targets its dedicated buffer, so we instead
un-dismiss by view name and re-run the splice path (as `g' does)."
  (interactive)
  (if-let ((ctx (aq--ctx-at-point)))
      (progn
        (aq--undismiss-all (aq-region-ctx-view-name ctx))
        (aq--rerender-region ctx)
        (aq--message "Snoozed items resurrected."))
    (user-error "Not inside a spliced view")))

;;; ─── host-buffer hook installers ────────────────────────────────────────────

(defvar-local aq--host-hook-installed nil
  "Non-nil once `aq--install-host-help-echo' has armed this buffer.
Buffer-local; idempotency flag for the splice's post-command-hook
installer, so multiple `(my-view t)' calls into the same host buffer
don't pile up duplicate hooks.")

(defun aq--install-host-help-echo ()
  "Install a buffer-local `post-command-hook' that drives `:help-echo'
on spliced regions.  Idempotent — runs once per host buffer.

Reads the `aq--help-echo' and `vtable-object' text-properties at point
on every command; when both are present, calls the lambda and messages
the result.  Mirrors `aq--install-help-echo' but reads the callback
from the property rather than a buffer-local closure, so multiple
spliced views can co-exist in one host buffer with their own help-echos."
  (unless aq--host-hook-installed
    (setq aq--host-hook-installed t)
    (add-hook 'post-command-hook
              (lambda ()
                (when (> (float-time) aq--suppress-help-echo-until)
                  (when-let* ((_ (get-text-property (point) 'aq--spliced-view))
                              (fn  (get-text-property (point) 'aq--help-echo))
                              (obj (get-text-property (point) 'vtable-object)))
                    (unless (eq obj aq--last-help-echo-obj)
                      (setq aq--last-help-echo-obj obj)
                      (when-let ((msg (funcall fn obj)))
                        (message "%s" (aq--center-message msg)))))))
              nil :local)))

;;; ─── splice machinery ──────────────────────────────────────────────────────

(cl-defun aq--splice-view-into (src-buf target-buf pos
                                        &key help-echo-fn view-name actions async-fn)
  "Insert the rendered content of SRC-BUF at POS in TARGET-BUF.

The actual characters are inserted verbatim ---so on save/reload the
table survives as plain ASCII rather than evaporating to an empty
string--- but every `face' property is rewritten to `font-lock-face'
so that `org-mode''s font-lock pass leaves the row colours alone.

`:actions' survive the splice via the `keymap' text-property that
`make-vtable' installs on each row.  When VIEW-NAME is non-nil we
*also* layer `aq--region-keymap' (composed with the existing keymap)
to bring `g'/`='/`m'/`U'/`B'/`M-up'/`M-down'/`?' into the region ---
all dispatched per-region via the `aq-region-ctx' attached as a
text-property over the span.  HELP-ECHO-FN, when non-nil, is the
`:help-echo' lambda; a buffer-local post-command hook is armed in
TARGET-BUF that reads the lambda from the property at point.

Demonstrative of the lolcat trick in `fortune--insert-lolcat-button'
---we take the durability-friendly half: real characters in the
buffer, font-lock kept at bay via `font-lock-face'.

We deliberately do *not* strip any properties here ---`keymap',
`vtable-object', `display' all carry essential behaviour.

Leaves point at the end of the spliced span ---like `insert' ---so
callers composing further prose after `(view :insert t)' can chain
naturally.  No `save-excursion': when an async deliver lands while
the user has navigated elsewhere, point will jump to the splice
site.  The alternative ---vtables silently appearing off-screen---
is worse, and the cached-deliver path positively requires this
contract so interleaved `:insert' + `(insert ...)' calls compose in
source order rather than collapsing in reverse."
  (let ((content (with-current-buffer src-buf
                   (buffer-substring (point-min) (point-max)))))
    ;; 1.  Convert `face' → `font-lock-face' so jit-lock will not
    ;;     overwrite vtable's row colours during refontification.
    (let ((idx 0) (len (length content)))
      (while (< idx len)
        (let ((next (or (next-single-property-change idx 'face content) len))
              (face (get-text-property idx 'face content)))
          (when face
            (put-text-property idx next 'font-lock-face face content)
            (put-text-property idx next 'face nil content))
          (setq idx next))))
    ;; 2.  Compose the per-region keymap on top of whatever vtable
    ;;     already painted.  We build an explicit actions-map from the
    ;;     ACTIONS triples so that user keys (e.g. RET) work regardless of
    ;;     whether vtable's internal keymap survived the buffer-substring.
    ;;     actions-map sits at the top (highest priority), then
    ;;     aq--region-keymap (g/=/m/U/B/?/M-up/M-down), then vtable's own
    ;;     keymap (S/sort/etc.) from the existing property.
    (when view-name
      (let ((actions-map (aq--actions->region-keymap actions))
            (idx 0) (len (length content)))
        (while (< idx len)
          (let* ((next     (or (next-single-property-change idx 'keymap content) len))
                 (existing (get-text-property idx 'keymap content))
                 (composed (make-composed-keymap
                            actions-map
                            (if existing
                                (make-composed-keymap aq--region-keymap existing)
                              aq--region-keymap))))
            (put-text-property idx next 'keymap composed content)
            (setq idx next)))))
    ;; 3.  Tag splice + help-echo properties (legacy hook reads these).
    (put-text-property 0 (length content) 'aq--spliced-view t content)
    (when help-echo-fn
      (put-text-property 0 (length content) 'aq--help-echo help-echo-fn content))
    ;; 4.  Insert into the host, then build markers around the new
    ;;     content and attach the full `aq-region-ctx' as a property
    ;;     over the span.  Both markers are insertion-type nil so the
    ;;     region stays put under the *sibling* views that append below
    ;;     it: `begin' stays before its content, and crucially `end'
    ;;     stays at *this* view's end rather than sliding forward with
    ;;     every later insert (which would make `end' creep to point-max
    ;;     and have `aq--rerender-region' delete every view below).
    (with-current-buffer target-buf
      (goto-char pos)
      (let ((inhibit-read-only t)
            (begin (copy-marker pos))
            end)
        (insert content)
        (setq end (copy-marker (point) nil))
        (when view-name
          ;; Carry the view buffer's editable setters onto the ctx so `e'
          ;; (via `aq-region-edit-cell') can edit cells in the host buffer,
          ;; where that buffer-local would otherwise be absent.
          (let ((ctx (make-aq-region-ctx
                      :view-name        view-name
                      :begin            begin
                      :end              end
                      :actions          actions
                      :help-echo-fn     help-echo-fn
                      :async-fn         async-fn
                      :editable-setters (buffer-local-value 'aq--editable-setters src-buf))))
            (put-text-property begin end 'aq--region-ctx ctx))))
      (when help-echo-fn
        (aq--install-host-help-echo))
      ;; Standard region keys (g/=/e/t/h/…) ride a buffer-local override map
      ;; --- fontification-proof, unlike the `keymap' text-property which
      ;; org-mode strips on the first redisplay.
      (when view-name
        (aq--install-host-standard-keys))
      (when (and view-name actions)
        (aq--install-host-action-keys actions)))))

(cl-defun aq--insert-view-async (view-buf &key help-echo-fn view-name actions async-fn)
  "Reserve a slot in the current buffer; fill it when VIEW-BUF delivers.

Composes the generic `point-async' primitive with actionable-query's
post-deliver-hook plumbing.  Reserves a slot at point in the host
buffer, then pushes a one-shot onto `aq--post-deliver-hook' (in
VIEW-BUF) that, when fired, resolves the slot and splices the
rendered VIEW-BUF content with full region-ctx for
`g'/`='/`m'/etc..

Cached-deliver path works automatically: when `(funcall deliver
cached)' fires inside the same call frame, the hook runs
synchronously inside the surrounding render call, so the resolve
happens before the macro returns and the inserted vtable is
already present when the caller's next `(insert ...)' lands.

HELP-ECHO-FN, VIEW-NAME, ACTIONS, ASYNC-FN are forwarded to
`aq--splice-view-into'."
  (let ((here (point-async-reserve
               :label (format "fetching %s…" (or view-name "view")))))
    (with-current-buffer view-buf
      (letrec ((hook (lambda ()
                       ;; Resolve clears the placeholder and parks
                       ;; point at the slot in the host buffer.
                       (point-async-resolve here)
                       (aq--splice-view-into
                        view-buf (current-buffer) (point)
                        :help-echo-fn help-echo-fn
                        :view-name    view-name
                        :actions      actions
                        :async-fn     async-fn)
                       (setq aq--post-deliver-hook
                             (delq hook aq--post-deliver-hook)))))
        (push hook aq--post-deliver-hook)))))

(provide 'aq-render-splice)
;;; aq-render-splice.el ends here
