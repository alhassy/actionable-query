;;; state-region-ctx.el --- Per-region context for spliced views  -*- lexical-binding: t; -*-

;; Author: Musa Al-hassy <alhassy@gmail.com>

;;; Commentary:

;; Once a view is spliced into a host buffer (e.g. `(my-view t)' inside
;; an org file), every per-table binding — `g', `=', `m'/`U'/`B',
;; `M-up'/`M-down', `?' — needs to act on *that* table, not on whatever
;; buffer-local state happens to be set.  We carry everything the
;; helpers need as text-properties on the spliced region itself, via
;; the `aq-region-ctx' struct here.  `aq--ctx-at-point' is the single
;; accessor every dispatch command goes through.

;;; Code:

(require 'cl-lib)
(require 'org-ql-view)  ; provides `org-ql-views'

(defvar-local aq--suppress-help-echo-until 0.0
  "Float-time after which help-echo may resume messaging.
Set transiently by actions (e.g. `r') that want their confirmation to remain visible.")

(cl-defstruct aq-region-ctx
  view-name           ; string — title; key for `org-ql-views' / caches.
  begin end           ; markers spanning the region; `g' axes (begin .. end).
  actions             ; augmented actions list (used by `?' popup, `B' bulk).
  help-echo-fn        ; (lambda (obj) → string).  Mirrors `:help-echo'.
  refresh-fn          ; 0-arg closure; called by `g'.  Builds at splice time.
  async-fn            ; non-nil when the view fetches asynchronously.
  (filters nil)       ; alist (column-name . regex) — per-region.
  (marked-rows nil)   ; list of vtable row objects marked in this region.
  (mark-overlays nil) ; list of overlays giving visual mark feedback.
  (editable-setters nil)   ; alist (column-index . setter-fn); mirrors the view
                           ; buffer's `aq--editable-setters' so `e' edits cells
                           ; in a splice without that buffer-local being present.
  (show-hearted-only nil)); per-region `H' toggle; the buffer-local
                          ; `aq--show-hearted-only' is wrong when several views
                          ; share one host buffer.

(defun aq--ctx-at-point ()
  "Return the `aq-region-ctx' covering point, or nil when point is outside any."
  (get-text-property (point) 'aq--region-ctx))

(defun aq--rerender-region (ctx)
  "Refresh the spliced view CTX in place, replacing `(begin .. end)`.

Procedure: axe the old region between markers, position point at
`begin', and re-invoke the view's `org-ql-views' lambda with
`insert-mode = t'.  That lambda is what installed the original
splice — re-running it does the fetch (or hits the cache) and goes
through the same splice path, which builds a *fresh* `aq-region-ctx'
covering the new content.

`begin'/`end' are used only to delete the old region; the re-run
view-fn captures fresh markers for the new content, so this never
reuses them post-insert.  Both are insertion-type nil so a sibling
view appending below `end' does not drag it forward (which would
make `end' creep to point-max and delete every view below)."
  (let* ((view-name (aq-region-ctx-view-name ctx))
         (begin     (aq-region-ctx-begin ctx))
         (end       (aq-region-ctx-end ctx))
         (view-fn   (alist-get view-name org-ql-views nil nil #'string=)))
    (unless view-fn
      (user-error "No view registered as %S" view-name))
    (save-excursion
      (let ((inhibit-read-only t))
        (delete-region begin end))
      (goto-char begin)
      (funcall view-fn :insert t))))

(defun aq--message (fmt &rest args)
  "Display a priority message, suppressing help-echo for 3 seconds."
  (setq aq--suppress-help-echo-until (+ (float-time) 3.0))
  (apply #'message fmt args))

(provide 'state-region-ctx)
;;; state-region-ctx.el ends here
