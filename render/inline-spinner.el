;;; inline-spinner.el --- ⏳ placeholders for async `:insert' splices  -*- lexical-binding: t; -*-

;; Author: Musa Al-hassy <alhassy@gmail.com>

;;; Commentary:

;; The composite-view problem: when a host buffer interleaves prose with
;; several `(my-view :insert t)' calls and each view's data arrives async,
;; the prose lands in order while the deliveries race — vtables end up
;; clumped at the bottom rather than under the heading they belong to.
;;
;; Solution, lifted from `~/.emacs.d/WR.org': insert a read-only ⏳
;; placeholder at point synchronously, capture (start, end) markers around
;; it, and on deliver replace the region between them with the rendered
;; vtable.  Each slot owns its own marker pair, so interleaved deliveries
;; cannot corrupt each other's region.
;;
;; Public surface:
;;
;;   • `aq--insert-pending-placeholder' — insert ⏳ at point, return plist.
;;   • `aq--resolve-placeholder'        — replace ⏳ region with rendered view.
;;   • `aq--fail-placeholder'           — replace ⏳ region with ⚠️ note.
;;   • `aq--insert-view-on-deliver-with-placeholder' — async wrapper that
;;     pushes a one-shot resolver onto `aq--post-deliver-hook'.
;;
;; The success path delegates to `aq--splice-view-into' (defined in
;; `aq-render-splice.el') so the resolved vtable carries the full
;; `aq-region-ctx' for `g'/`='/`m'/etc. — the placeholder is a *staging*
;; concern, the splice is a *content* concern, kept separate.

;;; Code:

(require 'cl-lib)
(require 'spinner)             ; mode-line animation handle
(require 'aq-state-loading)    ; `aq--hourglass-frames', `aq--format-elapsed'
(require 'aq-state-dismissal)  ; `aq--post-deliver-hook'

;; Forward declaration — `aq-render-splice' requires *us*, so we cannot
;; require it back without forming a cycle.  The resolver invokes
;; `aq--splice-view-into' at run-time; ARGLIST is `t' because the
;; byte-compiler's naive arity counter mis-handles `cl-defun's `&key',
;; spuriously warning that 11 keyword-flattened args exceed 8.
(declare-function aq--splice-view-into "aq-render-splice" t)

(defface aq--placeholder-face
  '((t :foreground "DarkOrange3" :slant italic))
  "Face for in-flight `:insert' placeholders.
Distinct from final content — placeholders are *temporary*, not
load-bearing, so visible (orange) rather than recessed.")

(defcustom aq-placeholder-deadline-seconds 300
  "Seconds after which an unresolved placeholder is replaced with a ⚠️ note.
Tuned for Gerrit/Jira fetches over corporate VPN — those routinely
take a minute or two on a cold morning.  Bump higher if you regularly
see false-failure ⚠️ notes for views that eventually succeed."
  :type 'integer
  :group 'actionable-query)

(cl-defun aq--insert-pending-placeholder (target-buf &key label)
  "Insert a read-only ⏳ placeholder at point in TARGET-BUF; return its plist.

LABEL is the human-facing description that follows the spinner glyph
(defaults to \"fetching…\").  Returns a plist with keys

  :start-marker — sticks at the beginning of the placeholder span,
  :end-marker   — sticks at the end (insertion-type nil, so deliveries
                  splicing at :start-marker do not push it forward),
  :spinner      — `spinner.el' handle, animating the mode-line,
  :glyph-timer  — repeating timer that cycles `aq--hourglass-frames'
                  in-buffer once a second,
  :deadline     — one-shot timer that fires `aq--fail-placeholder'
                  after `aq-placeholder-deadline-seconds'.

Markers and timers are owned by the caller — pass the plist to
`aq--resolve-placeholder' (success path) or `aq--fail-placeholder'
(timeout / explicit failure) to reclaim them."
  (let ((label (or label "fetching…")))
    (with-current-buffer target-buf
      (let* ((inhibit-read-only t)
             (start (copy-marker (point) nil))
             (visible (propertize
                       (format "⏳ %s" label)
                       'face 'aq--placeholder-face
                       'read-only "🔒 In-flight actionable-query fetch"
                       'front-sticky nil
                       'rear-nonsticky t
                       'aq--placeholder t))
             end sp glyph-timer deadline)
        (insert visible)
        (insert "\n")
        (setq end (copy-marker (point) nil))
        (set-marker-insertion-type end nil)
        (setq sp (spinner-create 'progress-bar t 10))
        (spinner-start sp)
        ;; In-buffer glyph cycle.  Wraps each tick in `save-excursion' so
        ;; the user's point doesn't get yanked, and `with-silent-modifications'
        ;; so the buffer's modified flag and undo list stay clean.
        (let ((frame 0))
          (setq glyph-timer
                (run-with-timer
                 1 1
                 (lambda ()
                   (when (and (buffer-live-p target-buf)
                              (marker-buffer start))
                     (with-current-buffer target-buf
                       (setq frame (% (1+ frame) (length aq--hourglass-frames)))
                       (let ((glyph (aref aq--hourglass-frames frame))
                             (inhibit-read-only t))
                         (with-silent-modifications
                           (save-excursion
                             (goto-char start)
                             (when (re-search-forward "[⏳⌛]" end t)
                               (replace-match glyph t t)
                               ;; `replace-match' strips text properties on
                               ;; the inserted glyph — re-apply them so the
                               ;; placeholder stays read-only and faced.
                               (add-text-properties
                                (match-beginning 0) (match-end 0)
                                (list 'face 'aq--placeholder-face
                                      'read-only "🔒 In-flight actionable-query fetch"
                                      'front-sticky nil
                                      'rear-nonsticky t
                                      'aq--placeholder t))))))))))))
        (let ((placeholder (list :start-marker start
                                 :end-marker end
                                 :spinner sp
                                 :glyph-timer glyph-timer
                                 :deadline nil)))
          (setq deadline
                (run-at-time
                 aq-placeholder-deadline-seconds nil
                 (lambda ()
                   (aq--fail-placeholder
                    placeholder
                    (format "%s deadline hit"
                            (aq--format-elapsed
                             aq-placeholder-deadline-seconds))))))
          (plist-put placeholder :deadline deadline)
          placeholder)))))

(defun aq--placeholder-cancel-timers (placeholder)
  "Stop the spinner and cancel both timers attached to PLACEHOLDER."
  (when-let ((sp (plist-get placeholder :spinner)))      (spinner-stop sp))
  (when-let ((tm (plist-get placeholder :glyph-timer)))  (cancel-timer tm))
  (when-let ((tm (plist-get placeholder :deadline)))     (cancel-timer tm)))

(defun aq--placeholder-clear-region (placeholder)
  "Delete the buffer span between PLACEHOLDER's start- and end-marker.
Returns the host buffer (or nil if it has been killed)."
  (let* ((start (plist-get placeholder :start-marker))
         (end   (plist-get placeholder :end-marker))
         (buf   (and start (marker-buffer start))))
    (when (buffer-live-p buf)
      (with-current-buffer buf
        (let ((inhibit-read-only t))
          (delete-region start end)))
      buf)))

(cl-defun aq--resolve-placeholder (placeholder src-buf
                                                &key help-echo-fn view-name
                                                actions async-fn)
  "Replace PLACEHOLDER's region with the rendered content of SRC-BUF.

Cancels timers, deletes the placeholder span, then delegates to
`aq--splice-view-into' positioned at the start-marker — so the
inserted vtable carries the full `aq-region-ctx' (g/=/m/U/B/?)
plumbing exactly like a synchronous splice.

Idempotent: a no-op once the placeholder is already resolved or its
host buffer has been killed."
  (when placeholder
    (aq--placeholder-cancel-timers placeholder)
    (when-let* ((buf (aq--placeholder-clear-region placeholder))
                (start (plist-get placeholder :start-marker)))
      (aq--splice-view-into src-buf buf start
                            :help-echo-fn help-echo-fn
                            :view-name    view-name
                            :actions      actions
                            :async-fn     async-fn)
      (set-marker (plist-get placeholder :start-marker) nil)
      (set-marker (plist-get placeholder :end-marker) nil))))

(defun aq--fail-placeholder (placeholder reason)
  "Replace PLACEHOLDER's region with a ⚠️ note explaining REASON.
The placeholder's region is reclaimed (markers nilled) just like the
success path, but the user is left with a static failure message at
that slot rather than a rendered vtable.  Press `g' on the surrounding
host buffer to retry the fetch."
  (when placeholder
    (aq--placeholder-cancel-timers placeholder)
    (when-let ((buf (aq--placeholder-clear-region placeholder)))
      (with-current-buffer buf
        (save-excursion
          (goto-char (plist-get placeholder :start-marker))
          (let ((inhibit-read-only t))
            (insert (propertize
                     (format "⚠️ Fetch failed: %s — press `g' to retry.\n" reason)
                     'face '(:foreground "red3" :slant italic))))))
      (set-marker (plist-get placeholder :start-marker) nil)
      (set-marker (plist-get placeholder :end-marker) nil))))

(cl-defun aq--insert-view-on-deliver-with-placeholder
    (view-buf placeholder &key help-echo-fn view-name actions async-fn)
  "Like `aq--insert-view-on-deliver', but resolve PLACEHOLDER on deliver.
After the next async delivery into VIEW-BUF, replace PLACEHOLDER's
region with the rendered content, threading the same region-ctx
metadata through `aq--resolve-placeholder' → `aq--splice-view-into'."
  (with-current-buffer view-buf
    (letrec ((hook (lambda ()
                     (aq--resolve-placeholder placeholder view-buf
                                              :help-echo-fn help-echo-fn
                                              :view-name    view-name
                                              :actions      actions
                                              :async-fn     async-fn)
                     (setq aq--post-deliver-hook
                           (delq hook aq--post-deliver-hook)))))
      (push hook aq--post-deliver-hook))))

(provide 'inline-spinner)
;;; inline-spinner.el ends here
