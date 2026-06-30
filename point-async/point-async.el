;;; point-async.el --- Reserve a point for async insertion  -*- lexical-binding: t; -*-

;; Author: Musa Al-hassy <alhassy@gmail.com>

;;; Commentary:

;; A composition primitive for buffers that interleave plain prose
;; with content whose value arrives later.  The motivating shape:
;;
;;     (insert "Before")
;;     (let ((here (point-async-reserve)))
;;       (run-with-timer 0.5 nil
;;         (lambda ()
;;           (point-async-resolve here)
;;           (insert "NICE"))))
;;     (insert "After")
;;
;; While the slot is in flight the buffer reads
;;
;;     Before ⏳ After
;;
;; and once we resolve and `(insert "NICE")' it reads
;;
;;     Before NICE After
;;
;; ---regardless of whether the resolve happens synchronously inside
;; the same form or hours later from a process filter.
;;
;; The key invariant: subsequent `(insert "After")' composes at the
;; right spot in source order even when the slot is still pending.
;; Each `point-async-reserve' call owns its own marker pair, so
;; interleaved resolutions of multiple slots stay in their respective
;; slots ---the prose-then-table-then-prose-then-table layout that
;; breaks when you naively `(insert (do-async))' because async
;; resolutions race the prose and clump at the end.
;;
;; API ---three user-facing functions:
;;
;;   (point-async-reserve &key label deadline)  ⇒ HERE
;;   (point-async-resolve HERE)
;;   (point-async-fail HERE REASON)
;;
;; `point-async-reserve' synchronously paints a ⏳-animated placeholder
;; at point in the current buffer, advances point past it like
;; `(insert ...)', and returns an opaque handle HERE.
;;
;; `point-async-resolve' cancels the spinner, deletes the placeholder
;; span, and parks point at the cleared slot.  Whatever the caller
;; runs next executes at that point ---the analogue of the body of a
;; `save-excursion' that has time-travelled to where reserve was
;; called.
;;
;; `point-async-fail' replaces the slot with a ⚠️ note explaining
;; REASON.  If nothing resolves or fails the slot, the deadline timer
;; trips after `point-async-deadline-seconds' (default 300s) and the
;; slot self-fails.
;;
;; The slot-passing shape (rather than CPS / Promise-style) means no
;; closure capture is required: callers may schedule their resolve
;; from any binding regime, including dynamically-bound buffers like
;; `*scratch*'.
;;
;; This file is intentionally standalone ---no dependency on any
;; particular UI framework.  The only external dep is `spinner.el'
;; (mode-line spinner library, MELPA).

;;; Code:

(require 'cl-lib)
(require 'spinner)

(defcustom point-async-deadline-seconds 300
  "Seconds after which an unresolved `point-async-reserve' slot self-fails.
Tuned for slow network fetches; bump higher if you regularly see
false-failure ⚠️ notes for producers that eventually succeed."
  :type 'integer
  :group 'point-async)

(defface point-async-placeholder-face
  '((t :foreground "DarkOrange3" :slant italic))
  "Face for in-flight `point-async-reserve' placeholders.
Distinct from final content ---placeholders are *temporary*, not
load-bearing, so visible (orange) rather than recessed."
  :group 'point-async)

(defconst point-async--hourglass-frames ["⏳" "⌛"]
  "Frames cycled in the placeholder while a slot is in flight.")

(defun point-async--format-elapsed (seconds)
  "Render SECONDS (float) as a terse human duration.
Examples: 0.12 → \"120ms\", 1.4 → \"1.4s\", 123.4 → \"2m 03.4s\"."
  (cond
   ((< seconds 1)  (format "%dms" (round (* seconds 1000))))
   ((< seconds 60) (format "%.1fs" seconds))
   (t              (let* ((m (floor (/ seconds 60)))
                          (s (- seconds (* m 60))))
                     (format "%dm %04.1fs" m s)))))

(defun point-async--paint-placeholder (target-buf label deadline)
  "Insert a read-only animated ⏳ placeholder at point in TARGET-BUF.
LABEL is the human text after the glyph.  DEADLINE is seconds
until the slot self-fails.  Returns an opaque slot plist holding
`:start-marker', `:end-marker', `:spinner', `:glyph-timer',
`:deadline', `:done'.  Both markers have insertion-type nil so
subsequent inserts at the slot's edges do not push them off the
placeholder span.  Point in TARGET-BUF advances past the
placeholder, just like `(insert ...)'."
  (with-current-buffer target-buf
    (let* ((inhibit-read-only t)
           (start (copy-marker (point) nil))
           (visible (propertize
                     (format "⏳ %s" label)
                     'face 'point-async-placeholder-face
                     'read-only "🔒 In-flight point-async slot"
                     'front-sticky nil
                     'rear-nonsticky t
                     'point-async--placeholder t))
           end sp glyph-timer deadline-timer)
      (insert visible)
      (insert "\n")
      (setq end (copy-marker (point) nil))
      (set-marker-insertion-type end nil)
      (setq sp (spinner-create 'progress-bar t 10))
      (spinner-start sp)
      (let ((frame 0))
        (setq glyph-timer
              (run-with-timer
               1 1
               (lambda ()
                 (when (and (buffer-live-p target-buf)
                            (marker-buffer start))
                   (with-current-buffer target-buf
                     (setq frame (% (1+ frame)
                                    (length point-async--hourglass-frames)))
                     (let ((glyph (aref point-async--hourglass-frames frame))
                           (inhibit-read-only t))
                       (with-silent-modifications
                         (save-excursion
                           (goto-char start)
                           (when (re-search-forward "[⏳⌛]" end t)
                             (replace-match glyph t t)
                             (add-text-properties
                              (match-beginning 0) (match-end 0)
                              (list 'face 'point-async-placeholder-face
                                    'read-only "🔒 In-flight point-async slot"
                                    'front-sticky nil
                                    'rear-nonsticky t
                                    'point-async--placeholder t))))))))))))
      (let ((slot (list :start-marker start
                        :end-marker end
                        :spinner sp
                        :glyph-timer glyph-timer
                        :deadline nil
                        :done nil)))
        (setq deadline-timer
              (run-at-time
               deadline nil
               (lambda ()
                 (point-async-fail
                  slot
                  (format "%s deadline hit"
                          (point-async--format-elapsed deadline))))))
        (plist-put slot :deadline deadline-timer)
        slot))))

(defun point-async--cancel-timers (slot)
  "Stop the spinner and cancel both timers attached to SLOT."
  (when-let ((sp (plist-get slot :spinner)))      (spinner-stop sp))
  (when-let ((tm (plist-get slot :glyph-timer)))  (cancel-timer tm))
  (when-let ((tm (plist-get slot :deadline)))     (cancel-timer tm)))

(defun point-async--clear-region (slot)
  "Delete the buffer span between SLOT's start- and end-marker.
Returns the host buffer (or nil if it has been killed)."
  (let* ((start (plist-get slot :start-marker))
         (end   (plist-get slot :end-marker))
         (buf   (and start (marker-buffer start))))
    (when (buffer-live-p buf)
      (with-current-buffer buf
        (let ((inhibit-read-only t))
          (delete-region start end)))
      buf)))

(defun point-async--release-markers (slot)
  "Release SLOT's marker pair so they don't keep the buffer alive."
  (when-let ((m (plist-get slot :start-marker))) (set-marker m nil))
  (when-let ((m (plist-get slot :end-marker)))   (set-marker m nil)))

(cl-defun point-async-reserve (&key (label "fetching…")
                                    (deadline point-async-deadline-seconds))
  "Reserve a point at the current location and return a handle.

Synchronously paints a ⏳-animated placeholder at point ---tagged
LABEL--- and advances point past it like `(insert ...)'.  Returns
an opaque handle HERE that the caller stashes for later use with
`point-async-resolve' or `point-async-fail'.

If the slot is never resolved or failed, the deadline timer trips
after DEADLINE seconds (default `point-async-deadline-seconds')
and the slot is replaced with a ⚠️ note.

Concretely:

  ;; Synchronous case ---data already on hand:
  (insert \"Before\")
  (let ((here (point-async-reserve)))
    (point-async-resolve here)
    (insert \"NICE\"))
  (insert \"After\")
  ;; ⇒ buffer reads \"BeforeNICEAfter\".
  ;; ⏳ never paints visibly to the user.

  ;; Asynchronous case ---fetch returns later:
  (insert \"Before\")
  (let ((here (point-async-reserve)))
    (run-with-timer 0.5 nil
                    (lambda ()
                      (point-async-resolve here)
                      (insert \"NICE\"))))
  (insert \"After\")
  ;; t=0:    buffer is \"Before⏳After\", point past After.
  ;; t=0.5s: buffer is \"BeforeNICEAfter\"."
  (point-async--paint-placeholder (current-buffer) label deadline))

(defun point-async-resolve (here)
  "Clear HERE's placeholder and park point at the saved location.

Cancels timers, deletes the placeholder span, and positions point
at the cleared slot in HERE's host buffer.  Whatever the caller
runs next executes at that point ---a single `(insert \"NICE\")', a
multi-line splice with text-properties, a vtable render, all from
a known point-position contract.

If HERE was reserved in a different buffer than the current one,
the caller is switched into HERE's buffer ---resolve is
buffer-aware on purpose, since the most common async source
(process filters, network sentinels) runs in unrelated buffers.

Idempotent: a no-op once HERE has already resolved or failed, or
if its host buffer has been killed."
  (when (and here (not (plist-get here :done)))
    (plist-put here :done t)
    (point-async--cancel-timers here)
    (when-let* ((buf (point-async--clear-region here))
                (start (plist-get here :start-marker)))
      (set-buffer buf)
      (goto-char start)
      (point-async--release-markers here))))

(defun point-async-fail (here reason)
  "Replace HERE's placeholder with a ⚠️ note explaining REASON.

The slot's region is reclaimed (markers nilled) just like the
success path, but the user is left with a static failure message
at that location rather than the resolved content.  Idempotent."
  (when (and here (not (plist-get here :done)))
    (plist-put here :done t)
    (point-async--cancel-timers here)
    (when-let ((buf (point-async--clear-region here)))
      (with-current-buffer buf
        (goto-char (plist-get here :start-marker))
        (let ((inhibit-read-only t))
          (insert (propertize
                   (format "⚠️ point-async failed: %s.\n" reason)
                   'face '(:foreground "red3" :slant italic)))))
      (point-async--release-markers here))))

(provide 'point-async)
;;; point-async.el ends here
