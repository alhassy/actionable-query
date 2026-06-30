;;; point-async-tests.el --- ERT tests for point-async.el  -*- lexical-binding: t; -*-
;;
;; Run batch:
;;   emacs --batch -L . -L ~/snap \
;;         --eval '(package-initialize)' \
;;         -l point-async-tests.el -f ert-run-tests-batch-and-exit
;;
;; Run interactively: M-x ert after loading this file.

(require 'cl-lib)
(load (expand-file-name "~/snap/snap.el") nil t t)
(snap-define-fixture deftest)
(load (expand-file-name "~/actionable-query/point-async/point-async.el") nil t t)

;;; ─── §1 · synchronous placeholder paint ─────────────────────────────────────

(deftest "point-async -- placeholder visible synchronously"
  ;; Reserve a slot but never resolve it; inspect the buffer immediately.
  ;; ⏳ glyph + label must be visible at point, markers must be live, and
  ;; both timers (glyph cycle + deadline) must be ticking.
  (let ((buf (generate-new-buffer " *point-async-test-paint*")))
    (unwind-protect
        (let (here)
          (with-current-buffer buf
            (insert "before\n")
            (setq here (point-async-reserve :label "fetching foo…"
                                            :deadline 60))
            (should (string-match-p "before\n⏳ fetching foo…\n"
                                    (buffer-string))))
          (should (markerp (plist-get here :start-marker)))
          (should (markerp (plist-get here :end-marker)))
          (should (timerp  (plist-get here :glyph-timer)))
          (should (timerp  (plist-get here :deadline)))
          (point-async--cancel-timers here))
      (kill-buffer buf))))

;;; ─── §2 · synchronous resolve ───────────────────────────────────────────────

(deftest "point-async -- resolve clears placeholder, parks point at slot"
  ;; Reserve, then immediately resolve and `(insert ...)' the content.  ⏳
  ;; never paints visibly; the inserted content is what survives, with
  ;; markers nilled and timers cancelled.
  (let ((buf (generate-new-buffer " *point-async-test-sync-resolve*")))
    (unwind-protect
        (let (here)
          (with-current-buffer buf
            (insert "before\n")
            (setq here (point-async-reserve))
            (point-async-resolve here)
            (insert "RENDERED CONTENT")
            (should-not (string-match-p "⏳" (buffer-string)))
            (should     (string-match-p "RENDERED CONTENT" (buffer-string))))
          (should-not (marker-buffer (plist-get here :start-marker)))
          (should-not (marker-buffer (plist-get here :end-marker))))
      (kill-buffer buf))))

;;; ─── §3 · out-of-order async resolution ─────────────────────────────────────

(deftest "point-async -- out-of-order resolution preserves source order"
  ;; Reserve slots A and B at distinct points, then resolve B before A.
  ;; A's content must still sit BEFORE B's in the buffer ---this is the
  ;; regression test for the prose-then-table clumping bug.
  (let ((host (generate-new-buffer " *point-async-test-ordering*"))
        here-a here-b)
    (unwind-protect
        (progn
          (with-current-buffer host
            (insert "* Section A\n")
            (setq here-a (point-async-reserve :label "A…"))
            (goto-char (point-max))
            (insert "* Section B\n")
            (setq here-b (point-async-reserve :label "B…")))
          ;; Resolve B first, then A ---the bad ordering case.
          (with-current-buffer host
            (point-async-resolve here-b)
            (insert "BBB")
            (point-async-resolve here-a)
            (insert "AAA"))
          (with-current-buffer host
            (let* ((s     (buffer-string))
                   (pos-a (string-match "AAA" s))
                   (pos-b (string-match "BBB" s)))
              (should pos-a)
              (should pos-b)
              (should (< pos-a pos-b)))))
      (kill-buffer host))))

;;; ─── §4 · prose interleaving via cached / sync resolve ──────────────────────

(deftest "point-async -- prose interleaves correctly when resolves are sync"
  ;; Composition shape:
  ;;
  ;;   (insert "* Section 1\n")
  ;;   (let ((here (point-async-reserve)))
  ;;     (point-async-resolve here)
  ;;     (insert "AAA"))
  ;;   (insert "* Section 2\n")
  ;;   (let ((here (point-async-reserve)))
  ;;     (point-async-resolve here)
  ;;     (insert "BBB"))
  ;;
  ;; Resolve must leave point at the END of the cleared slot ---like
  ;; `insert'--- so subsequent prose composes naturally rather than
  ;; landing in front of the resolved content.
  (let ((host (generate-new-buffer " *point-async-test-prose-interleave*")))
    (unwind-protect
        (with-current-buffer host
          (insert "* Section 1\n")
          (let ((here (point-async-reserve)))
            (point-async-resolve here)
            (insert "AAA"))
          (insert "* Section 2\n")
          (let ((here (point-async-reserve)))
            (point-async-resolve here)
            (insert "BBB"))
          (let* ((s       (buffer-string))
                 (pos-h1  (string-match "Section 1" s))
                 (pos-aaa (string-match "AAA" s))
                 (pos-h2  (string-match "Section 2" s))
                 (pos-bbb (string-match "BBB" s)))
            (should pos-h1)
            (should pos-aaa)
            (should pos-h2)
            (should pos-bbb)
            ;; Source order: heading 1, AAA, heading 2, BBB.
            (should (< pos-h1  pos-aaa))
            (should (< pos-aaa pos-h2))
            (should (< pos-h2  pos-bbb))))
      (kill-buffer host))))

;;; ─── §5 · explicit fail ─────────────────────────────────────────────────────

(deftest "point-async -- fail replaces slot with ⚠️ note"
  ;; Reserve but never resolve.  Call `point-async-fail' explicitly and
  ;; assert the slot was reclaimed with a ⚠️ note.
  (let ((host (generate-new-buffer " *point-async-test-fail*")))
    (unwind-protect
        (let (here)
          (with-current-buffer host
            (setq here (point-async-reserve :label "fetching Z…"
                                            :deadline 60)))
          (point-async-fail here "deadline hit")
          (with-current-buffer host
            (should-not (string-match-p "⏳" (buffer-string)))
            (should     (string-match-p "⚠️ point-async failed: deadline hit"
                                        (buffer-string))))
          (should-not (marker-buffer (plist-get here :start-marker)))
          (should-not (marker-buffer (plist-get here :end-marker))))
      (kill-buffer host))))

;;; ─── §6 · truly async resolve via run-with-timer ────────────────────────────

(deftest "point-async -- truly async (run-with-timer) resolves correctly"
  ;; Schedule the resolve via `run-with-timer' 0.05.  Immediately after
  ;; reserve, the buffer must show the ⏳ placeholder; after waiting,
  ;; the placeholder must be replaced with the inserted content.
  ;;
  ;; This is the test that *would* have failed under the old CPS API in
  ;; a dynamically-bound buffer.  Slot-passing makes binding mode
  ;; irrelevant, so this works under either.
  (let ((buf (generate-new-buffer " *point-async-test-truly-async*")))
    (unwind-protect
        (progn
          (with-current-buffer buf
            (insert "before\n")
            (let ((here (point-async-reserve)))
              (run-with-timer
               0.05 nil
               (lambda ()
                 (point-async-resolve here)
                 (insert "ASYNC NICE"))))
            ;; t = 0: placeholder visible.
            (should (string-match-p "⏳" (buffer-string)))
            (should-not (string-match-p "ASYNC NICE" (buffer-string))))
          (sit-for 0.2)
          (with-current-buffer buf
            (should-not (string-match-p "⏳" (buffer-string)))
            (should     (string-match-p "ASYNC NICE" (buffer-string)))))
      (kill-buffer buf))))

;;; ─── §7 · deadline timer trips when nothing resolves ────────────────────────

(deftest "point-async -- deadline trips when slot is never resolved"
  ;; Reserve with a 0.05s deadline; after waiting, the slot must carry
  ;; the ⚠️ note rather than the ⏳ placeholder.
  (let ((buf (generate-new-buffer " *point-async-test-deadline*")))
    (unwind-protect
        (progn
          (with-current-buffer buf
            (insert "before\n")
            (point-async-reserve :label "stalled…" :deadline 0.05)
            (should (string-match-p "⏳" (buffer-string))))
          (sit-for 0.2)
          (with-current-buffer buf
            (should-not (string-match-p "⏳" (buffer-string)))
            (should     (string-match-p "⚠️ point-async failed"
                                        (buffer-string)))))
      (kill-buffer buf))))

;;; ─── §8 · idempotency: :done flag protects against double-resolve / fail-after-resolve ────

(deftest "point-async -- resolve is idempotent (double resolve is a no-op)"
  ;; The `:done' flag exists specifically so a careless double-resolve
  ;; doesn't double-clear the slot and doesn't double-park point ---and
  ;; so a late deadline trip after a successful resolve is silent rather
  ;; than stomping the resolved content with a ⚠️ note.  Removing the
  ;; `(not (plist-get here :done))' guard from `point-async-resolve'
  ;; would pass every test before this one.
  (let ((buf (generate-new-buffer " *point-async-test-idempotent*")))
    (unwind-protect
        (with-current-buffer buf
          (insert "before\n")
          (let ((here (point-async-reserve)))
            (point-async-resolve here)
            (insert "RESOLVED")
            (let ((snapshot (buffer-string)))
              ;; Second resolve must be a silent no-op.
              (point-async-resolve here)
              (should (equal (buffer-string) snapshot))
              ;; And `fail' after `resolve' must not write a ⚠️ note.
              (point-async-fail here "late failure")
              (should (equal (buffer-string) snapshot))
              (should-not (string-match-p "⚠️" (buffer-string))))))
      (kill-buffer buf))))

;;; ─── §9 · resolve cancels the deadline timer ─────────────────────────────────

(deftest "point-async -- resolve cancels the deadline timer (no late ⚠️ note)"
  ;; §7 proves the deadline trips when *nothing* resolves; nothing
  ;; before this proved the deadline stays silent after a *successful*
  ;; resolve.  Without `point-async--cancel-timers' inside `resolve', a
  ;; slot that resolves quickly and then sits in the buffer would
  ;; silently self-fail later and overwrite the resolved content with
  ;; a ⚠️ note.
  (let ((buf (generate-new-buffer " *point-async-test-resolve-cancels-deadline*")))
    (unwind-protect
        (progn
          (with-current-buffer buf
            (insert "before\n")
            (let ((here (point-async-reserve :label "fast…" :deadline 0.05)))
              (point-async-resolve here)
              (insert "RESOLVED")))
          ;; Wait well past the deadline.
          (sit-for 0.2)
          (with-current-buffer buf
            (should     (string-match-p "RESOLVED" (buffer-string)))
            (should-not (string-match-p "⚠️"      (buffer-string)))
            (should-not (string-match-p "⏳"      (buffer-string)))))
      (kill-buffer buf))))

;;; ─── §10 · resolve routes to HERE's host buffer ──────────────────────────────

(deftest "point-async -- resolve routes to HERE's host buffer (cross-buffer)"
  ;; The real async path: process filters and network sentinels run in
  ;; unrelated buffers and must resolve into the host.  AQ's
  ;; `aq--insert-view-async' explicitly relies on this ---the resolve
  ;; fires from inside `(with-current-buffer view-buf ...)' but writes
  ;; to the host.  §6 covers truly-async resolve but stays in the same
  ;; buffer the whole time; this test proves the buffer-routing.
  (let ((host  (generate-new-buffer " *point-async-test-host*"))
        (other (generate-new-buffer " *point-async-test-other*")))
    (unwind-protect
        (progn
          (with-current-buffer host  (insert "HOST-before\n"))
          (with-current-buffer other (insert "OTHER untouched"))
          (let (here)
            (with-current-buffer host
              (setq here (point-async-reserve)))
            ;; Switch into the unrelated buffer and resolve from there.
            (with-current-buffer other
              (point-async-resolve here)
              (insert "RESOLVED-IN-HOST")))
          (with-current-buffer host
            (should     (string-match-p "HOST-before" (buffer-string)))
            (should     (string-match-p "RESOLVED-IN-HOST" (buffer-string)))
            (should-not (string-match-p "⏳" (buffer-string))))
          (with-current-buffer other
            (should     (equal (buffer-string) "OTHER untouched"))
            (should-not (string-match-p "RESOLVED-IN-HOST" (buffer-string)))))
      (kill-buffer host)
      (kill-buffer other))))

(provide 'point-async-tests)
;;; point-async-tests.el ends here
