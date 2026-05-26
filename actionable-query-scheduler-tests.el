;;; actionable-query-scheduler-tests.el --- ERT tests for actionable-query-scheduler.el  -*- lexical-binding: t; -*-
;;
;; Run batch:
;;   emacs --batch -L ~/actionable-query -L ~/snap \
;;         --eval '(package-initialize)' \
;;         -l actionable-query-scheduler-tests.el -f ert-run-tests-batch-and-exit
;;
;; Run interactively: M-x ert after loading this file.

(require 'cl-lib)
(load (expand-file-name "~/snap/snap.el") nil t t)
(load (expand-file-name "~/actionable-query/actionable-query-scheduler.el") nil t t)

(snap-define-fixture deftest)

;;; ─── actionable-query-effort-from-times ────────────────────────────────────────────────────

(deftest "effort-from-times -- 09:00 to 10:00 → 1:00"
  (should (equal "1:00" (actionable-query-effort-from-times "09:00" "10:00"))))

(deftest "effort-from-times -- 09:00 to 09:30 → 0:30"
  (should (equal "0:30" (actionable-query-effort-from-times "09:00" "09:30"))))

(deftest "effort-from-times -- 09:45 to 11:15 → 1:30"
  (should (equal "1:30" (actionable-query-effort-from-times "09:45" "11:15"))))

(deftest "effort-from-times -- nil start → nil"
  (should (null (actionable-query-effort-from-times nil "10:00"))))

(deftest "effort-from-times -- nil end → nil"
  (should (null (actionable-query-effort-from-times "09:00" nil))))

(deftest "effort-from-times -- end before start → nil (no negative duration)"
  (should (null (actionable-query-effort-from-times "10:00" "09:00"))))

(deftest "effort-from-times -- same time → nil (zero duration)"
  (should (null (actionable-query-effort-from-times "10:00" "10:00"))))

;;; ─── actionable-query--effort-minutes ──────────────────────────────────────────────────────

(deftest "effort-minutes -- 1:00 → 60"
  (should (= 60 (actionable-query--effort-minutes "1:00"))))

(deftest "effort-minutes -- 0:30 → 30"
  (should (= 30 (actionable-query--effort-minutes "0:30"))))

(deftest "effort-minutes -- 2:15 → 135"
  (should (= 135 (actionable-query--effort-minutes "2:15"))))

(deftest "effort-minutes -- nil → 60 (default)"
  (should (= 60 (actionable-query--effort-minutes nil))))

(deftest "effort-minutes -- empty string → 60 (default)"
  (should (= 60 (actionable-query--effort-minutes ""))))

;;; ─── actionable-query--round-up-15 ─────────────────────────────────────────────────────────

(deftest "round-up-15 -- exact boundary is unchanged"
  (should (= 480 (actionable-query--round-up-15 480))))  ; 08:00 exactly

(deftest "round-up-15 -- 481 → 495"
  (should (= 495 (actionable-query--round-up-15 481))))

(deftest "round-up-15 -- 494 → 495"
  (should (= 495 (actionable-query--round-up-15 494))))

(deftest "round-up-15 -- 495 → 495 (already on boundary)"
  (should (= 495 (actionable-query--round-up-15 495))))

(deftest "round-up-15 -- 0 → 0"
  (should (= 0 (actionable-query--round-up-15 0))))

(deftest "round-up-15 -- 1 → 15"
  (should (= 15 (actionable-query--round-up-15 1))))

;;; ─── actionable-query--first-free-slot-on (future date, no busy intervals) ─────────────────
;;
;; For a future date the busy set is empty (no org-agenda-files to scan in
;; batch mode), so the first slot is always 08:00 = 480 minutes.

(deftest "first-free-slot-on -- future date, 60 min, no busy → 480 (08:00)"
  (let ((future "2099-01-15"))
    (cl-letf (((symbol-function 'actionable-query--busy-intervals-on) (lambda (_) nil)))
      (should (= 480 (actionable-query--first-free-slot-on future 60))))))

(deftest "first-free-slot-on -- future date, single busy 480-540, 60 min → 540"
  ;; 08:00–09:00 is busy; next slot starts at 09:00.
  (cl-letf (((symbol-function 'actionable-query--busy-intervals-on)
             (lambda (_) '((480 . 540)))))
    (should (= 540 (actionable-query--first-free-slot-on "2099-01-15" 60)))))

(deftest "first-free-slot-on -- future date, two busy blocks, finds gap"
  ;; 08:00–09:00 and 09:30–10:30 busy; 60-min slot fits at 09:00? No —
  ;; 09:00–10:00 overlaps 09:30–10:30.  Next candidate after 10:30 rounded
  ;; up is 630 = 10:30.  That fits a 60-min slot ending at 11:30 ≤ 16:00.
  (cl-letf (((symbol-function 'actionable-query--busy-intervals-on)
             (lambda (_) '((480 . 540) (570 . 630)))))
    (should (= 630 (actionable-query--first-free-slot-on "2099-01-15" 60)))))

(deftest "first-free-slot-on -- future date, fully booked → nil"
  ;; One solid block from 08:00 to 16:00 leaves no room for even 1 min.
  (cl-letf (((symbol-function 'actionable-query--busy-intervals-on)
             (lambda (_) '((480 . 960)))))
    (should (null (actionable-query--first-free-slot-on "2099-01-15" 60)))))

(deftest "first-free-slot-on -- 30-min slot fits earlier than 60-min"
  ;; 08:00–08:45 busy; a 30-min slot fits at 08:45 (rounded up to 08:45).
  (cl-letf (((symbol-function 'actionable-query--busy-intervals-on)
             (lambda (_) '((480 . 525)))))
    (should (= 525 (actionable-query--first-free-slot-on "2099-01-15" 30)))))

(deftest "first-free-slot-on -- overlapping busy intervals are merged"
  ;; Two overlapping blocks: 08:00–09:00 and 08:30–10:00 → merged 08:00–10:00.
  ;; A 60-min slot should land at 600 = 10:00.
  (cl-letf (((symbol-function 'actionable-query--busy-intervals-on)
             (lambda (_) '((480 . 540) (510 . 600)))))
    (should (= 600 (actionable-query--first-free-slot-on "2099-01-15" 60)))))

;;; ─── actionable-query-next-free-slot ───────────────────────────────────────────────────────

(deftest "next-free-slot -- returns a valid Org timestamp string"
  ;; With no busy intervals the slot lands today at 08:00 or later.
  (cl-letf (((symbol-function 'actionable-query--busy-intervals-on) (lambda (_) nil)))
    (let ((stamp (actionable-query-next-free-slot 60)))
      (should (stringp stamp))
      (should (string-match-p "^<[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\} [A-Z][a-z][a-z] [0-9]\\{2\\}:[0-9]\\{2\\}>$"
                              stamp)))))

(deftest "next-free-slot -- 30-min request returns timestamp ending in :00 or :30"
  (cl-letf (((symbol-function 'actionable-query--busy-intervals-on) (lambda (_) nil)))
    (let* ((stamp (actionable-query-next-free-slot 30))
           (mins  (string-to-number (substring stamp -3 -1))))
      (should (member mins '(0 15 30 45))))))

(deftest "next-free-slot -- fully booked horizon raises user-error"
  (cl-letf (((symbol-function 'actionable-query--busy-intervals-on)
             (lambda (_) '((0 . 1440)))))  ; 24 h solid
    (should-error (actionable-query-next-free-slot 60) :type 'user-error)))

(provide 'actionable-query-scheduler-tests)
;;; actionable-query-scheduler-tests.el ends here
