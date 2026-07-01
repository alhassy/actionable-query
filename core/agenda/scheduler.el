;;; actionable-query-scheduler.el --- Calendar / scheduling helpers for actionable-query  -*- lexical-binding: t; -*-
;;
;; Provides utilities for finding the next free time slot in your Org
;; calendar — useful for scheduling TODO items without double-booking.
;;
;; Public API:
;;   `actionable-query-effort-from-times'   — "HH:MM" span string from two clock strings
;;   `actionable-query-next-free-slot'      — Org timestamp for next open slot in agenda
;;
;; Run tests:
;;   emacs --batch -L ~/actionable-query -L ~/snap \
;;         --eval '(package-initialize)' \
;;         -l actionable-query-scheduler-tests.el -f ert-run-tests-batch-and-exit

;;; Code:

(require 'cl-lib)
(require 'org)
(require 'org-clock)

;;;###autoload
(defun actionable-query-effort-from-times (start end)
  "Return an \"H:MM\" effort string spanning START to END (\"HH:MM\" each).
Nil on bad input — all-day events have no duration, and we prefer
a nil default to a fabricated 0:00 that'd silently pass the
effort-required check."
  (when (and start end)
    (ignore-errors
      (pcase-let* ((`(,h1 ,m1) (mapcar #'string-to-number (split-string start ":")))
                   (`(,h2 ,m2) (mapcar #'string-to-number (split-string end   ":")))
                   (mins (- (+ (* h2 60) m2) (+ (* h1 60) m1))))
        (when (> mins 0)
          (format "%d:%02d" (/ mins 60) (% mins 60)))))))

(defun actionable-query--effort-minutes (effort)
  "Parse EFFORT (a \"H:MM\" string) to minutes.  Nil input → 60."
  (if (and effort (string-match "\\`\\([0-9]+\\):\\([0-9]+\\)\\'" effort))
      (+ (* 60 (string-to-number (match-string 1 effort)))
         (string-to-number (match-string 2 effort)))
    60))

(defun actionable-query--round-up-15 (mins)
  "Round MINS (minutes-from-midnight) up to the next 15-minute boundary."
  (* 15 (ceiling mins 15)))

(defun actionable-query--busy-intervals-on (date)
  "Return DATE's busy time intervals as ((START . END) ...) in minutes-from-midnight.
DATE is a \"YYYY-MM-DD\" string.  Busy = scheduled org items that
carry an HH:MM component, using their Effort (defaulting to 1h)
to compute END.  When DATE is today, also includes the currently-
clocked task's (clock-start, now) interval so we don't schedule
over ourselves.  Items scheduled without a time are ignored — they
float.  Sorted by START and overlap-merged."
  (let* ((is-today (equal date (format-time-string "%Y-%m-%d")))
         intervals)
    (dolist (file (org-agenda-files))
      (with-current-buffer (find-file-noselect file)
        (save-restriction
          (widen)
          (org-map-entries
           (lambda ()
             (let ((raw (org-entry-get (point) "SCHEDULED")))
               (when (and raw
                          (string-match-p (regexp-quote date) raw)
                          (string-match-p "[0-9]+:[0-9]+" raw))
                 (let* ((time    (org-time-string-to-time raw))
                        (decoded (decode-time time))
                        (h       (nth 2 decoded))
                        (m       (nth 1 decoded))
                        (start   (+ (* 60 h) m))
                        (dur     (actionable-query--effort-minutes
                                  (org-entry-get (point) "Effort"))))
                   (push (cons start (+ start dur)) intervals)))))))))
    (when (and is-today (org-clocking-p))
      (let* ((t0         (decode-time org-clock-start-time))
             (t1         (decode-time (current-time)))
             (same-day   (and (= (nth 3 t0) (nth 3 t1))
                              (= (nth 4 t0) (nth 4 t1))
                              (= (nth 5 t0) (nth 5 t1))))
             (start-mins (if same-day
                             (+ (* 60 (nth 2 t0)) (nth 1 t0))
                           0))
             (now-mins   (+ (* 60 (nth 2 t1)) (nth 1 t1))))
        (push (cons start-mins now-mins) intervals)))
    (let ((sorted (sort intervals (lambda (a b) (< (car a) (car b)))))
          merged)
      (dolist (iv sorted)
        (if (and merged (<= (car iv) (cdr (car merged))))
            (setcdr (car merged) (max (cdr (car merged)) (cdr iv)))
          (push (cons (car iv) (cdr iv)) merged)))
      (nreverse merged))))

(defun actionable-query--first-free-slot-on (date duration-mins)
  "Return the first free DURATION-MINS slot on DATE as minutes-from-midnight, or nil.
DATE is a \"YYYY-MM-DD\" string.  Workday is 08:00–16:00 (an
8-hour shift).  When DATE is today AND now is outside that
window, the window is shifted to [now, now + 8h].  For future
dates, the full 08:00–16:00 window is always used.  Candidate
starts are rounded up to the next 15-minute boundary."
  (let* ((is-today    (equal date (format-time-string "%Y-%m-%d")))
         (now-m       (when is-today
                        (let ((now (decode-time (current-time))))
                          (+ (* 60 (nth 2 now)) (nth 1 now)))))
         (day-start   480)   ; 08:00
         (day-end     960)   ; 16:00
         (window-start
          (cond
           ((not is-today)                                     day-start)
           ((and (>= now-m day-start) (< now-m day-end))       (max now-m day-start))
           (t                                                   now-m)))
         (window-end
          (cond
           ((not is-today)                                     day-end)
           ((and (>= now-m day-start) (< now-m day-end))       day-end)
           (t                                                   (+ now-m (* 8 60)))))
         (cursor (actionable-query--round-up-15 window-start))
         (busy   (actionable-query--busy-intervals-on date))
         found)
    (while (and (not found) (<= (+ cursor duration-mins) window-end))
      (let ((collision (cl-find-if
                        (lambda (iv)
                          (and (< cursor (cdr iv))
                               (< (car iv) (+ cursor duration-mins))))
                        busy)))
        (if collision
            (setq cursor (actionable-query--round-up-15 (cdr collision)))
          (setq found cursor))))
    found))

;;;###autoload
(defun actionable-query-next-free-slot (&optional duration-mins)
  "Return an Org timestamp string for the first free DURATION-MINS slot.
DURATION-MINS defaults to 60.  Searches today first; if nothing
fits, rolls forward one day at a time up to a 14-day horizon.
When the chosen slot is not today, pops a non-blocking dialog (via
`non-blocking-message-box' when available, falling back to
`message') informing the user of the spillover.
Signals `user-error' if no slot fits within the 14-day horizon."
  (let* ((duration (or duration-mins 60))
         (one-day  (* 24 60 60))
         (horizon  14)
         (now      (current-time))
         chosen)
    (cl-loop for offset from 0 below horizon
             for date-time = (time-add now (* offset one-day))
             for date      = (format-time-string "%Y-%m-%d" date-time)
             for day-name  = (format-time-string "%a" date-time)
             for slot      = (actionable-query--first-free-slot-on date duration)
             when slot
             do (progn
                  (setq chosen (list :offset offset
                                     :date date
                                     :day-name day-name
                                     :date-time date-time
                                     :slot slot))
                  (cl-return)))
    (unless chosen
      (user-error
       "No free %d-min slot in the next %d days — your calendar is full, please triage"
       duration horizon))
    (let* ((offset   (plist-get chosen :offset))
           (date     (plist-get chosen :date))
           (day-name (plist-get chosen :day-name))
           (slot     (plist-get chosen :slot))
           (hh       (/ slot 60))
           (mm       (% slot 60))
           (stamp    (format "<%s %s %02d:%02d>" date day-name hh mm)))
      (when (> offset 0)
        (let ((msg (format (concat "No room left today, so I've scheduled this task for:\n\n"
                                   "    %s %s at %02d:%02d\n\n"
                                   "If you really want to work overtime and not see your kids,\n"
                                   "reschedule it back to today manually.  Better: change\n"
                                   "something already on today's calendar to land later in\n"
                                   "the week, and RET this item again.")
                           day-name date hh mm)))
          (if (fboundp 'non-blocking-message-box)
              (non-blocking-message-box
               :title "Scheduled for a later day"
               :content msg
               :buttons '(:OK nil))
            (message "%s" msg))))
      stamp)))

;;;###autoload
(defun actionable-query-next-free-slot-today (&optional duration-mins)
  "Return an Org timestamp for the first free DURATION-MINS slot TODAY, or error.
DURATION-MINS defaults to 60.  Unlike `actionable-query-next-free-slot',
this never rolls forward to a later day --- if nothing fits in today's
remaining workday window it signals a `user-error', so the caller can
nudge the user to triage rather than silently spilling into tomorrow."
  (let* ((duration (or duration-mins 60))
         (today    (format-time-string "%Y-%m-%d"))
         (day-name (format-time-string "%a"))
         (slot     (actionable-query--first-free-slot-on today duration)))
    (unless slot
      (user-error
       "No free %d-min slot left today — reshuffle today's calendar, or use `S' to spill to a later day"
       duration))
    (format "<%s %s %02d:%02d>" today day-name (/ slot 60) (% slot 60))))

(provide 'scheduler)
;;; actionable-query-scheduler.el ends here
