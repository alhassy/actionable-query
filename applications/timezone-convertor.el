;;; timezone-convertor.el --- World-clock widget for the actionable-query dashboard  -*- lexical-binding: t; -*-

;; Author: Musa Al-hassy <alhassy@gmail.com>

;;; Commentary:

;; The current time across the zones we collaborate in, as a single-row
;; `actionable-query-defview' (`dashboard/world-clock').  There is no fetch and
;; no cache: the time "now" in any zone is pure local computation via
;; `format-time-string' with a `TZ' string, so `g' just re-renders.  Editing a
;; cell pins an instant (`dashboard--clock-anchor') so every other zone
;; recomputes off the same wall-clock time ---a tiny timezone converter.

;;; Code:

(require 'actionable-query)
(require 'org)

(defvar dashboard-clock-zones
  '(("🇯🇵" "Asia/Tokyo"          "Japan (Tokyo)")
    ("🇨🇦" "America/Toronto"     "Canada (Toronto)")
    ("🇺🇸" "America/Los_Angeles" "California (Los Angeles)")
    ("🇮🇳" "Asia/Kolkata"        "India (Kolkata)"))
  "Zones shown by `dashboard/world-clock', in display order.
Each entry is (FLAG TZ LOCATION): FLAG is the emoji shown in the cell, TZ is
any value the OS accepts in the `TZ' env var (an IANA zone name), and
LOCATION is the human name surfaced via help-echo / tooltip.")

(defvar dashboard--clock-anchor nil
  "Universal time (a `current-time'-style value) the world-clock is pinned to.
Nil means \"now\" --- each open re-reads the live clock.  Editing any row's
Time sets this so every other zone recomputes off the same instant; `g'
clears it back to nil (live).")

(defun dashboard--clock-instant ()
  "The instant the clock is showing: the pinned anchor, or now."
  (or dashboard--clock-anchor (current-time)))

(defun dashboard--clock-parse (input tz)
  "Parse user INPUT (e.g. \"3pm\", \"15:30\", \"tomorrow 9am\") as a time in zone TZ.
Returns a universal time.  Leans on `org-read-date', interpreting the typed
time as wall-clock in TZ so the other zones convert correctly."
  (let* ((parsed (org-read-date t t input nil (dashboard--clock-instant)))
         ;; `org-read-date' parses against the *local* zone; re-encode the same
         ;; wall-clock fields under TZ so "3pm" means 3pm in THAT city.
         (dt     (decode-time parsed (current-time-zone) t)))
    (encode-time (append (list (decoded-time-second dt) (decoded-time-minute dt)
                               (decoded-time-hour dt) (decoded-time-day dt)
                               (decoded-time-month dt) (decoded-time-year dt))
                         (list nil -1 tz)))))

(defun dashboard--clock-cell (zone)
  "Render ZONE ((FLAG TZ LOCATION)) as \"🇯🇵 Thu 4:00 AM\" with a location tooltip.
The LOCATION rides a `help-echo' text-property, so hovering the cell reveals
which city the flag stands for."
  (let* ((flag (nth 0 zone)) (tz (nth 1 zone)) (loc (nth 2 zone))
         (time (format-time-string "%a %-I:%M %p" (dashboard--clock-instant) tz)))
    (propertize (format "%s %s" flag time) 'help-echo loc)))

(defun dashboard--clock-columns ()
  "Build one vtable column per zone --- each cell `flag + time', editable.
Editing a cell pins that wall-clock instant (via `dashboard--clock-parse'),
so every other zone recomputes off it; the table reverts whole on `e'."
  (mapcar
   (lambda (zone)
     (list :name "" :width 18
           :getter (lambda (&rest _) (dashboard--clock-cell zone))
           :editable t
           :setter (lambda (_o new-value)
                     (setq dashboard--clock-anchor
                           (dashboard--clock-parse new-value (nth 1 zone))))))
   dashboard-clock-zones))

(actionable-query-defview dashboard/world-clock "🕐 World clock"
  ;; A single row; each zone is its own column, so the four clocks sit on one
  ;; line.  The lone object is a placeholder --- every column getter ignores it
  ;; and reads its zone from the closure built by `dashboard--clock-columns'.
  :objects (lambda () (list (list :clock t)))
  ;; Header-line (not a buffer row) so the empty column names don't print a
  ;; blank line --- which also lets the weather table butt onto the same line.
  :use-header-line t
  :no-footer t
  :columns (dashboard--clock-columns)
  :actions `(("G" "Reset to the live time (drop the pinned instant)"
              ,(lambda (_o)
                 (setq dashboard--clock-anchor nil)
                 (actionable-query-refresh-current-view)
                 (message "World clock back to live time.")))))

(provide 'timezone-convertor)
;;; timezone-convertor.el ends here
