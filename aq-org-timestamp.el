;;; aq-org-timestamp.el --- Clicking an org timestamp opens an actionable-query view  -*- lexical-binding: t; -*-

;; Author: Musa Al-hassy <alhassy@gmail.com>

;;; Commentary:

;; Vanilla Org, on RET / `org-open-at-point' over a timestamp like
;; `<2026-05-26 Tue>', shows a plain one-day `org-agenda-list'.  This advises
;; `org-follow-timestamp-link' so a *single-day* timestamp instead opens the
;; nicer `dashboard/work-calendar' for that date (gcal + org day-agenda +
;; holidays, colourful, with all the calendar keybindings).  Date *ranges*
;; (`<a>--<b>') and the case where the calendar feature isn't loaded fall
;; through to Org's original behaviour.
;;
;; Enable with `actionable-query-enable-timestamp-views' (dashboard.el calls
;; this at load).  Disable with `actionable-query-disable-timestamp-views'.

;;; Code:

(require 'org)

(declare-function dashboard/work-calendar "dashboard")
(declare-function dashboard--gcal-clear-cache "dashboard")
(defvar dashboard-work-calendar-date)

(defun aq-org-timestamp--date-at-point ()
  "Return the single-day timestamp date at point as (MONTH DAY YEAR), or nil.
Nil for date *ranges* (those keep Org's native two-day agenda) and when
point isn't on a timestamp."
  (when (and (not (org-at-date-range-p t))
             (org-at-timestamp-p 'lax))
    (let* ((stamp (substring (match-string 1) 0 10))   ; "YYYY-MM-DD"
           (parts (mapcar #'string-to-number (split-string stamp "-"))))
      (list (nth 1 parts) (nth 2 parts) (nth 0 parts)))))  ; (M D Y)

(defun aq-org-timestamp--follow-advice (orig &rest args)
  "Around-advice for `org-follow-timestamp-link': open the work-calendar.
For a single-day timestamp (and when the calendar feature is loaded), set
the calendar's viewing date and open it; otherwise defer to ORIG."
  (let ((date (and (fboundp 'dashboard/work-calendar)
                   (aq-org-timestamp--date-at-point))))
    (if date
        (progn
          (setq dashboard-work-calendar-date date)
          (when (fboundp 'dashboard--gcal-clear-cache)
            (dashboard--gcal-clear-cache))
          (dashboard/work-calendar))
      (apply orig args))))

;;;###autoload
(defun actionable-query-enable-timestamp-views ()
  "Make RET / click on an Org timestamp open `dashboard/work-calendar' for it."
  (interactive)
  (advice-add 'org-follow-timestamp-link :around #'aq-org-timestamp--follow-advice))

(defun actionable-query-disable-timestamp-views ()
  "Restore Org's native one-day agenda on timestamp click."
  (interactive)
  (advice-remove 'org-follow-timestamp-link #'aq-org-timestamp--follow-advice))

(provide 'aq-org-timestamp)
;;; aq-org-timestamp.el ends here
