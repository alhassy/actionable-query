;;; aq-state-cache.el --- Async object & elapsed-time caches  -*- lexical-binding: t; -*-

;; Author: Musa Al-hassy <alhassy@gmail.com>

;;; Commentary:

;; Two global caches keyed by VIEW-NAME:
;;
;;   `aq--object-cache'        — last delivered objects per view, for instant
;;                               re-open without a fresh fetch.
;;   `aq--last-elapsed-cache'  — last fetch's elapsed seconds, used by the
;;                               slow-fetch guard to skip auto-refresh on
;;                               expensive views.
;;
;; Plus the buffer-local lifetime variables (`aq--last-fetch-time',
;; `aq--total-objects', `aq--auto-refresh-timer', `aq--refresh-fn'),
;; the public `actionable-query-refresh-current-view' command, and
;; `aq--obj-id' (the stable identifier extractor used by the dismissal
;; cluster).

;;; Code:

(defvar aq--object-cache (make-hash-table :test #'equal)
  "Hash: VIEW-NAME → last delivered objects (for instant sidebar re-open).")

(defvar aq--last-elapsed-cache (make-hash-table :test #'equal)
  "Hash: VIEW-NAME → elapsed seconds of the last completed fetch.
Persists across buffer kills so re-opening a slow view keeps the skip-auto-fetch decision.")

(defvar-local aq--last-fetch-time nil
  "Float-time of the last successful async delivery in this buffer.")

(defvar-local aq--last-fetch-start-time nil
  "Float-time captured immediately before the last async fetch began.
Used to compute the elapsed duration shown in the footer.  Nil on
cache-hit deliveries.")

(defvar-local aq--total-objects nil
  "Visible row count after the last async delivery; decremented on non-snooze removals.")

(defvar-local aq--auto-refresh-timer nil
  "Timer object installed by :auto-refresh, or nil.")

(defvar-local aq--refresh-fn nil
  "Buffer-local closure that re-runs this view's fetch.
Set by `actionable-query-defview' at build time; called by
`actionable-query-refresh-current-view'.")

(defun actionable-query-refresh-current-view ()
  "Refresh the actionable-query view in the current buffer.
Works from cells, header lines, prose, and footers alike."
  (interactive)
  (if aq--refresh-fn
      (funcall aq--refresh-fn)
    (user-error "No actionable-query view refresh fn in this buffer")))

(defun aq--obj-id (o)
  "Return a stable identity for object O, used for dismissal + hearting.
Prefers an explicit `:id', then `:url'; falls back to O itself.  A view whose
row plists gain/lose fields over time (e.g. a contact that later carries its
last-message snippet) MUST set a stable `:id' --- otherwise the whole-plist
fallback changes and previously-hearted/dismissed rows stop matching."
  (or (plist-get o :id) (plist-get o :url) o))

(defun aq--obj-label (o)
  "A human-readable label for object O, for confirmation messages.
Prefers a name-ish plist key (`:name'/`:title'/`:heading'/`:subject'); falls
back to the stable id from `aq--obj-id'.  So `h' on a contact reports the
person's name rather than an opaque jid."
  (or (and (consp o) (or (plist-get o :name) (plist-get o :title)
                         (plist-get o :heading) (plist-get o :subject)))
      (format "%s" (aq--obj-id o))))

(provide 'aq-state-cache)
;;; aq-state-cache.el ends here
