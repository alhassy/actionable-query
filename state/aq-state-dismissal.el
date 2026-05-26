;;; aq-state-dismissal.el --- Snooze, hearting, and footers  -*- lexical-binding: t; -*-

;; Author: Musa Al-hassy <alhassy@gmail.com>

;;; Commentary:

;; Three intertwined concerns kept together because they share state
;; and footer real-estate:
;;
;;   • Snoozing — `aq--dismissed' is a hash mapping (VIEW-NAME . EXPIRY-KEY)
;;     to lists of dismissed object IDs.  Persisted via savehist; pruned
;;     daily; the `r' action records, the `R' command resurrects.
;;
;;   • Hearting — `aq--hearted' is a hash mapping VIEW-NAME to favourite
;;     IDs.  `h' toggles, `H' filters to hearted-only.  Also persisted.
;;
;;   • Footers — `aq--upsert-footer' / `aq--update-dismissed-footer' /
;;     `aq--update-heart-footer' upsert single-line status footers at
;;     `point-max'.  An advice on `vtable-remove-object' keeps the
;;     dismissed-items footer in sync as rows leave the table.

;;; Code:

(require 'cl-lib)
(require 'savehist)
(require 'aq-state-cache)         ; `aq--obj-id', `aq--total-objects'
(require 'aq-state-region-ctx)    ; `aq--message'

(declare-function aq--apply-filters "aq-interaction-filters")  ; forward ref via runtime symbol
(defvar aq--active-filters)        ; from `aq-interaction-filters'

;;; ─── persistent state ──────────────────────────────────────────────────────

(defvar aq--dismissed (make-hash-table :test #'equal)
  "Hash: (VIEW-NAME . EXPIRY-KEY) → list of dismissed article IDs.")

(add-to-list 'savehist-additional-variables 'aq--dismissed)

(defvar aq--hearted (make-hash-table :test #'equal)
  "Hash: VIEW-NAME → list of hearted object IDs.")

(add-to-list 'savehist-additional-variables 'aq--hearted)

;;; ─── hearting ──────────────────────────────────────────────────────────────

(defun aq--hearted-ids (view-name)
  "Return list of hearted IDs for VIEW-NAME."
  (gethash view-name aq--hearted))

(defun aq--heart-p (view-name o)
  "Return non-nil if O is hearted in VIEW-NAME."
  (member (aq--obj-id o) (aq--hearted-ids view-name)))

(defun aq--toggle-heart (view-name o)
  "Toggle the heart on O for VIEW-NAME; return new state (t = hearted)."
  (let* ((id  (aq--obj-id o))
         (ids (gethash view-name aq--hearted)))
    (if (member id ids)
        (progn (puthash view-name (delete id ids) aq--hearted) nil)
      (puthash view-name (cons id ids) aq--hearted) t)))

;;; ─── snooze ────────────────────────────────────────────────────────────────

(defun aq--snooze-key (snooze-period)
  "Return the expiry date-string for SNOOZE-PERIOD.
SNOOZE-PERIOD is one of: `tomorrow' (default), `next-week', `forever'."
  (pcase snooze-period
    ('forever   "forever")
    ('next-week (format-time-string "%Y-%m-%d"
                                    (time-add nil (* 7 24 60 60))))
    (_          (format-time-string "%Y-%m-%d"))))

(defun aq--snooze-label (snooze-period)
  "Human-readable label for SNOOZE-PERIOD."
  (pcase snooze-period
    ('forever   "forever")
    ('next-week "until next week")
    (_          "until tomorrow")))

(defvar-local aq--show-hearted-only nil
  "When non-nil, the view shows only hearted rows.")

(defvar-local aq--post-deliver-hook nil
  "List of 0-arg fns called after each async deliver in this buffer.")

;;; ─── footers ──────────────────────────────────────────────────────────────

(defun aq--upsert-footer (prop line face)
  "Axe any existing footer tagged with PROP, then insert LINE with FACE at point-max."
  (let ((inhibit-read-only t))
    (save-excursion
      (goto-char (point-max))
      (when-let ((pos (text-property-search-backward prop t #'eq)))
        (delete-region (prop-match-beginning pos) (point-max)))
      (goto-char (point-max))
      (let ((start (point)))
        (insert "\n" (propertize line 'face face))
        (put-text-property start (point) prop t)))))

(defun aq--update-heart-footer (view-name)
  "Upsert the heart-state footer line in the current buffer."
  (let* ((n    (length (aq--hearted-ids view-name)))
         (mode aq--show-hearted-only)
         (line (if (zerop n)
                   "No hearted entries yet — press `h' to heart one."
                 (format "❤️  %d hearted%s — %s"
                         n
                         (if mode " (filtered)" "")
                         (if mode
                             "H to show all · h to toggle heart"
                           "H to show hearted only · h to toggle heart")))))
    (aq--upsert-footer 'actionable-query-heart-footer line '(:height 0.8 :foreground "orchid"))))

(defun aq--install-hearting (view-name all-objects-fn)
  "Bind `h' (toggle heart) and `H' (toggle hearted-only) buffer-locally.
ALL-OBJECTS-FN is a 0-arg thunk returning the current full object list."
  (local-set-key
   (kbd "h")
   (lambda ()
     (interactive)
     (when-let ((o (vtable-current-object)))
       (let ((now-hearted (aq--toggle-heart view-name o)))
         (aq--message "%s %s"
                         (if now-hearted "❤️  Hearted:" "🩶 Un-hearted:")
                         (aq--obj-id o))
         (when (and aq--show-hearted-only (not now-hearted))
           (vtable-remove-object (vtable-current-table) o))
         (aq--update-heart-footer view-name)))))
  (local-set-key
   (kbd "H")
   (lambda ()
     (interactive)
     (setq aq--show-hearted-only (not aq--show-hearted-only))
     (let* ((table (vtable-current-table))
            (all   (funcall all-objects-fn))
            (shown (if aq--show-hearted-only
                       (cl-remove-if-not (lambda (o) (aq--heart-p view-name o)) all)
                     all)))
       (setf (vtable-objects table) shown)
       (vtable--clear-cache table)
       (vtable-revert)
       (aq--update-heart-footer view-name)))))

(defun aq--dismissed-for-view (view-name)
  "Return alist of (key . id-list) for all entries in VIEW-NAME's snooze hash."
  (cl-loop for k being the hash-keys of aq--dismissed
           using (hash-values v)
           when (equal (car k) view-name)
           collect (cons k v)))

(defvar aq--last-prune-date nil
  "Date string of the most recent `aq--prune-expired-snoozes' run; throttles daily pruning.")

(defun aq--prune-expired-snoozes ()
  "Remove snooze entries whose date key is strictly before today.
Runs at most once per calendar day."
  (let ((today (format-time-string "%Y-%m-%d")))
    (unless (equal aq--last-prune-date today)
      (setq aq--last-prune-date today)
      (maphash
       (lambda (k _)
         (let ((expiry (cdr k)))
           (when (and (stringp expiry)
                      (not (string= expiry "forever"))
                      (string< expiry today))
             (remhash k aq--dismissed))))
       aq--dismissed))))

(defun aq--dismiss (view-name id &optional snooze-period)
  "Add ID to the snoozed set for VIEW-NAME with SNOOZE-PERIOD (default `tomorrow')."
  (let ((key (cons view-name (aq--snooze-key snooze-period))))
    (puthash key (cons id (gethash key aq--dismissed)) aq--dismissed)))

(defun aq--dismiss-until (view-name id date-string)
  "Dismiss ID in VIEW-NAME with a specific DATE-STRING expiry key."
  (let ((key (cons view-name date-string)))
    (puthash key (cons id (gethash key aq--dismissed)) aq--dismissed)))

(defun aq--dismissed-items (view-name)
  "Return list of all snoozed IDs for VIEW-NAME across all expiry keys."
  (aq--prune-expired-snoozes)
  (mapcan #'cdr (aq--dismissed-for-view view-name)))

(defun aq--undismiss-all (view-name)
  "Clear all snoozed entries for VIEW-NAME (any expiry key)."
  (dolist (entry (aq--dismissed-for-view view-name))
    (remhash (car entry) aq--dismissed)))

;;; ─── dismissed-items footer (synced via vtable advice) ────────────────────

(defun aq--update-dismissed-footer (view-name &optional snooze-period total)
  "Upsert (or axe) the dismissed-items footer line in the current buffer.
SNOOZE-PERIOD is the symbol used for new snoozes (for the label).
TOTAL is the number of visible rows before snooze filtering."
  (let* ((n       (length (aq--dismissed-items view-name)))
         (label   (aq--snooze-label snooze-period))
         (filters aq--active-filters)
         (unread  (when total (- total n)))
         (filter-note
          (when filters
            (concat " — filtered by: "
                    (mapconcat (lambda (f) (format "%s=%s" (car f) (cdr f)))
                               filters ", ")
                    " (C-u = to clear)")))
         (line
          (concat
           (when (> n 0)
             (format "%d snoozed %s" n label))
           (when (and total (> total 0))
             (format "%s%d unread"
                     (if (> n 0) " — " "")
                     (max 0 unread)))
           (when (> n 0) " — press `R' to resurrect")
           filter-note)))
    (let ((inhibit-read-only t))
      (if (or (> n 0) filters (and total (> total 0)))
          (aq--upsert-footer 'actionable-query-footer line '(:height 0.8 :foreground "gray50"))
        ;; Nothing to say — axe any stale footer.
        (save-excursion
          (goto-char (point-max))
          (when-let ((pos (text-property-search-backward 'actionable-query-footer t #'eq)))
            (delete-region (prop-match-beginning pos) (point-max))))))))

(defun aq--vtable-remove-object-advice (_table object)
  "Keep actionable-query's footer in sync with the buffer's live row count.
When a row leaves the vtable in a actionable-query-managed buffer, decrement
`aq--total-objects' (unless the row just entered the dismissed
list — that's a snooze, whose display is carried by the dismissed
count itself).  Always repaint the dismissed-items footer so
\"N unread\" reflects what's actually in the table."
  (when (and (boundp 'org-ql-view-title)
             (stringp org-ql-view-title)
             (numberp aq--total-objects))
    (let* ((view-name org-ql-view-title)
           (dismissed (aq--dismissed-items view-name))
           (snoozed-now-p (member (aq--obj-id object) dismissed)))
      (unless snoozed-now-p
        (setq aq--total-objects (max 0 (1- aq--total-objects))))
      (aq--update-dismissed-footer view-name nil aq--total-objects))))

(advice-add 'vtable-remove-object :after #'aq--vtable-remove-object-advice)

(provide 'aq-state-dismissal)
;;; aq-state-dismissal.el ends here
