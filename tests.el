;;; tests.el --- ERT tests for actionable-query.el  -*- lexical-binding: t; -*-
;;
;; Run batch:
;;   emacs --batch -L . -L ~/snap \
;;         --eval '(package-initialize)' \
;;         -l tests.el -f ert-run-tests-batch-and-exit
;;
;; Run interactively: M-x ert after loading this file.

(require 'cl-lib)
(require 'savehist)
(load (expand-file-name "~/snap/snap.el") nil t t)
(snap-define-fixture deftest)
(load (expand-file-name "~/actionable-query/actionable-query.el") nil t t)

;;; ─── fixture: state reset only ──────────────────────────────────────────────

(snap-define-fixture defaqtest
  "Reset actionable-query global state before body; restore afterward."
  (let ((old-dismissed (copy-hash-table aq--dismissed))
        (old-cache     (copy-hash-table aq--object-cache))
        (old-elapsed   (copy-hash-table aq--last-elapsed-cache)))
    (unwind-protect
        (progn
          (clrhash aq--dismissed)
          (clrhash aq--object-cache)
          (clrhash aq--last-elapsed-cache)
          &body)
      (setq aq--dismissed         old-dismissed
            aq--object-cache      old-cache
            aq--last-elapsed-cache old-elapsed))))

;;; ─── shared test data ───────────────────────────────────────────────────────

(defconst actionable-query-tests--rss-obj-a
  '(:title "Emacs 30 Released" :url "https://example.com/emacs30"
    :date "Thu, 07 May 2026 10:00:00 +0000" :description "Big release." :categories ("emacs"))
  "RSS plist — date formats to 2026-05-07.")

(defconst actionable-query-tests--rss-obj-b
  '(:title "Org-mode Tips" :url "https://example.com/org"
    :date "Thu, 07 May 2026 18:00:00 +0000" :description "Org tips." :categories ("org" "emacs"))
  "RSS plist — also formats to 2026-05-07.")

(defconst actionable-query-tests--rss-obj-c
  '(:title "Java CheatSheet" :url "https://example.com/java"
    :date "Sun, 24 Dec 2023 12:00:00 +0000" :description "Java reference." :categories ("java"))
  "RSS plist — noon UTC so local-time formatting stays on 2023-12-24 regardless of timezone.")

(defconst actionable-query-tests--objects
  (list actionable-query-tests--rss-obj-a actionable-query-tests--rss-obj-b actionable-query-tests--rss-obj-c))

(defconst actionable-query-tests--cols
  (aq--coerce-columns actionable-query-rss-columns)
  "vtable-column structs for the standard RSS column spec.")

;;; ─── · aq--apply-filters ──────────────────────────────────────────────

;; All tests in this section use actionable-query-tests--objects and actionable-query-tests--cols.
;; The Date getter calls aq--format-pubdate, so filter values must match
;; the formatted YYYY-MM-DD output, not the raw RFC 2822 string — this is
;; intentional: the filter operates on what the user sees on screen.

(deftest "apply-filters -- nil filters returns all objects"
  (should (= 3 (length (aq--apply-filters actionable-query-tests--objects nil actionable-query-tests--cols)))))

(deftest "apply-filters -- Date filter matches formatted date string"
  (let* ((filters '(("Date" . "2026-05-07")))
         (result  (aq--apply-filters actionable-query-tests--objects filters actionable-query-tests--cols)))
    (should (= 2 (length result)))))

(deftest "apply-filters -- Date filter excludes non-matching rows"
  (let* ((filters '(("Date" . "2023-12-24")))
         (result  (aq--apply-filters actionable-query-tests--objects filters actionable-query-tests--cols)))
    (should (= 1 (length result)))
    (should (equal "Java CheatSheet" (plist-get (car result) :title)))))

(deftest "apply-filters -- Title regex filter"
  (let* ((filters '(("Title" . "Emacs")))
         (result  (aq--apply-filters actionable-query-tests--objects filters actionable-query-tests--cols)))
    (should (= 1 (length result)))
    (should (equal "Emacs 30 Released" (plist-get (car result) :title)))))

(deftest "apply-filters -- two filters are ANDed"
  (let* ((filters '(("Date" . "2026-05-07") ("Title" . "Org")))
         (result  (aq--apply-filters actionable-query-tests--objects filters actionable-query-tests--cols)))
    (should (= 1 (length result)))
    (should (equal "Org-mode Tips" (plist-get (car result) :title)))))

(deftest "apply-filters -- regex partial match on year"
  (let* ((filters '(("Date" . "2026")))
         (result  (aq--apply-filters actionable-query-tests--objects filters actionable-query-tests--cols)))
    (should (= 2 (length result)))))

(deftest "apply-filters -- unknown column name: val is nil, all objects pass"
  ;; When the column name does not match any column, val is nil.
  ;; (or (null val) …) is t — every object passes. This is surprising but
  ;; intentional: a missing column is treated as a no-op rather than erroring.
  (let* ((filters '(("NoSuchColumn" . "anything")))
         (result  (aq--apply-filters actionable-query-tests--objects filters actionable-query-tests--cols)))
    (should (= 3 (length result)))))

(deftest "apply-filters -- nil columns arg: all objects pass (regression guard)"
  ;; If cols is nil (the bug we fixed), no column can ever be found, so val is
  ;; always nil, and every object passes the filter — the silent no-op bug.
  (let* ((filters '(("Date" . "2026-05-07")))
         (result  (aq--apply-filters actionable-query-tests--objects filters nil)))
    (should (= 3 (length result)))))

;;; ─── §9 · aq--apply-filter-to-view ───────────────────────────────────────

;; Helpers for building test buffers.

(defun actionable-query-tests--make-flat-view (view-name objects)
  "Create a buffer with a single vtable of OBJECTS; register in cache.
Returns the buffer.  Caller is responsible for killing it."
  (let ((buf (generate-new-buffer (format " *actionable-query-test-%s*" view-name))))
    (with-current-buffer buf
      (org-agenda-mode)
      (let ((inhibit-read-only t))
        (make-vtable :objects objects
                     :columns actionable-query-tests--cols))
      (setq aq--active-filters nil
            aq--all-objects    objects
            aq--total-objects  (length objects))
      (puthash view-name objects aq--object-cache))
    buf))

(defun actionable-query-tests--make-grouped-view (view-name grouped-plist)
  "Create a buffer with grouped vtables from GROUPED-PLIST; register in cache.
Returns the buffer.  Caller is responsible for killing it."
  (let ((buf (generate-new-buffer (format " *actionable-query-test-%s*" view-name))))
    (with-current-buffer buf
      (org-agenda-mode)
      (let ((inhibit-read-only t)
            (cols (aq--coerce-columns actionable-query-rss-columns))
            (alist (cl-loop for (k v) on grouped-plist by #'cddr collect (cons k v))))
        (aq--render-grouped alist cols nil nil view-name))
      (setq aq--active-filters nil
            aq--all-objects    (apply #'append
                                         (mapcar #'cdr (cl-loop for (k v) on grouped-plist by #'cddr collect (cons k v))))
            aq--total-objects  (length aq--all-objects))
      (puthash view-name grouped-plist aq--object-cache))
    buf))

(defaqtest "apply-filter-to-view flat -- filter reduces objects in vtable"
  (let* ((vn  "test/flat")
         (buf (actionable-query-tests--make-flat-view vn actionable-query-tests--objects)))
    (unwind-protect
        (with-current-buffer buf
          (setq aq--active-filters '(("Date" . "2026-05-07")))
          (aq--apply-filter-to-view vn)
          (should (= 2 (length (vtable-objects (vtable-current-table))))))
      (kill-buffer buf))))

(defaqtest "apply-filter-to-view flat -- nil filters restores all objects"
  (let* ((vn  "test/flat-clear")
         (buf (actionable-query-tests--make-flat-view vn actionable-query-tests--objects)))
    (unwind-protect
        (with-current-buffer buf
          ;; First filter down…
          (setq aq--active-filters '(("Date" . "2026-05-07")))
          (aq--apply-filter-to-view vn)
          ;; …then clear.
          (setq aq--active-filters nil)
          (aq--apply-filter-to-view vn)
          (should (= 3 (length (vtable-objects (vtable-current-table))))))
      (kill-buffer buf))))

(defun actionable-query-tests--collect-tables (buf)
  "Return list of all vtable structs in BUF in buffer order."
  (with-current-buffer buf
    (save-excursion
      (goto-char (point-min))
      (let (acc)
        (while (not (eobp))
          (if-let ((tbl (vtable-current-table)))
              (progn (push tbl acc) (vtable-end-of-table))
            (forward-char 1)))
        (nreverse acc)))))

(defaqtest "apply-filter-to-view grouped -- filter applies per-group"
  ;; Capture table structs before calling apply-filter-to-view.
  ;; setf mutates vtable-objects in place, so the same struct refs reflect
  ;; the filtered result even if vtable-revert can't redisplay in batch mode.
  (let* ((vn     "test/grouped")
         (gp     (list "Alpha" (list actionable-query-tests--rss-obj-a actionable-query-tests--rss-obj-c)
                       "Beta"  (list actionable-query-tests--rss-obj-b)))
         (buf    (actionable-query-tests--make-grouped-view vn gp))
         (tables (actionable-query-tests--collect-tables buf)))
    (unwind-protect
        (with-current-buffer buf
          (setq aq--active-filters '(("Date" . "2026-05-07")))
          (goto-char (point-min))
          (ignore-errors (aq--apply-filter-to-view vn))
          ;; Alpha: rss-obj-a matches (2026-05-07), rss-obj-c does not (2023-12-24) → 1
          ;; Beta:  rss-obj-b matches (2026-05-07) → 1
          (should (= 1 (length (vtable-objects (nth 0 tables)))))
          (should (= 1 (length (vtable-objects (nth 1 tables))))))
      (kill-buffer buf))))

(defaqtest "apply-filter-to-view grouped -- point-min regression: cols not nil"
  ;; The original bug: cols was fetched via (vtable-current-table) before
  ;; goto-char point-min. At point-min the buffer starts with a newline/title
  ;; line — vtable-current-table returns nil there, so cols was nil, and every
  ;; object silently passed the filter (all 3 instead of 2).
  (let* ((vn     "test/grouped-colsbug")
         (gp     (list "G" (list actionable-query-tests--rss-obj-a
                                 actionable-query-tests--rss-obj-b
                                 actionable-query-tests--rss-obj-c)))
         (buf    (actionable-query-tests--make-grouped-view vn gp))
         (tables (actionable-query-tests--collect-tables buf)))
    (unwind-protect
        (with-current-buffer buf
          (goto-char (point-min))
          (should-not (vtable-current-table))   ; confirm we're outside any vtable
          (setq aq--active-filters '(("Date" . "2026-05-07")))
          (ignore-errors (aq--apply-filter-to-view vn))
          ;; 2 of 3 objects match — NOT 3 (the cols=nil regression).
          (should (= 2 (length (vtable-objects (car tables))))))
      (kill-buffer buf))))

(defaqtest "apply-filter-to-view grouped -- nil filters restores all objects"
  (let* ((vn     "test/grouped-clear")
         (gp     (list "G" actionable-query-tests--objects))
         (buf    (actionable-query-tests--make-grouped-view vn gp))
         (tables (actionable-query-tests--collect-tables buf)))
    (unwind-protect
        (with-current-buffer buf
          (setq aq--active-filters '(("Date" . "2026-05-07")))
          (ignore-errors (aq--apply-filter-to-view vn))
          (setq aq--active-filters nil)
          (ignore-errors (aq--apply-filter-to-view vn))
          (should (= 3 (length (vtable-objects (car tables))))))
      (kill-buffer buf))))

;;; ─── §10 · aq--dismiss / dismissed-items / undismiss-all ─────────────────

(defaqtest "dismiss -- forever key is literally \"forever\""
  (aq--dismiss "test-view" "id-1" 'forever)
  (let ((entries (aq--dismissed-for-view "test-view")))
    (should (cl-some (lambda (e) (equal "forever" (cdar e))) entries))))

(defaqtest "dismiss -- tomorrow key matches today's date"
  (aq--dismiss "test-view" "id-1" 'tomorrow)
  (let* ((entries (aq--dismissed-for-view "test-view"))
         (key     (cdar (car entries))))
    (should (string-match-p "^[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}$" key))))

(defaqtest "dismissed-items -- collects ids across expiry keys"
  (aq--dismiss "v" "id-1" 'tomorrow)
  (aq--dismiss "v" "id-2" 'forever)
  (should (member "id-1" (aq--dismissed-items "v")))
  (should (member "id-2" (aq--dismissed-items "v"))))

(defaqtest "undismiss-all -- clears all entries for view"
  (aq--dismiss "v" "id-1" 'tomorrow)
  (aq--dismiss "v" "id-2" 'forever)
  (aq--undismiss-all "v")
  (should (null (aq--dismissed-items "v"))))

(defaqtest "undismiss-all -- does not affect other views"
  (aq--dismiss "v1" "id-1" 'tomorrow)
  (aq--dismiss "v2" "id-2" 'tomorrow)
  (aq--undismiss-all "v1")
  (should (member "id-2" (aq--dismissed-items "v2"))))

;;; ─── §11 · aq--show-loading / aq--stop-loading ────────────────────────

(deftest "show-loading -- buffer contains hourglass glyph"
  (let ((buf (generate-new-buffer " *actionable-query-test-loading*")))
    (unwind-protect
        (progn
          (aq--show-loading buf)
          (with-current-buffer buf
            (should (string-match-p "[⏳⌛]" (buffer-string)))))
      (aq--stop-loading buf)
      (kill-buffer buf))))

(deftest "show-loading -- loading timer is installed"
  (let ((buf (generate-new-buffer " *actionable-query-test-loading2*")))
    (unwind-protect
        (progn
          (aq--show-loading buf)
          (with-current-buffer buf
            (should (timerp aq--loading-timer))))
      (aq--stop-loading buf)
      (kill-buffer buf))))

(deftest "stop-loading -- timer is nil after stop"
  (let ((buf (generate-new-buffer " *actionable-query-test-loading3*")))
    (unwind-protect
        (progn
          (aq--show-loading buf)
          (aq--stop-loading buf)
          (with-current-buffer buf
            (should (null aq--loading-timer))))
      (kill-buffer buf))))

(deftest "stop-loading -- idempotent when no timer is running"
  (let ((buf (generate-new-buffer " *actionable-query-test-loading4*")))
    (unwind-protect
        (with-current-buffer buf
          (setq aq--loading-timer nil)
          (should (null (aq--stop-loading buf))))  ; no error
      (kill-buffer buf))))

(deftest "show-loading -- calling twice cancels first timer, no leak"
  (let ((buf (generate-new-buffer " *actionable-query-test-loading5*")))
    (unwind-protect
        (progn
          (aq--show-loading buf)
          (let ((first-timer (with-current-buffer buf aq--loading-timer)))
            (aq--show-loading buf)
            (with-current-buffer buf
              ;; First timer was cancelled; a new one is active.
              (should (timerp aq--loading-timer))
              (should-not (eq first-timer aq--loading-timer)))))
      (aq--stop-loading buf)
      (kill-buffer buf))))

;;; ─── §11½ · async splice placeholders ──────────────────────────────────────
;;
;; The placeholder primitive moved to `~/actionable-query/point-async/'
;; ---a self-contained slot-passing reservation library with no AQ
;; coupling.  Generic regression coverage (slot ordering, prose
;; interleaving, sync vs truly-async resolves, deadline failure) lives
;; in `point-async/point-async-tests.el'.  AQ-side integration is
;; exercised through the `sym-form' tests below ---e.g.
;; `(sym t) inserts cached content at point in current buffer'.

;;; ─── §12 · aq--parse-refresh-interval ────────────────────────────────────

(deftest "parse-refresh-interval -- 5 minutes"
  (should (= 300 (aq--parse-refresh-interval "5 minutes"))))

(deftest "parse-refresh-interval -- 1 minute singular"
  (should (= 60 (aq--parse-refresh-interval "1 minute"))))

(deftest "parse-refresh-interval -- 30 minutes"
  (should (= 1800 (aq--parse-refresh-interval "30 minutes"))))

(deftest "parse-refresh-interval -- 1 hour"
  (should (= 3600 (aq--parse-refresh-interval "1 hour"))))

(deftest "parse-refresh-interval -- 2 hours"
  (should (= 7200 (aq--parse-refresh-interval "2 hours"))))

(deftest "parse-refresh-interval -- 1 day"
  (should (= 86400 (aq--parse-refresh-interval "1 day"))))

(deftest "parse-refresh-interval -- unrecognised string returns nil"
  (should (null (aq--parse-refresh-interval "every tuesday"))))

(deftest "parse-refresh-interval -- nil returns nil"
  (should (null (aq--parse-refresh-interval nil))))

;;; ─── end-to-end helpers ─────────────────────────────────────────────────────

(defun actionable-query-tests--open-view (view-name objects)
  "Register VIEW-NAME delivering OBJECTS synchronously; open it; return buffer.
OBJECTS may be a flat list or a grouped plist (\"Group\" (items…) …).
The :objects-async callback fires immediately — no network, no timers.
Note: `actionable-query-defview' itself auto-pops the buffer at expansion
time, so we do not need a separate `funcall' here."
  (eval `(actionable-query-defview ,view-name
           :objects (lambda (cb) (funcall cb ',objects))
           :columns actionable-query-rss-columns
           :actions '()))
  (get-buffer (format "%s%s*" org-ql-view-buffer-name-prefix view-name)))

(defun actionable-query-tests--normalise-buffer-string (s)
  "Strip trailing whitespace per line and collapse runs of blank lines."
  (let* ((s (replace-regexp-in-string "[ \t]+\n" "\n" s))
         (s (replace-regexp-in-string "\n\\{3,\\}" "\n\n" s)))
    (string-trim s)))

;;; ─── fixture: define-view-test (flat, 3 objects) ──────────────────────────────────

(snap-define-fixture define-view-test
  "Open a flat actionable-query view pre-loaded with 3 test RSS objects; body runs inside buffer."
  (let* ((old-dismissed (copy-hash-table aq--dismissed))
         (old-cache     (copy-hash-table aq--object-cache))
         (view-name     "test/actionable-query-e2e")
         (buf           (actionable-query-tests--open-view view-name actionable-query-tests--objects)))
    (unwind-protect
        (with-current-buffer buf &body)
      (when (buffer-live-p buf) (kill-buffer buf))
      (setq org-ql-views (assoc-delete-all view-name org-ql-views #'string=))
      (setq aq--dismissed  old-dismissed
            aq--object-cache old-cache))))

(deftest "A grouped actionable-query view (Today: obj-a obj-b, Older: obj-c) has multiple tables and filters apply to each one"
  (let* ((old-dismissed (copy-hash-table aq--dismissed))
         (old-cache     (copy-hash-table aq--object-cache))
         (view-name     "test/actionable-query-e2e-grouped")
         (grouped       (list "Today"
                              (list actionable-query-tests--rss-obj-a actionable-query-tests--rss-obj-b)
                              "Older"
                              (list actionable-query-tests--rss-obj-c)))
         (buf           (actionable-query-tests--open-view view-name grouped)))
    (unwind-protect        
        (let ((actual (with-current-buffer buf
                        (actionable-query-tests--normalise-buffer-string
                         (buffer-substring-no-properties (point-min) (point-max))))))
          ;; ✔ There are two vtables
          ;; ✔ Group titles “Today” and “Older” appear
          ;; ✔ 
          (should (snap-equal-modulo
                   "Today
Date  Title  Category
2026-05-07 Emacs 30 Released emacs
2026-05-07 Org-mode Tips org, emacs

Older
Date  Title  Category
2023-12-24 Java CheatSheet java

3 unread
Last fetched at 8:03:12AM (took 3ms) — press `g' to refresh."
                   actual
                   '("Last fetched at .* — press `g' to refresh.")))

          ;; Apply filters -- Date=2026-05-07 applies per group"
          (let ((tables (with-current-buffer buf
                          (setq aq--active-filters '(("Date" . "2026-05-07")))
                          (goto-char (point-min))
                          (ignore-errors (aq--apply-filter-to-view "test/actionable-query-e2e-grouped"))
                          ;; (actionable-query-refresh-current-view) ;; FIXME: This fails for some reason!
                          ;; (actionable-query-tests--normalise-buffer-string (buffer-substring-no-properties (point-min) (point-max)))
                          (actionable-query-tests--collect-tables (current-buffer)))))
            ;; Today group: obj-a (2026-05-07) + obj-b (2026-05-07) → both match → 2
            ;; Older group: obj-c (2023-12-24) → does not match → 0... but group has 1 item
            ;; With filter, Older group gets 0; Today stays at 2.
            (should (= 2 (length (vtable-objects (nth 0 tables)))))
            (should (= 0 (length (vtable-objects (nth 1 tables)))))
            ;;
            ))
      (when (buffer-live-p buf) (kill-buffer buf))
      (setq org-ql-views (assoc-delete-all view-name org-ql-views #'string=))
      (setq aq--dismissed  old-dismissed
            aq--object-cache old-cache))))

;;; ─── snap-define-relation: actionable-query-view ───────────────────────────────────────

(snap-define-relation actionable-query-view (objects actions expected-view modulo)
                      "Open a flat actionable-query view delivering OBJECTS, execute ACTIONS, compare buffer to EXPECTED-VIEW.

OBJECTS and ACTIONS are passed as unevaluated forms by snap-define-relation, so this body
evals them.  EXPECTED-VIEW is a normalised buffer-string snapshot; C-u C-x C-e fills it.
MODULO is an optional regex or list of regexes tolerated during comparison
(see `snap-equal-modulo') — useful for neutralising drifting substrings like
the per-run \"Last fetched at H:MM:SSpm (took Nms)\" footer."
  (let* ((old-dismissed (copy-hash-table aq--dismissed))
         (old-cache     (copy-hash-table aq--object-cache))
         (view-name     "test/actionable-query-relation")
         ;; snap-define-relation quotes params verbatim — eval to get the actual values.
         (real-objects  (eval objects))
         (real-actions  (eval actions))
         (buf           (actionable-query-tests--open-view view-name real-objects)))
    (unwind-protect
        (progn
          (with-current-buffer buf
            (dolist (action real-actions)
              (funcall action view-name buf)))
          (let ((actual (with-current-buffer buf
                          (actionable-query-tests--normalise-buffer-string
                           (buffer-substring-no-properties (point-min) (point-max))))))
            (should (if modulo
                        (snap-equal-modulo expected-view actual modulo)
                      (equal expected-view actual)))
            (list :expected-view actual)))
      (when (buffer-live-p buf) (kill-buffer buf))
      (setq org-ql-views (assoc-delete-all view-name org-ql-views #'string=))
      (setq aq--dismissed  old-dismissed
            aq--object-cache old-cache))))

;;; ─── §E1 · Opening & basic rendering ───────────────────────────────────────

(define-view-test "open flat view -- vtable has 3 objects"
  (let ((tbl (vtable-current-table)))
    (should tbl)
    (should (= 3 (length (vtable-objects tbl))))))

(define-view-test "open flat view -- loading timer is nil after deliver"
  (should (null aq--loading-timer)))

(define-view-test "open flat view -- buffer contains Last-fetched footer"
  (should (string-match-p "Last fetched" (buffer-string))))

(define-view-test "open flat view -- cache populated for view name"
  (should (gethash "test/actionable-query-e2e" aq--object-cache)))

(define-view-test "open flat view -- total-objects is 3"
  (should (= 3 aq--total-objects)))

;;; ─── §E2 · Column filtering workflows ──────────────────────────────────────

(define-view-test "filter flat -- Date=2026-05-07 reduces vtable to 2 rows"
  (setq aq--active-filters '(("Date" . "2026-05-07")))
  (aq--apply-filter-to-view "test/actionable-query-e2e")
  (should (= 2 (length (vtable-objects (vtable-current-table))))))

(define-view-test "filter flat -- clearing filter restores all 3 rows"
  (setq aq--active-filters '(("Date" . "2026-05-07")))
  (aq--apply-filter-to-view "test/actionable-query-e2e")
  (setq aq--active-filters nil)
  (aq--apply-filter-to-view "test/actionable-query-e2e")
  (should (= 3 (length (vtable-objects (vtable-current-table))))))

(define-view-test "filter flat -- Title=Emacs reduces vtable to 1 row"
  (setq aq--active-filters '(("Title" . "Emacs")))
  (aq--apply-filter-to-view "test/actionable-query-e2e")
  (should (= 1 (length (vtable-objects (vtable-current-table))))))

;;; ─── §E3 · Snooze / dismissal ───────────────────────────────────────────────

(define-view-test "snooze -- vtable shrinks to 2 rows after dismissing obj-a"
  (let* ((tbl   (vtable-current-table))
         (obj-a (car (vtable-objects tbl))))
    (aq--dismiss "test/actionable-query-e2e" (aq--obj-id obj-a) 'tomorrow)
    (vtable-remove-object tbl obj-a)
    (should (= 2 (length (vtable-objects tbl))))))

(define-view-test "snooze -- dismissed id is recorded"
  (let* ((tbl   (vtable-current-table))
         (obj-a (car (vtable-objects tbl)))
         (id    (aq--obj-id obj-a)))
    (aq--dismiss "test/actionable-query-e2e" id 'tomorrow)
    (should (member id (aq--dismissed-items "test/actionable-query-e2e")))))

(define-view-test "snooze then re-open -- dismissed row absent in fresh view"
  ;; Dismiss obj-a, then re-open the same view (cache hit).
  ;; The deliver branch filters dismissed items, so obj-a should not appear.
  (let* ((tbl   (vtable-current-table))
         (obj-a (car (vtable-objects tbl)))
         (id    (aq--obj-id obj-a))
         (vn    "test/actionable-query-e2e"))
    (aq--dismiss vn id 'tomorrow)
    ;; Re-open the view (re-uses cache).
    (funcall (alist-get vn org-ql-views nil nil #'string=))
    (let* ((buf2 (get-buffer (format "%s%s*" org-ql-view-buffer-name-prefix vn)))
           (tbl2 (with-current-buffer buf2 (vtable-current-table))))
      (should (= 2 (length (vtable-objects tbl2))))
      (should-not (member id (mapcar #'aq--obj-id (vtable-objects tbl2)))))))

(define-view-test "undismiss-all -- all 3 rows present after undismiss"
  (let* ((tbl   (vtable-current-table))
         (obj-a (car (vtable-objects tbl)))
         (vn    "test/actionable-query-e2e"))
    ;; Snooze, then clear.
    (aq--dismiss vn (aq--obj-id obj-a) 'tomorrow)
    (aq--undismiss-all vn)
    ;; Re-open — all objects should flow through.
    (funcall (alist-get vn org-ql-views nil nil #'string=))
    (let* ((buf2 (get-buffer (format "%s%s*" org-ql-view-buffer-name-prefix vn)))
           (tbl2 (with-current-buffer buf2 (vtable-current-table))))
      (should (= 3 (length (vtable-objects tbl2)))))))

;;; ─── §E5 · Snapshot tests (define-actionable-query-view-test) ─────────────────────────
;;
;; These tests use snap-define-relation — designed for interactive use.
;; With :expected-view "" they will FAIL until you populate the snapshot:
;;   1. Load this file: M-x eval-buffer (or C-c C-l)
;;   2. Place point inside a define-actionable-query-view-test form
;;   3. C-u C-x C-e  — computes actual buffer and rewrites :expected-view in source
;;
;; The "Last fetched at HH:MM" footer changes each run; actionable-query-tests--normalise-buffer-string
;; strips trailing whitespace but NOT the timestamp — so snapshots need a stable footer.
;; Use C-u C-x C-e to capture the snapshot at a specific time and accept the footer as-is.

(define-actionable-query-view-test "flat view with 3 objects -- full buffer snapshot"
  :objects (list '(:title "Emacs 30 Released" :url "https://example.com/emacs30"
                          :date "Thu, 07 May 2026 10:00:00 +0000" :description "Big release." :categories ("emacs"))
                 '(:title "Org-mode Tips" :url "https://example.com/org"
                          :date "Thu, 07 May 2026 18:00:00 +0000" :description "Org tips." :categories ("org" "emacs"))
                 '(:title "Java CheatSheet" :url "https://example.com/java"
                          :date "Sun, 24 Dec 2023 12:00:00 +0000" :description "Java reference." :categories ("java")))
  :actions (list)
  :expected-view "Date  Title  Category
2026-05-07 Emacs 30 Released emacs
2026-05-07 Org-mode Tips org, emacs
2023-12-24 Java CheatSheet java

3 unread
Last fetched at 4:21:21PM (took 1ms) — press `g' to refresh."
  :modulo ("Last fetched at [^—]* — press"))

(define-actionable-query-view-test "flat view filtered by Date=2026-05-07 -- snapshot shows 2 rows"
:objects (list '(:title "Emacs 30 Released" :url "https://example.com/emacs30"
                        :date "Thu, 07 May 2026 10:00:00 +0000" :description "Big release." :categories ("emacs"))
               '(:title "Org-mode Tips" :url "https://example.com/org"
                        :date "Thu, 07 May 2026 18:00:00 +0000" :description "Org tips." :categories ("org" "emacs"))
               '(:title "Java CheatSheet" :url "https://example.com/java"
                        :date "Sun, 24 Dec 2023 12:00:00 +0000" :description "Java reference." :categories ("java")))
:actions (list (lambda (vn _buf)
                 (setq aq--active-filters '(("Date" . "2026-05-07")))
                 (aq--apply-filter-to-view vn)))
:expected-view "Date  Title  Category
2026-05-07 Emacs 30 Released emacs
2026-05-07 Org-mode Tips org, emacs

3 unread — filtered by: Date=2026-05-07 (C-u = to clear)")

;;; ─── §E6 · Slow-fetch threshold guard ──────────────────────────────────────
;;
;; The threshold logic lives in two places:
;;   1. aq--make-deliver persists elapsed → aq--last-elapsed-cache after delivery.
;;   2. The actionable-query-defview macro reads that cache on the next open; if
;;      elapsed > actionable-query-auto-fetch-slow-threshold it skips the fetch.
;;
;; We test both sides without real timers: seed aq--last-elapsed-cache directly
;; and observe whether the async-fn fires on the second open.

(defmacro actionable-query-tests--with-threshold (threshold &rest body)
  "Run BODY with `actionable-query-auto-fetch-slow-threshold' bound to THRESHOLD."
  (declare (indent 1))
  `(let ((actionable-query-auto-fetch-slow-threshold ,threshold))
     ,@body))

(defun actionable-query-tests--open-counting (view-name counter-cell)
  "Register VIEW-NAME with an async :objects fn that increments (car COUNTER-CELL) on each call.
Returns the view buffer.  Caller is responsible for killing it and cleaning up `org-ql-views'."
  (let ((objects-fn (lambda (cb)
                      (cl-incf (car counter-cell))
                      (funcall cb (list actionable-query-tests--rss-obj-a)))))
    (setf (alist-get view-name org-ql-views nil nil #'string=)
          (lambda (&optional _insert-mode)
            (interactive)
            (let* ((bufname (format "%s%s*" org-ql-view-buffer-name-prefix view-name))
                   (buf     (get-buffer-create bufname))
                   (snooze  'tomorrow)
                   (actions (aq--augment-actions nil view-name snooze))
                   (deliver (aq--make-deliver
                              buf view-name actions snooze
                              (aq--coerce-columns actionable-query-rss-columns)
                              nil nil nil nil nil nil)))
              (with-current-buffer buf
                (setq aq--marked-rows nil aq--active-filters nil
                      aq--all-objects nil aq--total-objects nil)
                (let ((inhibit-read-only t))
                  (erase-buffer)
                  (org-agenda-mode))
                (let* ((threshold  actionable-query-auto-fetch-slow-threshold)
                       (last-secs  (gethash view-name aq--last-elapsed-cache))
                       (too-slow-p (and threshold last-secs (> last-secs threshold))))
                  (if too-slow-p
                      (message "slow")
                    (setq aq--fetch-aborted nil
                          aq--last-fetch-start-time (float-time))
                    (funcall objects-fn deliver))))
              buf))))
  (funcall (alist-get view-name org-ql-views nil nil #'string=))
  (get-buffer (format "%s%s*" org-ql-view-buffer-name-prefix view-name)))

(defaqtest "elapsed-cache -- deliver populates aq--last-elapsed-cache for view"
  ;; Seed start-time far in the past so elapsed is clearly > 0.
  ;; Pass one real object + RSS columns so make-vtable can render without error.
  (let* ((vn  "test/elapsed-populate")
         (buf (generate-new-buffer (format " *aq-test-%s*" vn))))
    (unwind-protect
        (let ((deliver (aq--make-deliver buf vn nil 'tomorrow
                                         actionable-query-rss-columns
                                         nil nil nil nil nil nil)))
          (with-current-buffer buf
            (setq aq--last-fetch-start-time (- (float-time) 2.5)))
          (funcall deliver (list actionable-query-tests--rss-obj-a)))
      (kill-buffer buf))
    (let ((recorded (gethash vn aq--last-elapsed-cache)))
      (should recorded)
      (should (> recorded 2.0)))))

(defaqtest "elapsed-cache -- nil start-time does not overwrite existing entry"
  ;; When a cache-hit deliver fires (start-time is nil), the elapsed entry from
  ;; the real fetch must be preserved.
  (let* ((vn  "test/elapsed-nil-start")
         (buf (generate-new-buffer (format " *aq-test-%s*" vn))))
    (puthash vn 5.0 aq--last-elapsed-cache)
    (unwind-protect
        (let ((deliver (aq--make-deliver buf vn nil 'tomorrow
                                         actionable-query-rss-columns
                                         nil nil nil nil nil nil)))
          (with-current-buffer buf
            (setq aq--last-fetch-start-time nil))   ; cache-hit path
          (funcall deliver (list actionable-query-tests--rss-obj-a)))
      (kill-buffer buf))
    (should (= 5.0 (gethash vn aq--last-elapsed-cache)))))

(snap-define-fixture defcounting-test
  "Reset AQ global state; body runs with `buf', `vn', and `fetches' available.
The test body is responsible for binding `vn' (string) and `fetches' (cons cell)
and calling `actionable-query-tests--open-counting' to populate `buf'.
The fixture saves/restores all AQ global state and `org-ql-views'."
  (let ((old-dismissed  (copy-hash-table aq--dismissed))
        (old-cache      (copy-hash-table aq--object-cache))
        (old-elapsed    (copy-hash-table aq--last-elapsed-cache))
        (old-views      (copy-sequence org-ql-views)))
    (unwind-protect
        (progn
          (clrhash aq--dismissed)
          (clrhash aq--object-cache)
          (clrhash aq--last-elapsed-cache)
          &body)
      (setq aq--dismissed          old-dismissed
            aq--object-cache       old-cache
            aq--last-elapsed-cache old-elapsed
            org-ql-views           old-views))))

(defcounting-test "slow-fetch guard -- first open always fetches (no prior elapsed)"
  (let* ((vn      "test/slow-guard-first")
         (fetches (cons 0 nil))
         (buf     (actionable-query-tests--with-threshold 1.0
                    (actionable-query-tests--open-counting vn fetches))))
    (unwind-protect
        (should (= 1 (car fetches)))
      (when (buffer-live-p buf) (kill-buffer buf)))))

(defcounting-test "slow-fetch guard -- second open skips fetch when elapsed > threshold"
  (let* ((vn      "test/slow-guard-skip")
         (fetches (cons 0 nil))
         (buf     (actionable-query-tests--with-threshold 1.0
                    ;; Seed: last fetch took 3 seconds — above the 1.0 s threshold.
                    (puthash vn 3.0 aq--last-elapsed-cache)
                    (actionable-query-tests--open-counting vn fetches))))
    (unwind-protect
        (should (= 0 (car fetches)))
      (when (buffer-live-p buf) (kill-buffer buf)))))

(defcounting-test "slow-fetch guard -- second open auto-fetches when elapsed < threshold"
  (let* ((vn      "test/slow-guard-fast")
         (fetches (cons 0 nil))
         (buf     (actionable-query-tests--with-threshold 1.0
                    ;; Seed: last fetch was fast — 0.2 s, well under threshold.
                    (puthash vn 0.2 aq--last-elapsed-cache)
                    (actionable-query-tests--open-counting vn fetches))))
    (unwind-protect
        (should (= 1 (car fetches)))
      (when (buffer-live-p buf) (kill-buffer buf)))))

(defcounting-test "slow-fetch guard -- threshold nil disables guard entirely"
  (let* ((vn      "test/slow-guard-nil-threshold")
         (fetches (cons 0 nil))
         (buf     (actionable-query-tests--with-threshold nil
                    ;; Even a very slow prior fetch must not suppress auto-fetch.
                    (puthash vn 999.0 aq--last-elapsed-cache)
                    (actionable-query-tests--open-counting vn fetches))))
    (unwind-protect
        (should (= 1 (car fetches)))
      (when (buffer-live-p buf) (kill-buffer buf)))))

(defcounting-test "slow-fetch guard -- elapsed exactly at threshold still auto-fetches"
  ;; The guard is a strict > comparison; = threshold is not considered slow.
  (let* ((vn      "test/slow-guard-exact")
         (fetches (cons 0 nil))
         (buf     (actionable-query-tests--with-threshold 1.0
                    (puthash vn 1.0 aq--last-elapsed-cache)
                    (actionable-query-tests--open-counting vn fetches))))
    (unwind-protect
        (should (= 1 (car fetches)))
      (when (buffer-live-p buf) (kill-buffer buf)))))

;;; ─── §E7 · Symbol-name form + insert-at-point ──────────────────────────────

(defmacro actionable-query-tests--with-sym-view (sym title objects &rest body)
  "Define a view with symbol SYM, string TITLE, and OBJECTS; run BODY; clean up."
  (declare (indent 3))
  `(let ((old-cache (copy-hash-table aq--object-cache)))
     (unwind-protect
         (progn
           (eval '(actionable-query-defview ,sym ,title
                    :objects (lambda (cb) (funcall cb ',objects))
                    :columns actionable-query-rss-columns
                    :actions '()))
           ,@body)
       (let ((buf (get-buffer (format "%s%s*" org-ql-view-buffer-name-prefix ,title))))
         (when (buffer-live-p buf) (kill-buffer buf)))
       (setq org-ql-views (assoc-delete-all ,title org-ql-views #'string=))
       (fmakunbound ',sym)
       (setq aq--object-cache old-cache))))

(deftest "sym-form -- M-x command is bound after defview"
  (actionable-query-tests--with-sym-view
      aq-test-sym-bound "test/sym-bound" ()
    (should (fboundp 'aq-test-sym-bound))))

(deftest "sym-form -- view is registered in org-ql-views under string title"
  (actionable-query-tests--with-sym-view
      aq-test-sym-registered "test/sym-registered" ()
    (should (assoc "test/sym-registered" org-ql-views #'string=))))

(deftest "sym-form -- calling with no arg opens dedicated buffer"
  (actionable-query-tests--with-sym-view
      aq-test-sym-open "test/sym-open" ((:title "row-a"))
    (aq-test-sym-open)
    (let ((buf (get-buffer (format "%s%s*" org-ql-view-buffer-name-prefix "test/sym-open"))))
      (should (buffer-live-p buf)))))

(deftest "sym-form -- (sym t) inserts cached content at point in current buffer"
  (actionable-query-tests--with-sym-view
      aq-test-sym-insert "test/sym-insert" ((:title "hello-row"))
    ;; First open to populate the cache.
    (aq-test-sym-insert)
    ;; Now insert into a scratch buffer.
    (let ((scratch (generate-new-buffer " *aq-test-insert-target*")))
      (unwind-protect
          (with-current-buffer scratch
            (insert "BEFORE\n")
            (aq-test-sym-insert :insert t)
            (let ((content (buffer-substring-no-properties (point-min) (point-max))))
              (should (string-match-p "BEFORE" content))
              ;; The spliced view must contain something (not just "BEFORE").
              (should (> (length content) (length "BEFORE\n")))))
        (kill-buffer scratch)))))

(deftest "sym-form -- string-only form still works (backward compat)"
  (let ((old-cache (copy-hash-table aq--object-cache)))
    (unwind-protect
        (progn
          (eval '(actionable-query-defview "test/str-compat"
                   :objects '()
                   :actions '()))
          (should (assoc "test/str-compat" org-ql-views #'string=)))
      (setq org-ql-views (assoc-delete-all "test/str-compat" org-ql-views #'string=))
      (setq aq--object-cache old-cache))))

;; The macro at actionable-query.el:2026 calls `aq--splice-view-into' with
;; three positional args plus four `&key' arguments
;; (`:help-echo-fn', `:view-name', `:actions', `:async-fn').  If the
;; helper drifts to fewer required args (e.g. an interactive debug stub
;; gets accidentally saved), every `(view t)' invocation explodes with
;; `wrong-number-of-arguments' — exactly the regression that motivated
;; this test.  `cl-defun' with `&key' compiles down to (REQUIRED .
;; many), so we pin to that.
(deftest "splice-view-into -- arity matches macro call site"
  (let ((arity (func-arity #'aq--splice-view-into)))
    (should (equal arity '(3 . many)))))

;; End-to-end regression for the synchronous splice path.  The existing
;; test at line 788 uses the async-callback `:objects' form, whose
;; arity errors hide inside the deliver hook.  Here we use a literal
;; `:objects' list, which goes through `aq--splice-view-into'
;; *synchronously* from the macro body — the path `(my/first-view t)'
;; in `org-mode' actually takes.  We assert: no error, content inserted,
;; and the per-region `aq-region-ctx' attached as a text-property.
(deftest "sym-form -- sync :objects + (sym t) splices into org-mode buffer"
  (let ((old-cache (copy-hash-table aq--object-cache)))
    (unwind-protect
        (progn
          (eval '(actionable-query-defview aq-test-sync-splice "test/sync-splice"
                   :objects '((:title "sync-row-α") (:title "sync-row-β"))
                   :columns actionable-query-rss-columns
                   :actions '()))
          ;; Warm the cache by opening the dedicated buffer once.
          (aq-test-sync-splice)
          (with-temp-buffer
            (org-mode)
            (insert "BEFORE\n")
            (let ((before-len (- (point-max) (point-min))))
              (should-not
               (condition-case err
                   (progn (aq-test-sync-splice :insert t) nil)
                 (error err)))
              (should (> (- (point-max) (point-min)) before-len)))
            ;; The splice begins at (point-min) + length("BEFORE\n"); read
            ;; the ctx from one char past that boundary to land squarely
            ;; inside the spliced region.
            (let* ((splice-start (+ (point-min) (length "BEFORE\n")))
                   (ctx (get-text-property splice-start 'aq--region-ctx)))
              (should (aq-region-ctx-p ctx))
              (should (equal (aq-region-ctx-view-name ctx) "test/sync-splice")))))
      (let ((buf (get-buffer (format "%stest/sync-splice*"
                                     org-ql-view-buffer-name-prefix))))
        (when (buffer-live-p buf) (kill-buffer buf)))
      (setq org-ql-views (assoc-delete-all "test/sync-splice" org-ql-views #'string=))
      (when (fboundp 'aq-test-sync-splice) (fmakunbound 'aq-test-sync-splice))
      (setq aq--object-cache old-cache))))

(defvar aq-tests--ret-fired nil
  "Scratch var threaded through `eval' scope in the RET-in-org-mode test.")

(deftest "sym-form -- RET action fires in org-mode spliced region"
  ;; Regression: aq--install-host-action-keys had a broken fallback that caused
  ;; infinite recursion, resolving to a newline insert instead of the action.
  ;; `aq-tests--ret-fired' is a plain defvar so the lambda inside `eval' can
  ;; set it without fighting lexical-binding scope rules.
  (let ((old-cache (copy-hash-table aq--object-cache)))
    (setq aq-tests--ret-fired nil)
    (unwind-protect
        (progn
          (eval '(actionable-query-defview aq-test-ret-org "test/ret-org"
                   :objects '("row-α" "row-β")
                   :actions '(("RET" "Fire"
                                (lambda (it) (setq aq-tests--ret-fired it))))))
          (aq-test-ret-org)
          (with-temp-buffer
            (org-mode)
            (insert "BEFORE\n")
            (aq-test-ret-org :insert t)
            ;; Land point on the first vtable data row.
            (goto-char (point-min))
            (let ((match (text-property-search-forward 'vtable-object nil nil)))
              (should match)
              (goto-char (prop-match-beginning match)))
            ;; Simulate RET via the overriding-map binding directly —
            ;; execute-kbd-macro bypasses minor-mode-overriding-map-alist in batch.
            (let* ((entry (assoc 'aq--host-actions-active minor-mode-overriding-map-alist))
                   (fn    (and entry (lookup-key (cdr entry) (kbd "RET")))))
              (should (commandp fn))
              (call-interactively fn))
            (should aq-tests--ret-fired)
            (should (equal aq-tests--ret-fired "row-α"))))
      (let ((buf (get-buffer (format "%stest/ret-org*" org-ql-view-buffer-name-prefix))))
        (when (buffer-live-p buf) (kill-buffer buf)))
      (setq org-ql-views (assoc-delete-all "test/ret-org" org-ql-views #'string=))
      (when (fboundp 'aq-test-ret-org) (fmakunbound 'aq-test-ret-org))
      (setq aq--object-cache old-cache))))

;;; ─── · aq--eval-last-sexp ───────────────────────────────────────────────────

;; Regression: `aq--eval-last-sexp' used `(cadr form)' unconditionally to
;; extract the view name, which broke two of the three legal calling forms:
;;   (defview SYM "Title" …) → cadr is a symbol, not a string → org-ql-view
;;                             received a symbol → opened *rg QL View: nil*
;;   (defview :keyword …)    → cadr is a keyword → org-ql-view silently
;;                             no-oped → view never shown after C-x C-e
;; The fix branches on (cadr form)'s type, mirroring the macro's own parsing.

(defun actionable-query-tests--eval-last-sexp-name (form)
  "Return the view-name that `aq--eval-last-sexp' would extract from FORM.
Inserts FORM into a temp buffer, positions point at end, then reads the
`view-name' binding via the advice logic directly — without actually calling
`org-ql-view' or eval-ing the defview form."
  (with-temp-buffer
    (emacs-lisp-mode)
    (insert (prin1-to-string form))
    ;; The advice does (backward-sexp) then (sexp-at-point), so point must be
    ;; at/after the closing paren.
    (goto-char (point-max))
    (let (result)
      (save-excursion
        (backward-sexp)
        (when-let* ((f    (sexp-at-point))
                    (_    (eq (car-safe f) 'actionable-query-defview))
                    (head (cadr f)))
          (setq result
                (cond
                 ((and (symbolp head) (not (keywordp head))) (caddr f))
                 ((stringp head) head)
                 (t nil)))))
      result)))

(deftest "aq--eval-last-sexp -- string form extracts title correctly"
  (should (equal (actionable-query-tests--eval-last-sexp-name
                  '(actionable-query-defview "My first view" :objects '("one")))
                 "My first view")))

(deftest "aq--eval-last-sexp -- sym+string form extracts title, not symbol"
  ;; Regression: previously returned the symbol `my/first-view', causing
  ;; org-ql-view to open *rg QL View: nil* instead of the real buffer.
  (should (equal (actionable-query-tests--eval-last-sexp-name
                  '(actionable-query-defview my/first-view "My second view" :objects '("one")))
                 "My second view")))

(deftest "aq--eval-last-sexp -- keyword-only (anonymous) form returns nil name"
  ;; The anonymous form has no string title; the advice correctly returns nil
  ;; so `org-ql-view' is never called (the macro's own `funcall view-fn'
  ;; opens the buffer directly — no advice-side open needed).
  ;; Regression: previously returned `:objects', causing a spurious
  ;; org-ql-view call that silently no-oped and left the view unopened.
  (should (null (actionable-query-tests--eval-last-sexp-name
                 '(actionable-query-defview :objects '("one"))))))

(deftest "anonymous defview -- C-x C-e opens a buffer (not nil)"
  ;; Regression: the macro's final `(funcall view-fn)' was guarded by
  ;; `(when name …)', so anonymous forms silently returned nil on C-x C-e
  ;; instead of opening their *aq-anon* buffer.
  (let ((bufs-before (mapcar #'buffer-name (buffer-list))))
    (unwind-protect
        (progn
          (eval '(actionable-query-defview :objects '("one" "two" "three")))
          (let ((anon-buf (cl-find-if (lambda (b)
                                        (and (not (member (buffer-name b) bufs-before))
                                             (string-prefix-p "*aq-anon*" (buffer-name b))))
                                      (buffer-list))))
            (should (buffer-live-p anon-buf))))
      (dolist (b (buffer-list))
        (when (and (not (member (buffer-name b) bufs-before))
                   (string-prefix-p "*aq-anon*" (buffer-name b)))
          (kill-buffer b))))))

;;; ─── §E8 · aq--center-vtable-headers ───────────────────────────────────────

(defun aq-tests--header-text-at-col (buf index)
  "Return the header cell string for column INDEX in BUF, stripped of display props.
Finds the region carrying `vtable-column' INDEX and returns its plain text."
  (with-current-buffer buf
    (save-excursion
      (goto-char (point-min))
      (when-let* ((match (text-property-search-forward 'vtable-column index #'eql)))
        (buffer-substring-no-properties
         (prop-match-beginning match)
         (prop-match-end match))))))

(defun aq-tests--header-left-pad-px (buf index)
  "Return the pixel width of the leading display-space in the header cell for column INDEX."
  (with-current-buffer buf
    (save-excursion
      (goto-char (point-min))
      (when-let* ((match (text-property-search-forward 'vtable-column index #'eql))
                  (start (prop-match-beginning match))
                  (disp  (get-text-property start 'display)))
        ;; disp is (space :width (N)) — extract N
        (when (and (listp disp) (eq (car disp) 'space))
          (car (plist-get (cdr disp) :width)))))))

(deftest "center-vtable-headers -- centered column has symmetric leading pad"
  ;; A centered column should have a non-zero left pad (i.e. the advice fired).
  (let* ((col  (make-vtable-column :name "Age" :width 10 :align 'center
                                   :getter (lambda (o &rest _) (format "%s" o))))
         (buf  (generate-new-buffer " *aq-center-test*")))
    (unwind-protect
        (with-current-buffer buf
          (let ((inhibit-read-only t))
            (make-vtable :objects '("x") :columns (list col)
                         :use-header-line nil))
          (let ((pad (aq-tests--header-left-pad-px buf 0)))
            (should (numberp pad))
            (should (> pad 0))))
      (kill-buffer buf))))

(deftest "center-vtable-headers -- left-aligned column has zero leading pad"
  ;; A left-aligned column must not be touched by the advice.
  (let* ((col (make-vtable-column :name "Subject" :width 40 :align 'left
                                  :getter (lambda (o &rest _) (format "%s" o))))
         (buf (generate-new-buffer " *aq-center-test-left*")))
    (unwind-protect
        (with-current-buffer buf
          (let ((inhibit-read-only t))
            (make-vtable :objects '("x") :columns (list col)
                         :use-header-line nil))
          (let ((pad (aq-tests--header-left-pad-px buf 0)))
            ;; No leading display-space inserted — helper returns nil.
            (should (null pad))))
      (kill-buffer buf))))

(deftest "center-vtable-headers -- column name text is preserved after centering"
  (let* ((col (make-vtable-column :name "Jira" :width 12 :align 'center
                                  :getter (lambda (o &rest _) (format "%s" o))))
         (buf (generate-new-buffer " *aq-center-test-name*")))
    (unwind-protect
        (with-current-buffer buf
          (let ((inhibit-read-only t))
            (make-vtable :objects '("x") :columns (list col)
                         :use-header-line nil))
          (let ((text (aq-tests--header-text-at-col buf 0)))
            (should (string-match-p "Jira" text))))
      (kill-buffer buf))))

;;; actionable-query-tests.el ends here
