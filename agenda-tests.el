;;; agenda-tests.el --- ERT tests for agenda.el  -*- lexical-binding: t; -*-
;;
;; Run batch:
;;   emacs --batch -L ~/actionable-query \
;;         $(find ~/.emacs.d/elpa -maxdepth 1 -mindepth 1 -type d | sed 's/^/-L /') \
;;         --eval '(require (quote savehist))' \
;;         -l agenda-tests.el -f ert-run-tests-batch-and-exit
;;
;; Run interactively: M-x ert after loading this file.

(require 'cl-lib)
(require 'savehist)
(require 'org)
(require 'org-clock)
(load (expand-file-name "~/snap/snap.el") nil t t)
(snap-define-fixture deftest)
(load (expand-file-name "~/actionable-query/actionable-query.el") nil t t)
;; agenda.el is required by actionable-query.el; load explicitly for clarity.
(load (expand-file-name "~/actionable-query/agenda.el") nil t t)

;;; ─── fixture helpers ─────────────────────────────────────────────────────────

(defmacro agenda-tests--with-notes-file (&rest body)
  "Execute BODY with `org-default-notes-file' pointing at a fresh temp file.
The temp file is deleted on exit regardless of errors."
  (declare (indent 0))
  (let ((f (gensym "notes-file")))
    `(let* ((,f (make-temp-file "agenda-tests-notes-" nil ".org"))
            (org-default-notes-file ,f))
       (unwind-protect
           (progn ,@body)
         (when (file-exists-p ,f) (delete-file ,f))))))

(defmacro agenda-tests--with-org-buf (content &rest body)
  "Create a temp Org buffer with CONTENT, evaluate BODY inside it, then kill."
  (declare (indent 1))
  `(let ((buf (generate-new-buffer " *agenda-tests-org*")))
     (unwind-protect
         (with-current-buffer buf
           (org-mode)
           (insert ,content)
           (goto-char (point-min))
           ,@body)
       (kill-buffer buf))))

(defmacro agenda-tests--with-view-buf (&rest body)
  "Create a minimal actionable-query buffer in org-agenda-mode; run BODY inside it."
  (declare (indent 0))
  `(let ((buf (generate-new-buffer " *agenda-tests-view*")))
     (unwind-protect
         (with-current-buffer buf
           (org-agenda-mode)
           ,@body)
       (kill-buffer buf))))

;;; ─── aq-notify-macos ─────────────────────────────────────────────────────────

(deftest "aq-notify-macos -- calls osascript with title and body"
  ;; We mock call-process to capture args rather than actually shelling out.
  (let (captured-args)
    (cl-letf (((symbol-function 'call-process)
               (lambda (&rest args) (setq captured-args args) 0)))
      (aq-notify-macos "My View" "Ready!")
      (should (equal "osascript" (car captured-args)))
      (should (cl-some (lambda (a) (and (stringp a) (string-match-p "My View" a)))
                       captured-args))
      (should (cl-some (lambda (a) (and (stringp a) (string-match-p "Ready!" a)))
                       captured-args)))))

(deftest "aq-notify-macos -- body defaults to empty string when omitted"
  (let (captured-args)
    (cl-letf (((symbol-function 'call-process)
               (lambda (&rest args) (setq captured-args args) 0)))
      (aq-notify-macos "Title Only")
      ;; The -e script arg must contain the title.
      (should (cl-some (lambda (a) (and (stringp a) (string-match-p "Title Only" a)))
                       captured-args)))))

;;; ─── aq-agenda--current-marker ───────────────────────────────────────────────

(deftest "aq-agenda--current-marker -- returns org-hd-marker when present"
  (agenda-tests--with-view-buf
    (let* ((buf (current-buffer))
           (m   (with-temp-buffer (point-min-marker))))
      (let ((inhibit-read-only t))
        (insert "row\n")
        (put-text-property (line-beginning-position 0) (line-end-position 0)
                           'org-hd-marker m))
      (goto-char (point-min))
      (should (equal m (aq-agenda--current-marker))))))

(deftest "aq-agenda--current-marker -- falls back to org-marker"
  (agenda-tests--with-view-buf
    (let* ((m (with-temp-buffer (point-min-marker))))
      (let ((inhibit-read-only t))
        (insert "row\n")
        (put-text-property (line-beginning-position 0) (line-end-position 0)
                           'org-marker m))
      (goto-char (point-min))
      (should (equal m (aq-agenda--current-marker))))))

(deftest "aq-agenda--current-marker -- nil when no marker on line"
  (agenda-tests--with-view-buf
    (let ((inhibit-read-only t))
      (insert "plain row\n"))
    (goto-char (point-min))
    (should (null (aq-agenda--current-marker)))))

;;; ─── aq-agenda--marker-or-error ─────────────────────────────────────────────

(deftest "aq-agenda--marker-or-error -- signals user-error when no marker"
  (agenda-tests--with-view-buf
    (let ((inhibit-read-only t))
      (insert "no-marker row\n"))
    (goto-char (point-min))
    (should-error (aq-agenda--marker-or-error) :type 'user-error)))

(deftest "aq-agenda--marker-or-error -- returns marker when present"
  (agenda-tests--with-view-buf
    (let* ((m (with-temp-buffer (point-min-marker))))
      (let ((inhibit-read-only t))
        (insert "row\n")
        (put-text-property (line-beginning-position 0) (line-end-position 0)
                           'org-hd-marker m))
      (goto-char (point-min))
      (should (equal m (aq-agenda--marker-or-error))))))

;;; ─── aq-agenda--ensure-marker ────────────────────────────────────────────────

(deftest "ensure-marker -- finds existing heading in notes file"
  (agenda-tests--with-notes-file
    (with-current-buffer (find-file-noselect org-default-notes-file)
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert "* TODO My Ticket ABC-123\n  :PROPERTIES:\n  :END:\n")
        (save-buffer)))
    (let* ((obj '(:id "ABC-123"))
           (m   (aq-agenda--ensure-marker obj "ABC-123")))
      (should (markerp m))
      (should (marker-buffer m))
      (with-current-buffer (marker-buffer m)
        (goto-char m)
        (should (string-match-p "ABC-123" (org-get-heading t t t t)))))))

(deftest "ensure-marker -- creates heading when none exists"
  (agenda-tests--with-notes-file
    ;; Notes file starts empty.
    (let* ((obj '(:id "NEW-999"))
           (m   (aq-agenda--ensure-marker obj "NEW-999")))
      (should (markerp m))
      (should (marker-buffer m))
      (with-current-buffer (marker-buffer m)
        (goto-char m)
        (should (string-match-p "NEW-999" (org-get-heading t t t t)))))))

(deftest "ensure-marker -- created heading has CREATED property"
  (agenda-tests--with-notes-file
    (let* ((obj '(:id "X-1"))
           (m   (aq-agenda--ensure-marker obj "X-1")))
      (with-current-buffer (marker-buffer m)
        (goto-char m)
        (should (org-entry-get (point) "CREATED"))))))

(deftest "ensure-marker -- created heading is a TODO"
  (agenda-tests--with-notes-file
    (let* ((obj '(:id "X-2"))
           (m   (aq-agenda--ensure-marker obj "X-2")))
      (with-current-buffer (marker-buffer m)
        (goto-char m)
        (should (equal "TODO" (org-get-todo-state)))))))

;;; ─── aq-agenda-show-new-time ─────────────────────────────────────────────────

(deftest "show-new-time -- display property set on matching line"
  (agenda-tests--with-view-buf
    (let* ((m (with-temp-buffer (point-min-marker))))
      (let ((inhibit-read-only t))
        (insert "some row text\n")
        (put-text-property (line-beginning-position 0) (line-end-position 0)
                           'org-marker m))
      (goto-char (point-min))
      (aq-agenda-show-new-time m "<2026-05-17 Sun>" " S")
      ;; The display property should appear somewhere on the line.
      (goto-char (line-beginning-position))
      (let ((found nil))
        (while (< (point) (line-end-position))
          (when (get-text-property (point) 'display)
            (setq found t))
          (forward-char 1))
        (should found)))))

(deftest "show-new-time -- display text contains the stamp"
  (agenda-tests--with-view-buf
    (let* ((m (with-temp-buffer (point-min-marker)))
           (stamp "<2026-05-17 Sun>"))
      (let ((inhibit-read-only t))
        (insert "row\n")
        (put-text-property (line-beginning-position 0) (line-end-position 0)
                           'org-marker m))
      (goto-char (point-min))
      (aq-agenda-show-new-time m stamp " S")
      ;; Find the display property value anywhere on the line and check its content.
      (goto-char (line-beginning-position))
      (let ((display-val nil))
        (while (and (< (point) (line-end-position)) (null display-val))
          (setq display-val (get-text-property (point) 'display))
          (forward-char 1))
        (should (stringp display-val))
        (should (string-match-p (regexp-quote stamp) display-val))))))

(deftest "show-new-time -- no-op when no line carries the marker"
  ;; Should not signal an error even when the marker matches nothing.
  (agenda-tests--with-view-buf
    (let* ((m (with-temp-buffer (point-min-marker))))
      (let ((inhibit-read-only t))
        (insert "untagged row\n"))
      (goto-char (point-min))
      ;; Should complete without error:
      (should (null (aq-agenda-show-new-time m "<2026-05-17 Sun>" " S"))))))

;;; ─── aq-abort-fetch ──────────────────────────────────────────────────────────

(deftest "aq-abort-fetch -- sets aq--fetch-aborted flag"
  (agenda-tests--with-view-buf
    (setq aq--fetch-aborted nil)
    (aq--show-loading (current-buffer))
    (let ((inhibit-read-only t))
      (aq-abort-fetch))
    (should aq--fetch-aborted)))

(deftest "aq-abort-fetch -- stops the loading timer"
  (agenda-tests--with-view-buf
    (setq aq--fetch-aborted nil)
    (aq--show-loading (current-buffer))
    (should (timerp aq--loading-timer))
    (let ((inhibit-read-only t))
      (aq-abort-fetch))
    (should (null aq--loading-timer))))

(deftest "aq-abort-fetch -- buffer shows abort message"
  (agenda-tests--with-view-buf
    (setq aq--fetch-aborted nil)
    (aq--show-loading (current-buffer))
    (let ((inhibit-read-only t))
      (aq-abort-fetch))
    (should (string-match-p "aborted" (buffer-string)))))

(deftest "deliver after abort -- no-ops, buffer left showing abort message"
  ;; Simulate: abort mid-flight, then deliver fires anyway.
  (let* ((buf (generate-new-buffer " *agenda-tests-abort*"))
         (delivered nil))
    (unwind-protect
        (with-current-buffer buf
          (org-agenda-mode)
          (setq aq--fetch-aborted nil)
          (aq--show-loading buf)
          ;; Build a real deliver closure for this buffer (minimal args).
          (let ((deliver (aq--make-deliver
                          buf "test/abort" '() 'forever
                          nil nil nil nil '() nil nil)))
            ;; Abort first.
            (let ((inhibit-read-only t))
              (aq-abort-fetch))
            (should aq--fetch-aborted)
            ;; Now fire the deliver — it should no-op.
            (funcall deliver (list "object-1" "object-2"))
            ;; aq--all-objects should NOT have been set (deliver was skipped).
            (should (null aq--all-objects))
            ;; Buffer should still show the abort message, not a vtable.
            (should (string-match-p "aborted" (buffer-string)))))
      (kill-buffer buf))))

;;; ─── :async-notifier integration ────────────────────────────────────────────

(defvar agenda-tests--notifier-fired nil
  "Set to t by the :async-notifier in the notifier integration test.")

(deftest ":async-notifier fires after async delivery"
  (let* ((old-cache     (copy-hash-table aq--object-cache))
         (old-dismissed (copy-hash-table aq--dismissed))
         (vn "test/notifier"))
    (setq agenda-tests--notifier-fired nil)
    (unwind-protect
        (progn
          (eval '(actionable-query-defview "test/notifier"
                   :objects (lambda (cb) (funcall cb '("a" "b")))
                   :columns '((:name "X" :width 10 :getter (lambda (o &rest _) o)))
                   :actions '()
                   :async-notifier (setq agenda-tests--notifier-fired t)))
          (funcall (alist-get vn org-ql-views nil nil #'string=))
          (should agenda-tests--notifier-fired))
      (setq org-ql-views (assoc-delete-all vn org-ql-views #'string=))
      (setq aq--object-cache old-cache
            aq--dismissed    old-dismissed))))

;;; ─── aq-agenda-install-keys ──────────────────────────────────────────────────

(deftest "aq-agenda-install-keys -- binds I to aq-agenda-clock-in"
  (agenda-tests--with-view-buf
    (aq-agenda-install-keys)
    (should (eq 'aq-agenda-clock-in
                (lookup-key (current-local-map) (kbd "I"))))))

(deftest "aq-agenda-install-keys -- binds t to aq-agenda-todo"
  (agenda-tests--with-view-buf
    (aq-agenda-install-keys)
    (should (eq 'aq-agenda-todo
                (lookup-key (current-local-map) (kbd "t"))))))

(deftest "aq-agenda-install-keys -- binds C-c C-s to aq-agenda-schedule"
  (agenda-tests--with-view-buf
    (aq-agenda-install-keys)
    (should (eq 'aq-agenda-schedule
                (lookup-key (current-local-map) (kbd "C-c C-s"))))))

(deftest "aq-agenda-install-keys -- binds C-c C-d to aq-agenda-deadline"
  (agenda-tests--with-view-buf
    (aq-agenda-install-keys)
    (should (eq 'aq-agenda-deadline
                (lookup-key (current-local-map) (kbd "C-c C-d"))))))

(deftest "aq-agenda-install-keys -- binds comma to aq-agenda-set-priority"
  (agenda-tests--with-view-buf
    (aq-agenda-install-keys)
    (should (eq 'aq-agenda-set-priority
                (lookup-key (current-local-map) (kbd ","))))))

(deftest "aq-agenda-install-keys -- binds E to aq-agenda-set-effort"
  (agenda-tests--with-view-buf
    (aq-agenda-install-keys)
    (should (eq 'aq-agenda-set-effort
                (lookup-key (current-local-map) (kbd "E"))))))

;;; ─── Q abort key wired by install-standard-hooks ────────────────────────────

(deftest "Q key is bound to aq-abort-fetch in org views"
  ;; aq--install-standard-hooks wires Q unconditionally; verify via a real view.
  (let* ((old-cache     (copy-hash-table aq--object-cache))
         (old-dismissed (copy-hash-table aq--dismissed))
         (vn "test/q-key"))
    (unwind-protect
        (progn
          (eval `(actionable-query-defview ,vn
                   :objects '("x")
                   :columns '((:name "C" :width 5 :getter (lambda (o &rest _) o)))
                   :actions '()))
          (funcall (alist-get vn org-ql-views nil nil #'string=))
          (let ((buf (get-buffer (format "%s%s*" org-ql-view-buffer-name-prefix vn))))
            (should buf)
            (with-current-buffer buf
              (should (eq 'aq-abort-fetch
                          (lookup-key (current-local-map) (kbd "Q")))))))
      (setq org-ql-views    (assoc-delete-all vn org-ql-views #'string=))
      (setq aq--object-cache old-cache
            aq--dismissed    old-dismissed))))

;;; ─── :org-ql preset keyword ──────────────────────────────────────────────────

(deftest ":org-ql preset is registered"
  (should (gethash :org-ql aq--preset-keywords)))

(deftest ":org-ql preset -- expands to plist with :objects :org :columns :actions"
  (let* ((preset (gethash :org-ql aq--preset-keywords))
         (result (funcall (aq--preset-fn preset) '(todo "TODO"))))
    (should (plist-member result :objects))
    (should (plist-member result :org))
    (should (plist-member result :columns))
    (should (plist-member result :actions))))

(deftest ":org-ql preset -- :objects is a 0-arg lambda"
  (let* ((preset  (gethash :org-ql aq--preset-keywords))
         (result  (funcall (aq--preset-fn preset) '(todo "TODO")))
         (obj-fn  (plist-get result :objects)))
    (should (functionp obj-fn))
    ;; 0-arg: max arity is 0 (not 'many, not ≥ 1).
    (should (= 0 (cdr (func-arity obj-fn))))))

(deftest ":org-ql preset -- :org is a 1-arg lambda"
  (let* ((preset (gethash :org-ql aq--preset-keywords))
         (result (funcall (aq--preset-fn preset) '(todo "TODO")))
         (org-fn (plist-get result :org)))
    (should (functionp org-fn))
    (should (= 1 (cdr (func-arity org-fn))))))

(deftest ":org-ql preset -- :actions is non-nil"
  (let* ((preset  (gethash :org-ql aq--preset-keywords))
         (result  (funcall (aq--preset-fn preset) '(todo "TODO"))))
    (should (plist-get result :actions))))

(deftest ":org-ql preset -- :columns default has 5 entries"
  ;; aq-org-ql-default-columns: TODO, Pri, Headline, Scheduled, Tags.
  (should (= 5 (length aq-org-ql-default-columns))))

(deftest ":org-ql preset -- :columns includes Headline column"
  (should (cl-some (lambda (col) (equal "Headline" (plist-get col :name)))
                   aq-org-ql-default-columns)))

(deftest ":org-ql preset -- :columns includes TODO column"
  (should (cl-some (lambda (col) (equal "TODO" (plist-get col :name)))
                   aq-org-ql-default-columns)))

;;; ─── aq-agenda command integration (against real Org buffers) ───────────────

(deftest "aq-agenda-todo -- cycles TODO state on linked heading"
  (agenda-tests--with-notes-file
    (with-current-buffer (find-file-noselect org-default-notes-file)
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert "* TODO Fix the bug\n")
        (save-buffer)))
    (let* ((notes-buf (find-file-noselect org-default-notes-file))
           (m (with-current-buffer notes-buf
                (goto-char (point-min))
                (re-search-forward "^\\* " nil t)
                (org-back-to-heading t)
                (point-marker))))
      (agenda-tests--with-view-buf
        (let ((inhibit-read-only t))
          (insert "row\n")
          (put-text-property (line-beginning-position 0) (line-end-position 0)
                             'org-hd-marker m))
        (goto-char (point-min))
        ;; Intercept vtable-revert so it doesn't blow up outside a real vtable.
        (cl-letf (((symbol-function 'vtable-current-table) (lambda () nil)))
          (aq-agenda-todo nil))
        ;; The heading's TODO state should now have advanced.
        (with-current-buffer (marker-buffer m)
          (goto-char m)
          ;; After one cycle from TODO: DONE (or next keyword; just not TODO anymore).
          (should-not (equal "TODO" (org-get-todo-state))))))))

(deftest "aq-agenda-clock-out -- signals when no running clock"
  (cl-letf (((symbol-function 'org-clocking-p) (lambda () nil)))
    (should-error (aq-agenda-clock-out) :type 'user-error)))

(deftest "aq-agenda-schedule -- shows new-time overlay after scheduling"
  (agenda-tests--with-notes-file
    (with-current-buffer (find-file-noselect org-default-notes-file)
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert "* TODO Schedule me\n")
        (save-buffer)))
    (let* ((notes-buf (find-file-noselect org-default-notes-file))
           (m (with-current-buffer notes-buf
                (goto-char (point-min))
                (re-search-forward "^\\* " nil t)
                (org-back-to-heading t)
                (point-marker)))
           (overlay-set nil))
      (agenda-tests--with-view-buf
        (let ((inhibit-read-only t))
          (insert "row text here\n")
          (put-text-property (line-beginning-position 0) (line-end-position 0)
                             'org-hd-marker m))
        (goto-char (point-min))
        ;; Mock org-schedule to return a timestamp string without prompting.
        ;; Mock show-new-time to record that it was called.
        (cl-letf (((symbol-function 'org-schedule)
                   (lambda (_arg) "<2026-05-17 Sun>"))
                  ((symbol-function 'aq-agenda-show-new-time)
                   (lambda (_m _ts _pfx) (setq overlay-set t)))
                  ((symbol-function 'vtable-current-table) (lambda () nil)))
          (aq-agenda-schedule nil))
        (should overlay-set)))))

(provide 'agenda-tests)
;;; agenda-tests.el ends here
