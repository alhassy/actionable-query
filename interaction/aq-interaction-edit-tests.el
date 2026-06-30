;;; aq-interaction-edit-tests.el --- ERT tests for aq-interaction-edit.el  -*- lexical-binding: t; -*-
;;
;; Run batch:
;;   emacs --batch -L . -L render -L data -L state -L interaction -L point-async -L ~/snap \
;;         --eval '(package-initialize)' \
;;         -l actionable-query.el -l interaction/aq-interaction-edit-tests.el \
;;         -f ert-run-tests-batch-and-exit
;;
;; Run interactively: M-x ert after loading this file.
;;
;; Exercises the editable-cell feature end to end: define a Celsius/
;; Fahrenheit view with one `:editable' column, simulate pressing `e'
;; on a cell via `aq--edit-current-cell' (with `read-string' stubbed),
;; and assert the buffer reflects the new value.  A sibling test
;; confirms a non-editable column still refuses the edit.

(require 'cl-lib)
(load (expand-file-name "~/snap/snap.el") nil t t)
(snap-define-fixture deftest)
(load (expand-file-name "~/actionable-query/actionable-query.el") nil t t)

;;; ─── fixture: open the temperature-converter demo view ─────────────────────
;;
;; Mirrors the README "Temperature Converter" example: two rows
;; (Celsius, Fahrenheit), a `:setter' on the "Value" column via
;; `plist-put', and an unrelated unnamed column to prove non-editable
;; columns coexist untouched.

(defconst aq-edit-tests--view-name "test/aq-edit-temperature")

(defun aq-edit-tests--fresh-objects ()
  "Return a fresh two-row plist list --- mirrors the README temperature demo.
Freshly allocated on every call: the `:setter' below uses `plist-put',
which mutates in place, and `aq--object-cache' remembers the list across
runs by view-name --- reusing one `defconst' list would let one test's
edits leak into the next."
  (list (list :label "Celsius"    :value "31"   :unit "°C")
        (list :label "Fahrenheit" :value "87.8" :unit "°F")))

(defun aq-edit-tests--open-view ()
  "Register and open the temperature-converter demo view; return its buffer.
`actionable-query-defview' only *registers* the view in `org-ql-views' ---
opening the buffer needs an explicit `org-ql-view' call, same as `M-x
org-ql-view' would do interactively.  Also clears `aq--object-cache' for
this view-name first, so each test starts from fresh fixture objects
rather than replaying a previous test's mutated ones."
  (remhash aq-edit-tests--view-name aq--object-cache)
  (let ((objects (aq-edit-tests--fresh-objects)))
    (eval
     `(actionable-query-defview ,aq-edit-tests--view-name
        :objects (lambda (cb) (funcall cb ',objects))
        :columns '((:name "" :width 12
                           :getter (lambda (o &rest _) (plist-get o :label)))
                   (:name "Value" :width 8 :align 'right
                          :editable t
                          :getter (lambda (o &rest _) (plist-get o :value))
                          :setter (lambda (o v) (plist-put o :value v)))
                   (:name "" :width 4
                           :getter (lambda (o &rest _) (plist-get o :unit))))
        :actions '())))
  (org-ql-view aq-edit-tests--view-name)
  (get-buffer (format "%s%s*" org-ql-view-buffer-name-prefix aq-edit-tests--view-name)))

(snap-define-fixture define-edit-test
  "Open the temperature-converter view; body runs inside its buffer."
  (let ((buf (aq-edit-tests--open-view)))
    (unwind-protect
        (with-current-buffer buf &body)
      (when (buffer-live-p buf) (kill-buffer buf))
      (setq org-ql-views (assoc-delete-all aq-edit-tests--view-name org-ql-views #'string=))
      (remhash aq-edit-tests--view-name aq--object-cache))))

(defun aq-edit-tests--simulate-edit (search-string new-value)
  "Move point onto the cell containing SEARCH-STRING and edit it to NEW-VALUE.
Stubs `read-string' so the simulated keypress needs no minibuffer input ---
this is the elisp equivalent of pressing `e' and typing NEW-VALUE RET."
  (goto-char (point-min))
  (search-forward search-string)
  (backward-char 1)
  (cl-letf (((symbol-function 'read-string) (lambda (&rest _) new-value)))
    (aq--edit-current-cell)))

;;; ─── §1 · editing the Value column ───────────────────────────────────────────

(define-edit-test "editable cell -- editing Celsius Value updates the buffer"
  (aq-edit-tests--simulate-edit "31" "100")
  (should (string-match-p "Celsius +100 °C" (buffer-string))))

(define-edit-test "editable cell -- editing Fahrenheit Value updates the buffer"
  (aq-edit-tests--simulate-edit "87.8" "0")
  (should (string-match-p "Fahrenheit +0 °F" (buffer-string))))

(define-edit-test "editable cell -- editing one row leaves the other row untouched"
  (aq-edit-tests--simulate-edit "31" "100")
  (should (string-match-p "Celsius +100 °C" (buffer-string)))
  (should (string-match-p "Fahrenheit +87.8 °F" (buffer-string))))

(define-edit-test "editable cell -- setter mutates the underlying object's plist"
  ;; The buffer reflecting the new value is necessary but not sufficient ---
  ;; this confirms the `:setter' actually ran against the row object, since
  ;; a future re-render (e.g. `g') reads from the object, not the old text.
  (aq-edit-tests--simulate-edit "31" "100")
  (let ((celsius-obj (cl-find "Celsius" (vtable-objects (vtable-current-table))
                              :key (lambda (o) (plist-get o :label)) :test #'string=)))
    (should (equal "100" (plist-get celsius-obj :value)))))

;;; ─── §2 · refusing non-editable columns ──────────────────────────────────────

(define-edit-test "editable cell -- editing the Label column signals user-error"
  ;; The "" label column (Celsius / Fahrenheit) carries no `:setter' ---
  ;; `aq--edit-current-cell' must refuse rather than silently no-op, so a
  ;; stray `e' press on the wrong column tells the user clearly.
  (goto-char (point-min))
  (search-forward "Celsius")
  (backward-char 1)
  (should-error (aq--edit-current-cell) :type 'user-error))

(provide 'aq-interaction-edit-tests)
;;; aq-interaction-edit-tests.el ends here
