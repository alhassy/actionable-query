;;; tests.el --- ERT tests for actionable-query  -*- lexical-binding: t; -*-

;;; Commentary:

;; Comprehensive tests for `actionable-query.el', using `snap.el'
;; (`deftest' / `deftestfixture' / `define-relation') as the harness.
;;
;; The tests are organised into eight groups:
;;
;;   1. Registry — `actionable-query--register' upsert semantics.
;;   2. Dismissal cache — per-day item dismissal.
;;   3. Async cache — `actionable-query--async-store' / retrieval.
;;   4. Rendering — sync `:items-fn' path produces correct text & props.
;;   5. show-if — sections with `:show-if-fn nil' are silently skipped.
;;   6. item-to-string — `:item-to-string-fn' converts structs to strings.
;;   7. on-hover — `help-echo' text property is set from `:on-hover-fn'.
;;   8. org-fn — `org-marker' / `org-hd-marker' set when :org-fn returns marker.
;;   9. async rendering — placeholder on cache miss; items on cache hit.
;;  10. defquery macro — keyword → internal plist contract.
;;  11. cache invalidation — `actionable-query-invalidate-cache'.
;;
;; Run with:
;;   emacs --batch -L . -l tests.el -f ert-run-tests-batch-and-exit
;; or interactively: M-x ert RET t RET
;;
;; Depends on `snap' for `deftest' / `deftestfixture' / `define-relation'.

;;; Code:

(require 'actionable-query)
(require 'snap)

;; ─────────────────────────────────────────────────────────────────
;; Fixture: isolated registry + async cache for every test.
;; We save and restore both globals so tests are hermetic.
;; ─────────────────────────────────────────────────────────────────

(snap-define-fixture defaq-test
  "Save & restore registry, caches, and view registry around the test body.
Also resets the dynamic `actionable-query--current-view' to nil so one
test can't leak a named view into another."
  (let ((saved-registry  actionable-query--registry)
        (saved-views     actionable-query-views)
        (saved-dismissed (copy-hash-table actionable-query--dismissed))
        (saved-async     (copy-hash-table actionable-query--async-cache))
        (actionable-query--current-view nil))
    (setq actionable-query--registry nil
          actionable-query-views     nil)
    (clrhash actionable-query--dismissed)
    (clrhash actionable-query--async-cache)
    (unwind-protect
        (progn &body)
      (setq actionable-query--registry saved-registry
            actionable-query-views     saved-views)
      (clrhash actionable-query--dismissed)
      (maphash (lambda (k v) (puthash k v actionable-query--dismissed))
               saved-dismissed)
      (clrhash actionable-query--async-cache)
      (maphash (lambda (k v) (puthash k v actionable-query--async-cache))
               saved-async))))

;; ─────────────────────────────────────────────────────────────────
;; Helper: render a section into a temp buffer, return the text.
;; ─────────────────────────────────────────────────────────────────

(defun aq-test--render (section)
  "Insert SECTION into a fresh buffer and return the buffer string."
  (with-temp-buffer
    (let ((inhibit-read-only t))
      (actionable-query--insert section)
      (buffer-string))))

(defun aq-test--render-all ()
  "Insert all registered sections and return the buffer string."
  (with-temp-buffer
    (let ((inhibit-read-only t))
      (actionable-query--insert-all)
      (buffer-string))))

(defun aq-test--props-at-line (text section-text prop)
  "In a rendered buffer containing SECTION-TEXT, return PROP at item line 1."
  (with-temp-buffer
    (let ((inhibit-read-only t))
      (insert text)
      ;; Find first numbered item line.
      (goto-char (point-min))
      (re-search-forward "^1\\. " nil t)
      (get-text-property (point) prop))))

;; ═══════════════════════════════════════════════════════════════════
;; Group 1 — Registry
;; ═══════════════════════════════════════════════════════════════════

(defaq-test "register stores a section by title" [registry]
  (actionable-query--register :title "Alpha" :items-fn (lambda () '("a")))
  (should (= 1 (length actionable-query--registry)))
  (should (equal "Alpha" (plist-get (car actionable-query--registry) :title))))

(defaq-test "register upserts — second registration replaces first" [registry]
  (actionable-query--register :title "Alpha" :items-fn (lambda () '("v1")))
  (actionable-query--register :title "Alpha" :items-fn (lambda () '("v2")))
  (should (= 1 (length actionable-query--registry)))
  (let ((fn (plist-get (car actionable-query--registry) :items-fn)))
    (should (equal '("v2") (funcall fn)))))

(defaq-test "register preserves declaration order for distinct titles" [registry]
  (actionable-query--register :title "First"  :items-fn (lambda () nil))
  (actionable-query--register :title "Second" :items-fn (lambda () nil))
  (actionable-query--register :title "Third"  :items-fn (lambda () nil))
  ;; Registry is newest-first internally; --insert-all reverses it.
  (let ((titles (mapcar (lambda (s) (plist-get s :title))
                        (reverse actionable-query--registry))))
    (should (equal '("First" "Second" "Third") titles))))

(defaq-test "register tolerates re-registration of multiple distinct sections" [registry]
  (dotimes (i 5)
    (actionable-query--register :title (format "Sec%d" i) :items-fn (lambda () nil)))
  (should (= 5 (length actionable-query--registry))))

;; ═══════════════════════════════════════════════════════════════════
;; Group 2 — Dismissal cache
;; ═══════════════════════════════════════════════════════════════════

(defaq-test "dismissed-p returns nil for unknown item" [dismissal]
  (should-not (actionable-query--dismissed-p "Sec" "item-A")))

(defaq-test "dismiss marks item as dismissed for today" [dismissal]
  (actionable-query--dismiss "Sec" "item-A")
  (should (actionable-query--dismissed-p "Sec" "item-A")))

(defaq-test "dismiss is scoped per title — different titles are independent" [dismissal]
  (actionable-query--dismiss "Alpha" "item-A")
  (should-not (actionable-query--dismissed-p "Beta" "item-A")))

(defaq-test "dismiss is scoped per item — different items are independent" [dismissal]
  (actionable-query--dismiss "Alpha" "item-A")
  (should-not (actionable-query--dismissed-p "Alpha" "item-B")))

(defaq-test "dismissed items are filtered from sync rendering" [dismissal]
  (actionable-query--dismiss "Tasks" "buy milk")
  (actionable-query--register
   :title "Tasks"
   :items-fn (lambda () '("buy milk" "write tests")))
  (let ((text (aq-test--render (car actionable-query--registry))))
    (should-not (string-match-p "buy milk" text))
    (should     (string-match-p "write tests" text))))

(defaq-test "remove-on-return dismisses item and clears it on next render" [dismissal]
  (actionable-query--register
   :title "Tasks"
   :items-fn (lambda () '("task-1" "task-2"))
   :remove-on-return t)
  ;; Manually dismiss task-1 (simulating RET).
  (actionable-query--dismiss "Tasks" "task-1")
  (let ((text (aq-test--render (car actionable-query--registry))))
    (should-not (string-match-p "task-1" text))
    (should     (string-match-p "task-2" text))))

;; ═══════════════════════════════════════════════════════════════════
;; Group 3 — Async cache
;; ═══════════════════════════════════════════════════════════════════

(defaq-test "async-cached-items returns nil before any store" [async-cache]
  (should-not (actionable-query--async-cached-items "Sec")))

(defaq-test "async-store then async-cached-items round-trips correctly" [async-cache]
  (actionable-query--async-store "Sec" '("alpha" "beta"))
  (should (equal '("alpha" "beta")
                 (actionable-query--async-cached-items "Sec"))))

(defaq-test "async-cached-items returns nil for a different title" [async-cache]
  (actionable-query--async-store "Sec-A" '("only-in-A"))
  (should-not (actionable-query--async-cached-items "Sec-B")))

(defaq-test "invalidate-cache with title clears only that section" [async-cache]
  (actionable-query--async-store "Alpha" '("a"))
  (actionable-query--async-store "Beta"  '("b"))
  (actionable-query-invalidate-cache "Alpha")
  (should-not (actionable-query--async-cached-items "Alpha"))
  (should     (equal '("b") (actionable-query--async-cached-items "Beta"))))

(defaq-test "invalidate-cache without title clears all sections" [async-cache]
  (actionable-query--async-store "Alpha" '("a"))
  (actionable-query--async-store "Beta"  '("b"))
  (actionable-query-invalidate-cache)
  (should-not (actionable-query--async-cached-items "Alpha"))
  (should-not (actionable-query--async-cached-items "Beta")))

;; Async cache is per-day — simulate a stale entry by patching its date.
(defaq-test "async-cached-items returns nil when cached date is yesterday" [async-cache]
  (puthash "Sec"
           (list :items '("stale") :date "1970-01-01")
           actionable-query--async-cache)
  (should-not (actionable-query--async-cached-items "Sec")))

;; ═══════════════════════════════════════════════════════════════════
;; Group 4 — Sync rendering
;; ═══════════════════════════════════════════════════════════════════

(defaq-test "sync section renders title with org-agenda-structure face" [rendering]
  (actionable-query--register
   :title "My Section"
   :items-fn (lambda () '("item one")))
  (let* ((text (aq-test--render (car actionable-query--registry)))
         (face (get-text-property 1 'face text)))
    (should (string-match-p "My Section" text))
    (should (eq 'org-agenda-structure face))))

(defaq-test "sync section numbers items from 1" [rendering]
  (actionable-query--register
   :title "Nums"
   :items-fn (lambda () '("alpha" "beta" "gamma")))
  (let ((text (aq-test--render (car actionable-query--registry))))
    (should (string-match-p "^1\\. alpha" text))
    (should (string-match-p "^2\\. beta"  text))
    (should (string-match-p "^3\\. gamma" text))))

(defaq-test "sync section with empty items renders nothing" [rendering]
  (actionable-query--register
   :title "Empty"
   :items-fn (lambda () nil))
  (let ((text (aq-test--render (car actionable-query--registry))))
    (should (string= "" text))))

(defaq-test "insert-all renders sections in declaration order" [rendering]
  (actionable-query--register :title "First"  :items-fn (lambda () '("f")))
  (actionable-query--register :title "Second" :items-fn (lambda () '("s")))
  (let ((text (aq-test--render-all)))
    (should (< (string-match-p "First"  text)
               (string-match-p "Second" text)))))

(defaq-test "actionable-query--item property is set on item lines" [rendering]
  (actionable-query--register
   :title "Props"
   :items-fn (lambda () '("the-item")))
  (let* ((text (aq-test--render (car actionable-query--registry)))
         (stored (aq-test--props-at-line text "Props" 'actionable-query--item)))
    (should (equal "the-item" stored))))

(defaq-test "actionable-query--title property is set on item lines" [rendering]
  (actionable-query--register
   :title "TitleProp"
   :items-fn (lambda () '("item")))
  (let* ((text (aq-test--render (car actionable-query--registry)))
         (stored (aq-test--props-at-line text "TitleProp"
                                          'actionable-query--title)))
    (should (equal "TitleProp" stored))))

;; ═══════════════════════════════════════════════════════════════════
;; Group 5 — show-if
;; ═══════════════════════════════════════════════════════════════════

(defaq-test "show-if nil suppresses the entire section" [show-if]
  (actionable-query--register
   :title "Hidden"
   :items-fn  (lambda () '("should not appear"))
   :show-if-fn (lambda () nil))
  (let ((text (aq-test--render (car actionable-query--registry))))
    (should (string= "" text))))

(defaq-test "show-if t renders the section normally" [show-if]
  (actionable-query--register
   :title "Visible"
   :items-fn  (lambda () '("visible item"))
   :show-if-fn (lambda () t))
  (let ((text (aq-test--render (car actionable-query--registry))))
    (should (string-match-p "visible item" text))))

(defaq-test "show-if is re-evaluated on each render call" [show-if]
  (let ((toggle t))
    (actionable-query--register
     :title "Dynamic"
     :items-fn  (lambda () '("x"))
     :show-if-fn (lambda () toggle))
    (should (string-match-p "x"
             (aq-test--render (car actionable-query--registry))))
    (setq toggle nil)
    (should (string= ""
             (aq-test--render (car actionable-query--registry))))))

(defaq-test "insert-all skips hidden sections between visible ones" [show-if]
  (actionable-query--register
   :title "A" :items-fn (lambda () '("a")) :show-if-fn (lambda () t))
  (actionable-query--register
   :title "B" :items-fn (lambda () '("b")) :show-if-fn (lambda () nil))
  (actionable-query--register
   :title "C" :items-fn (lambda () '("c")) :show-if-fn (lambda () t))
  (let ((text (aq-test--render-all)))
    (should     (string-match-p "a" text))
    (should-not (string-match-p "b" text))
    (should     (string-match-p "c" text))))

;; ═══════════════════════════════════════════════════════════════════
;; Group 6 — item-to-string
;; ═══════════════════════════════════════════════════════════════════

(defaq-test "item-to-string-fn converts struct-like items to display strings" [item-to-string]
  (cl-defstruct aq-test-item label score)
  (actionable-query--register
   :title "Struct Items"
   :items-fn (lambda ()
               (list (make-aq-test-item :label "alpha" :score 42)
                     (make-aq-test-item :label "beta"  :score 7)))
   :item-to-string-fn (lambda (it)
                        (format "%s [%d]"
                                (aq-test-item-label it)
                                (aq-test-item-score it))))
  (let ((text (aq-test--render (car actionable-query--registry))))
    (should (string-match-p "alpha \\[42\\]" text))
    (should (string-match-p "beta \\[7\\]"  text))))

(defaq-test "default item-to-string is identity — items are plain strings" [item-to-string]
  (actionable-query--register
   :title "Plain"
   :items-fn (lambda () '("hello" "world")))
  (let ((text (aq-test--render (car actionable-query--registry))))
    (should (string-match-p "hello" text))
    (should (string-match-p "world" text))))

(defaq-test "item-to-string-fn result appears in the displayed line not the raw item" [item-to-string]
  ;; Raw item is a number; displayed line should show its square.
  (actionable-query--register
   :title "Squares"
   :items-fn (lambda () '(3 4 5))
   :item-to-string-fn (lambda (n) (format "%d²=%d" n (* n n))))
  (let ((text (aq-test--render (car actionable-query--registry))))
    (should (string-match-p "3²=9"  text))
    (should (string-match-p "4²=16" text))
    (should (string-match-p "5²=25" text))))

;; ═══════════════════════════════════════════════════════════════════
;; Group 7 — on-hover
;; ═══════════════════════════════════════════════════════════════════

(defaq-test "on-hover-fn result is stored in help-echo property" [on-hover]
  (actionable-query--register
   :title "Hover"
   :items-fn   (lambda () '("task"))
   :on-hover-fn (lambda (it) (format "Hint for %s" it)))
  (let* ((text (aq-test--render (car actionable-query--registry)))
         (echo (aq-test--props-at-line text "Hover" 'help-echo)))
    (should (equal "Hint for task" echo))))

(defaq-test "on-hover-fn receives the raw item, not the display string" [on-hover]
  ;; Raw item is a number; the hover receives the number, not the string.
  (let (captured)
    (actionable-query--register
     :title "RawHover"
     :items-fn      (lambda () '(99))
     :item-to-string-fn (lambda (n) (format "item-%d" n))
     :on-hover-fn   (lambda (it) (setq captured it) "ok"))
    (aq-test--render (car actionable-query--registry))
    (should (equal 99 captured))))

(defaq-test "without on-hover-fn no help-echo property is set" [on-hover]
  (actionable-query--register
   :title "NoHover"
   :items-fn (lambda () '("task")))
  (let* ((text (aq-test--render (car actionable-query--registry)))
         (echo (aq-test--props-at-line text "NoHover" 'help-echo)))
    (should-not echo)))

;; ═══════════════════════════════════════════════════════════════════
;; Group 8 — org-fn / org-marker
;; ═══════════════════════════════════════════════════════════════════

(defaq-test "org-fn returning a live marker sets org-marker and org-hd-marker" [org-fn]
  (let* ((buf    (get-buffer-create " *aq-test-marker*"))
         (marker (with-current-buffer buf
                   (erase-buffer)
                   (insert "* Heading\n")
                   (goto-char (point-min))
                   (point-marker))))
    (actionable-query--register
     :title   "Marked"
     :items-fn (lambda () '("linked-item"))
     :org-fn   (lambda (_it) marker))
    (let* ((text (aq-test--render (car actionable-query--registry)))
           (om   (aq-test--props-at-line text "Marked" 'org-marker))
           (ohm  (aq-test--props-at-line text "Marked" 'org-hd-marker)))
      (should (equal marker om))
      (should (equal marker ohm)))
    (kill-buffer buf)))

(defaq-test "org-fn returning nil sets neither org-marker nor org-hd-marker" [org-fn]
  (actionable-query--register
   :title   "Unmarked"
   :items-fn (lambda () '("plain"))
   :org-fn   (lambda (_it) nil))
  (let* ((text (aq-test--render (car actionable-query--registry)))
         (om   (aq-test--props-at-line text "Unmarked" 'org-marker)))
    (should-not om)))

(defaq-test "org-fn with dead marker sets neither property" [org-fn]
  (let* ((buf    (get-buffer-create " *aq-test-dead*"))
         (marker (with-current-buffer buf (point-marker))))
    (kill-buffer buf) ; marker is now dead
    (actionable-query--register
     :title   "Dead"
     :items-fn (lambda () '("item"))
     :org-fn   (lambda (_it) marker))
    (let* ((text (aq-test--render (car actionable-query--registry)))
           (om   (aq-test--props-at-line text "Dead" 'org-marker)))
      (should-not om))))

;; ═══════════════════════════════════════════════════════════════════
;; Group 9 — Async rendering
;; ═══════════════════════════════════════════════════════════════════

(defaq-test "async section shows placeholder when cache is empty" [async]
  (let (callback-received)
    (actionable-query--register
     :title "Async"
     :items-async-fn (lambda (cb) (setq callback-received cb)))
    (let ((text (aq-test--render (car actionable-query--registry))))
      (should (string-match-p "⏳" text))
      (should (string-match-p "Async" text))
      ;; The async fn was called and received a callback.
      (should (functionp callback-received)))))

(defaq-test "async section renders items after cache is populated" [async]
  (actionable-query--async-store "Async-Hit" '("item-one" "item-two"))
  (actionable-query--register
   :title "Async-Hit"
   :items-async-fn (lambda (_cb) (error "Should not be called when cache is warm")))
  (let ((text (aq-test--render (car actionable-query--registry))))
    (should-not (string-match-p "⏳" text))
    (should     (string-match-p "item-one" text))
    (should     (string-match-p "item-two" text))))

(defaq-test "async section callback stores items and they render on subsequent call" [async]
  (let (stored-cb)
    (actionable-query--register
     :title "Async-CB"
     :items-async-fn (lambda (cb) (setq stored-cb cb)))
    ;; First render: placeholder.
    (aq-test--render (car actionable-query--registry))
    ;; Simulate async completion.
    (funcall stored-cb '("arrived"))
    ;; Second render: items from cache.
    (let ((text (aq-test--render (car actionable-query--registry))))
      (should (string-match-p "arrived" text)))))

(defaq-test "async section with show-if nil shows nothing even on cache hit" [async]
  (actionable-query--async-store "Async-Hidden" '("secret"))
  (actionable-query--register
   :title "Async-Hidden"
   :items-async-fn (lambda (_cb) nil)
   :show-if-fn (lambda () nil))
  (let ((text (aq-test--render (car actionable-query--registry))))
    (should (string= "" text))))

;; ═══════════════════════════════════════════════════════════════════
;; Group 10 — defquery macro
;; ═══════════════════════════════════════════════════════════════════

(defaq-test "macro registers a section with the given title" [macro]
  (defquery
    :title "Macro Title"
    :items '("x"))
  (let ((reg (car actionable-query--registry)))
    (should (equal "Macro Title" (plist-get reg :title)))))

(defaq-test "macro :items wraps form in items-fn" [macro]
  (defquery
    :title "Macro Items"
    :items (list "a" "b"))
  (let* ((reg (car actionable-query--registry))
         (fn  (plist-get reg :items-fn)))
    (should (functionp fn))
    (should (equal '("a" "b") (funcall fn)))))

(defaq-test "macro :on-return wraps form in on-return-fn with it bound" [macro]
  (let (last-it)
    (defquery
      :title "Macro OnReturn"
      :items '("q")
      :on-return (setq last-it it))
    (let* ((reg (car actionable-query--registry))
           (fn  (plist-get reg :on-return-fn)))
      (should (functionp fn))
      (funcall fn "the-item")
      (should (equal "the-item" last-it)))))

(defaq-test "macro :on-hover wraps form in on-hover-fn with it bound" [macro]
  (defquery
    :title "Macro Hover"
    :items '("q")
    :on-hover (format "tip:%s" it))
  (let* ((reg (car actionable-query--registry))
         (fn  (plist-get reg :on-hover-fn)))
    (should (equal "tip:the-item" (funcall fn "the-item")))))

(defaq-test "macro :org wraps form in org-fn with it bound" [macro]
  (defquery
    :title "Macro Org"
    :items '("q")
    :org (when (stringp it) (intern it)))
  (let* ((reg (car actionable-query--registry))
         (fn  (plist-get reg :org-fn)))
    (should (eq 'q (funcall fn "q")))))

(defaq-test "macro :item-to-string wraps form in item-to-string-fn with it bound" [macro]
  (defquery
    :title "Macro I2S"
    :items '(42)
    :item-to-string (number-to-string it))
  (let* ((reg (car actionable-query--registry))
         (fn  (plist-get reg :item-to-string-fn)))
    (should (equal "42" (funcall fn 42)))))

(defaq-test "macro :show-if t registers show-if-fn returning t" [macro]
  (defquery
    :title "Macro ShowT"
    :items '("x")
    :show-if t)
  (let* ((reg (car actionable-query--registry))
         (fn  (plist-get reg :show-if-fn)))
    (should (funcall fn))))

(defaq-test "macro :show-if nil registers show-if-fn returning nil" [macro]
  (defquery
    :title "Macro ShowNil"
    :items '("x")
    :show-if nil)
  (let* ((reg (car actionable-query--registry))
         (fn  (plist-get reg :show-if-fn)))
    (should-not (funcall fn))))

(defaq-test "macro without :show-if defaults to always-visible" [macro]
  (defquery
    :title "Macro ShowDefault"
    :items '("x"))
  (let* ((reg (car actionable-query--registry))
         (fn  (plist-get reg :show-if-fn)))
    (should (funcall fn))))

(defaq-test "macro :items-async wraps form in items-async-fn with callback in scope" [macro]
  (let (got-callback)
    (defquery
      :title "Macro Async"
      :items-async (setq got-callback callback))
    (let* ((reg (car actionable-query--registry))
           (fn  (plist-get reg :items-async-fn)))
      (should (functionp fn))
      (funcall fn #'identity)
      (should (eq #'identity got-callback)))))

(defaq-test "macro :remove-item-on-return stores the flag" [macro]
  (defquery
    :title "Macro Remove"
    :items '("x")
    :remove-item-on-return t)
  (let ((reg (car actionable-query--registry)))
    (should (plist-get reg :remove-on-return))))

;; ═══════════════════════════════════════════════════════════════════
;; Group 11 — define-relation snapshot: section → rendered text
;; ═══════════════════════════════════════════════════════════════════

(snap-define-relation aq-section (title items item-to-string-fn hover-fn show-if-fn rendered)
  "Verify a section with TITLE and ITEMS renders to RENDERED.
ITEM-TO-STRING-FN, HOVER-FN, and SHOW-IF-FN are optional lambdas
(pass nil to use defaults).  Output key is `:rendered'."
  (let ((section
         (list :title            title
               :items-fn         (lambda () items)
               :item-to-string-fn (or item-to-string-fn #'identity)
               :on-hover-fn      hover-fn
               :show-if-fn       (or show-if-fn (lambda () t)))))
    (let ((actual (with-temp-buffer
                    (let ((inhibit-read-only t))
                      (actionable-query--insert section)
                      (buffer-string)))))
      (should (equal rendered actual))
      (list :rendered actual))))

;; Note: `snap-define-relation' auto-quotes each parameter value, so we
;; pass raw literals (no leading `'`).  Function values go through
;; `function' for the same reason.

(define-aq-section-test "plain string items numbered correctly" [aq-section]
  :title            "Work"
  :items            ("Buy milk" "Write tests" "Ship it")
  :item-to-string-fn nil
  :hover-fn          nil
  :show-if-fn        nil
  :rendered "
Work
1. Buy milk
2. Write tests
3. Ship it
")

(define-aq-section-test "item-to-string transforms items before display" [aq-section]
  :title            "Squares"
  :items            (2 3 4)
  :item-to-string-fn (lambda (n) (format "%d²=%d" n (* n n)))
  :hover-fn          nil
  :show-if-fn        nil
  :rendered "
Squares
1. 2²=4
2. 3²=9
3. 4²=16
")

(define-aq-section-test "show-if nil produces empty output" [aq-section]
  :title            "Hidden"
  :items            ("secret")
  :item-to-string-fn nil
  :hover-fn          nil
  :show-if-fn        (lambda () nil)
  :rendered "")

(define-aq-section-test "empty items list produces empty output" [aq-section]
  :title            "Empty"
  :items            ()
  :item-to-string-fn nil
  :hover-fn          nil
  :show-if-fn        nil
  :rendered "")

;; ═══════════════════════════════════════════════════════════════════
;; Group 12 — views: membership, filtering, dispatch
;; ═══════════════════════════════════════════════════════════════════

;; ── `--belongs-to-view-p' predicate ────────────────────────────────

(defaq-test "section with no :view is universal under every view" [views]
  (let ((section (list :title "Toolbar")))
    (should (actionable-query--belongs-to-view-p section nil))
    (should (actionable-query--belongs-to-view-p section "daily"))
    (should (actionable-query--belongs-to-view-p section "anything"))))

(defaq-test "string :view matches only its own name" [views]
  (let ((section (list :title "Inbox" :view "daily")))
    (should      (actionable-query--belongs-to-view-p section "daily"))
    (should-not  (actionable-query--belongs-to-view-p section "weekly"))
    (should-not  (actionable-query--belongs-to-view-p section nil))))

(defaq-test "list :view matches any member, nothing else" [views]
  (let ((section (list :title "Jira" :view '("daily" "standup"))))
    (should      (actionable-query--belongs-to-view-p section "daily"))
    (should      (actionable-query--belongs-to-view-p section "standup"))
    (should-not  (actionable-query--belongs-to-view-p section "weekly"))
    (should-not  (actionable-query--belongs-to-view-p section nil))))

;; ── `--insert-all' filters by current view ─────────────────────────

(defaq-test "insert-all under named view shows only matching + universal" [views]
  (defquery :title "Univ"   :items '("u"))
  (defquery :title "Daily"  :view "daily"  :items '("d"))
  (defquery :title "Weekly" :view "weekly" :items '("w"))
  (let* ((actionable-query--current-view "daily")
         (text (aq-test--render-all)))
    (should     (string-match-p "Univ"   text))
    (should     (string-match-p "Daily"  text))
    (should-not (string-match-p "Weekly" text))))

(defaq-test "insert-all with no active view shows only universal sections" [views]
  (defquery :title "Univ"   :items '("u"))
  (defquery :title "Daily"  :view "daily" :items '("d"))
  (let* ((actionable-query--current-view nil)
         (text (aq-test--render-all)))
    (should     (string-match-p "Univ"  text))
    (should-not (string-match-p "Daily" text))))

;; ── Buffer-local stash survives `g'-refresh ────────────────────────

(defaq-test "insert-all stashes view buffer-locally so refresh preserves identity" [views]
  (defquery :title "Daily" :view "daily" :items '("d"))
  (with-temp-buffer
    ;; First render: dynamic flag in scope.
    (let ((actionable-query--current-view "daily")
          (inhibit-read-only t))
      (actionable-query--insert-all))
    (should (equal "daily" actionable-query--view))
    ;; Simulate `org-agenda-redo': dynamic flag gone, but buffer-local
    ;; stash should carry the view through so the Daily section keeps
    ;; rendering.
    (erase-buffer)
    (let ((inhibit-read-only t))
      (actionable-query--insert-all))
    (should (string-match-p "Daily" (buffer-string)))))

;; ── `define-view' upsert semantics ─────────────────────────────────

(defaq-test "define-view registers a view by :name" [views]
  (actionable-query-define-view
   :name "v1"
   :open (lambda () (ignore)))
  (should (= 1 (length actionable-query-views)))
  (should (equal "v1" (car (car actionable-query-views)))))

(defaq-test "define-view upsert — second call replaces first" [views]
  (actionable-query-define-view :name "v1" :open (lambda () :v1)
                                  :description "first")
  (actionable-query-define-view :name "v1" :open (lambda () :v1)
                                  :description "second")
  (should (= 1 (length actionable-query-views)))
  (should (equal "second"
                 (plist-get (cdr (car actionable-query-views))
                            :description))))

(defaq-test "define-view errors on unknown keys" [views]
  (should-error
   (actionable-query-define-view :name "v1" :opne (lambda () :v1))))

(defaq-test "define-view errors when :name is missing or non-string" [views]
  (should-error (actionable-query-define-view :open (lambda () :v1)))
  (should-error (actionable-query-define-view :name 'v1 :open (lambda () :v1))))

(defaq-test "define-view errors when :open is missing or non-function" [views]
  (should-error (actionable-query-define-view :name "v1"))
  (should-error (actionable-query-define-view :name "v1" :open "nope")))

;; ── `open-view' dispatch ───────────────────────────────────────────

(defaq-test "open-view binds dynamic flag while :open runs" [views]
  (let (observed)
    (actionable-query-define-view
     :name "obs"
     :open (lambda () (setq observed actionable-query--current-view)))
    (actionable-query-open-view "obs")
    (should (equal "obs" observed))))

(defaq-test "open-view errors on unknown view name" [views]
  (should-error (actionable-query-open-view "no-such-view")))

(defaq-test "open-view with decorated candidate strips suffix" [views]
  (let (observed)
    (actionable-query-define-view
     :name "obs"
     :description "desc"
     :open (lambda () (setq observed actionable-query--current-view)))
    ;; Simulate the completing-read candidate shape "NAME — DESC".
    (actionable-query-open-view "obs — desc")
    (should (equal "obs" observed))))

;; ── Universal button dedup across views ────────────────────────────

(defaq-test "universal button dedup — same button in two universal sections renders once" [views]
  (defquery
    :title "U1" :items '("x")
    :buttons (list (list :title "B" :action "https://example.com"
                         :location 'top)))
  (defquery
    :title "U2" :items '("y")
    :buttons (list (list :title "B" :action "https://example.com"
                         :location 'top)))
  (let* ((actionable-query--current-view nil)
         (text (aq-test--render-all))
         (count 0))
    (with-temp-buffer
      (insert text)
      (goto-char (point-min))
      (while (re-search-forward "^\\s-*B\\s-*$" nil t)
        (cl-incf count)))
    (should (= 1 count))))

(defaq-test "view-scoped button does not leak into other view" [views]
  (defquery
    :title "Daily" :view "daily" :items '("x")
    :buttons (list (list :title "DailyBtn" :action "https://example.com"
                         :location 'top)))
  ;; Render under a different view — scoped button must not appear.
  (let* ((actionable-query--current-view "weekly")
         (text (aq-test--render-all)))
    (should-not (string-match-p "DailyBtn" text))))

;; ── Registry forwards :view from the macro ─────────────────────────

(defaq-test "macro forwards :view to registered plist" [views]
  (defquery :title "T" :view "daily" :items '("x"))
  (let ((reg (car actionable-query--registry)))
    (should (equal "daily" (plist-get reg :view)))))

(defaq-test "macro without :view stores nil (universal)" [views]
  (defquery :title "T" :items '("x"))
  (let ((reg (car actionable-query--registry)))
    (should-not (plist-get reg :view))))

(provide 'actionable-query-tests)
;;; tests.el ends here
