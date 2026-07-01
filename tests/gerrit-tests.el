;;; gerrit-tests.el --- ERT tests for org-agenda-gerrit.el (neato-gerrit)  -*- lexical-binding: t; -*-
;;
;; Run batch:
;;   emacs --batch -L . -L ~/snap -L ~/actionable-query \
;;         --eval '(package-initialize)' \
;;         -l tests.el -f ert-run-tests-batch-and-exit
;;
;; Run interactively: M-x ert after loading this file.
;;
;; Snapshot workflow (§E5 pattern):
;;   1. Load this file: M-x eval-buffer (or C-c C-l)
;;   2. Place point inside a define-ng-view-test form with :expected-view ""
;;   3. C-u C-x C-e — snap rewrites :expected-view from the actual buffer

(require 'cl-lib)
(require 'savehist)
(load (expand-file-name "~/snap/snap.el")         nil t t)
(load (expand-file-name "~/actionable-query/core/actionable-query.el") nil t t)
(load (expand-file-name "~/actionable-query/applications/org-agenda-gerrit/org-agenda-gerrit.el") nil t t)

;;; ─── fixture ────────────────────────────────────────────────────────────────

(snap-define-fixture deftest)

(snap-define-fixture defngtest
  "Reset neato-gerrit caches before body; restore afterward."
  (let ((old-cache (copy-hash-table org-agenda-gerrit--jira-title-cache)))
    (unwind-protect
        (progn
          (clrhash org-agenda-gerrit--jira-title-cache)
          &body)
      (setq org-agenda-gerrit--jira-title-cache old-cache))))

;;; ─── DSL helpers ─────────────────────────────────────────────────────────────
;;
;; Renamed from `org-agenda-gerrit-test--' → `ng-test--'.
;; Pure string parsers; no Gerrit SSH or Jira HTTP involved.

(defun ng-test--name-to-username (full-name)
  "Derive a Gerrit-style username from FULL-NAME.
\"Grace Hopper\" → \"ghopper\"."
  (let ((parts (split-string (string-trim full-name) " ")))
    (downcase (concat (substring (car parts) 0 1)
                      (car (last parts))))))

(defun ng-test--as-gerrit-patch (spec)
  "Parse a human-readable SPEC string into a Gerrit change alist.
SPEC format matches the deprecated `org-agenda-gerrit-test--as-gerrit-patch'."
  (let* ((lines (split-string (string-trim spec) "\n"))
         (lines (mapcar #'string-trim lines))
         (lines (cl-remove-if #'string-empty-p lines))
         (subject (car lines))
         (meta-re (concat "^\\(Change-Id\\|Owner\\|Reviewer"
                          "\\|Added-Reviewer\\|Reviewer-Negative"
                          "\\|Last-Updated\\|Patchset\\|Comments"
                          "\\|Size\\|Verified\\|WIP\\|Attention"
                          "\\|Urgent\\):"))
         (msg-lines (cl-loop for l in lines
                             while (not (string-match-p meta-re l))
                             collect l))
         (meta-lines (cl-subseq lines (length msg-lines)))
         (commit-msg (string-trim (string-join msg-lines "\n")))
         (number nil) (owner-name nil) (last-updated nil)
         (patchset-num nil) (comment-count nil)
         (size-insertions nil) (size-deletions nil)
         (verified-value nil) (wip-p nil) (attention-p nil) (urgent-p nil)
         (reviewers nil) (added-reviewers nil) (neg-reviewers nil))
    (dolist (l meta-lines)
      (cond
       ((string-prefix-p "Change-Id:" l)
        (setq number (string-to-number (string-trim (substring l 10)))))
       ((string-prefix-p "Owner:" l)
        (setq owner-name (string-trim (substring l 6))))
       ((string-prefix-p "Last-Updated:" l)
        (let ((val (string-trim (substring l 13))))
          (setq last-updated
                (if (string-match "\\([0-9]+\\) \\(day\\|week\\|month\\|year\\)s? ago" val)
                    (let* ((n    (string-to-number (match-string 1 val)))
                           (unit (match-string 2 val))
                           (secs (* n (pcase unit
                                        ("day"   86400)
                                        ("week"  604800)
                                        ("month" 2592000)
                                        ("year"  31536000)))))
                      (truncate (- (float-time) secs)))
                  (string-to-number val)))))
       ((string-prefix-p "Reviewer-Negative:" l)
        (push (string-trim (substring l 18)) neg-reviewers))
       ((string-prefix-p "Added-Reviewer:" l)
        (push (string-trim (substring l 15)) added-reviewers))
       ((string-prefix-p "Reviewer:" l)
        (push (string-trim (substring l 9)) reviewers))
       ((string-prefix-p "Patchset:" l)
        (setq patchset-num (string-to-number (string-trim (substring l 9)))))
       ((string-prefix-p "Comments:" l)
        (setq comment-count (string-to-number (string-trim (substring l 9)))))
       ((string-prefix-p "Size:" l)
        (let ((val (string-trim (substring l 5))))
          (when (string-match "\\([0-9]+\\)x\\([0-9]+\\)" val)
            (setq size-insertions (string-to-number (match-string 1 val))
                  size-deletions  (string-to-number (match-string 2 val))))))
       ((string-prefix-p "Verified:" l)
        (setq verified-value (string-trim (substring l 9))))
       ((string-prefix-p "WIP:" l)        (setq wip-p t))
       ((string-prefix-p "Attention:" l)  (setq attention-p t))
       ((string-prefix-p "Urgent:" l)     (setq urgent-p t))))
    (setq reviewers      (nreverse reviewers)
          added-reviewers (nreverse added-reviewers)
          neg-reviewers  (nreverse neg-reviewers))
    (let ((all-rv (mapcar (lambda (r)
                            `((name . ,r)
                              (username . ,(ng-test--name-to-username r))))
                          (append reviewers added-reviewers neg-reviewers)))
          (pos-approvals
           (mapcar (lambda (r)
                     `((type . "Code-Review")
                       (by (name . ,r)
                           (username . ,(ng-test--name-to-username r)))))
                   reviewers))
          (neg-approvals
           (mapcar (lambda (r)
                     `((type . "Code-Review") (value . "-1")
                       (by (name . ,r)
                           (username . ,(ng-test--name-to-username r)))))
                   neg-reviewers))
          (verified-approvals
           (when verified-value
             `(((type . "Verified") (value . ,verified-value)
                (by (name . "Jenkins") (username . "jenkins")))))))
      `((number . ,number)
        (subject . ,subject)
        (commitMessage . ,commit-msg)
        ,@(when last-updated   `((lastUpdated . ,last-updated)))
        ,@(when wip-p          `((wip . t)))
        ,@(when attention-p    `((attention . t)))
        ,@(when urgent-p       `((urgent . t)))
        (owner (name . ,owner-name)
               (username . ,(ng-test--name-to-username owner-name)))
        ,@(when all-rv `((allReviewers . ,all-rv)))
        (currentPatchSet
         ,@(when patchset-num `((number . ,patchset-num)))
         (approvals . ,(append pos-approvals neg-approvals verified-approvals))
         ,@(when size-insertions `((sizeInsertions . ,size-insertions)))
         ,@(when size-deletions  `((sizeDeletions  . ,size-deletions))))
        ,@(when comment-count
            `((comments . ,(mapcar (lambda (i)
                                     `((timestamp . ,(truncate (float-time)))
                                       (message . ,(format "stub comment %d" i))))
                                   (number-sequence 1 comment-count)))))))))

(defun ng-test--as-gerrit-stack (&rest specs)
  "Map `ng-test--as-gerrit-patch' over SPECS.
Auto-assigns change numbers by position when `Change-Id:' is absent."
  (cl-loop for spec in specs
           for idx from 0
           collect (let ((c (ng-test--as-gerrit-patch spec)))
                     (unless (assoc 'number c)
                       (setq c (cons `(number . ,idx) c)))
                     c)))

(defun ng-test--stack-from-string (s)
  "Split S on 5+ dashes; pass each segment to `ng-test--as-gerrit-stack'."
  (let ((segments (split-string s "-\\{5,\\}" t "[ \t\n]*")))
    (apply #'ng-test--as-gerrit-stack segments)))

(defun ng-test--normalise (s)
  "Strip trailing whitespace per line; collapse 3+ blank lines to 2."
  (let* ((s (replace-regexp-in-string "[ \t]+\n" "\n" s))
         (s (replace-regexp-in-string "\n\\{3,\\}" "\n\n" s)))
    (string-trim s)))

;;; ─── snap-define-relation: ng-view ──────────────────────────────────────────
;;
;; Open a minimal actionable-query view pre-loaded with items derived from STACK
;; or STACKS (bypassing real SSH/HTTP); compare the rendered vtable buffer to
;; EXPECTED-VIEW via snap-equal-modulo.
;;
;; STACK and STACKS are quoted verbatim by snap-define-relation; eval them inside
;; the body (same pattern as `actionable-query-view' in actionable-query/tests.el).
;; ACTIONS is a list of (lambda (vn buf) …) run after the view opens.
;; MODULO is a regex or list of regexes tolerated during comparison.
;; HOVER, when non-nil, is asserted against `org-agenda-gerrit--help-echo' on the
;; first item (centered padding stripped).

(snap-define-relation ng-view (stack stacks actions expected-view modulo hover)
  "Open a neato-gerrit vtable view pre-loaded from STACK or STACKS; compare buffer to EXPECTED-VIEW."
  (let* ((org-agenda-gerrit-jira-ticket-regex "\\(BUG-[0-9]+\\)")
         (org-agenda-gerrit-base-url          "https://gerrit.example.com")
         (org-agenda-gerrit-project-path      "/c/repo/+/")
         (org-agenda-gerrit-jira-base-url     "https://jira.example.com/browse")
         (org-agenda-gerrit--jira-title-cache (make-hash-table :test 'equal))
         (old-cache     (copy-hash-table aq--object-cache))
         (old-dismissed (copy-hash-table aq--dismissed))
         (view-name     "test/ng-view")
         (raw   (if (eval stacks)
                    (apply #'append
                           (mapcar #'ng-test--stack-from-string (eval stacks)))
                  (ng-test--stack-from-string (eval stack))))
         (items (-mapcat #'org-agenda-gerrit--items-from-stack
                         (org-agenda-gerrit--group-into-stacks raw)))
         (buf   (progn
                  (eval `(actionable-query-defview ,view-name
                           :objects (lambda (cb) (funcall cb ',items))
                           :columns org-agenda-gerrit-columns
                           :actions '()))
                  (funcall (alist-get view-name org-ql-views nil nil #'string=))
                  (get-buffer (format "%s%s*" org-ql-view-buffer-name-prefix view-name)))))
    (unwind-protect
        (progn
          (with-current-buffer buf
            (dolist (action (eval actions))
              (funcall action view-name buf)))
          (let ((actual (with-current-buffer buf
                          (ng-test--normalise
                           (buffer-substring-no-properties (point-min) (point-max))))))
            (should (if modulo
                        (snap-equal-modulo expected-view actual modulo)
                      (equal expected-view actual)))
            (when hover
              (let* ((raw-echo (substring-no-properties
                                (org-agenda-gerrit--help-echo (car items) nil)))
                     (stripped (mapconcat #'string-trim
                                          (split-string raw-echo "\n") "\n")))
                (should (equal hover stripped))))
            (list :expected-view actual)))
      (when (buffer-live-p buf) (kill-buffer buf))
      (setq org-ql-views    (assoc-delete-all view-name org-ql-views #'string=)
            aq--object-cache old-cache
            aq--dismissed    old-dismissed))))

;;; ─── §1 · Jira ticket extraction ────────────────────────────────────────────

(define-ng-view-test "tip's Jira ticket when stack intersection is empty"
  :stack "[core] Extract sermon on piety from Nahj al-Balagha

          Owner: Grace Hopper
          --------
          [core] Add khutbah validation for Friday prayers

          BUG-42 #progress

          Owner: Grace Hopper"
  :stacks nil
  :actions (list)
  :expected-view ""
  :modulo ("Last fetched at [^—]* — press"))

(define-ng-view-test "shared Jira ticket when every change references it"
  :stack "[lang] Model the event of Ghadir Khumm

          BUG-7 #progress

          Owner: Ada Lovelace
          --------
          [lang] Parse the Prophet's declaration at Ghadir

          BUG-7 #progress

          Owner: Ada Lovelace"
  :stacks nil
  :actions (list)
  :expected-view ""
  :modulo ("Last fetched at [^—]* — press"))

(define-ng-view-test "Jira ticket for single-change stack"
  :stack "[ui] Render Ali's counsel to Malik al-Ashtar

          BUG-101 #progress

          Owner: Alan Turing"
  :stacks nil
  :actions (list)
  :expected-view ""
  :modulo ("Last fetched at [^—]* — press"))

;;; ─── §2 · Stack splitting ────────────────────────────────────────────────────

(define-ng-view-test "reviews-needed splits multi-author stack into per-author items"
  :stack "[core] Document Ali's role at the Battle of Badr

          BUG-52 #progress

          Owner: Ada Lovelace
          --------
          [core] Record Ali's duel with Amr ibn Abd Wudd

          BUG-52 #progress

          Owner: Ada Lovelace
          --------
          [core] Chronicle the Battle of Uhud

          BUG-61 #progress

          Owner: Grace Hopper
          --------
          [core] Model Ali's defence of the Prophet at Uhud

          BUG-61 #progress

          Owner: Grace Hopper"
  :stacks nil
  :actions (list)
  :expected-view ""
  :modulo ("Last fetched at [^—]* — press"))

(define-ng-view-test "reviews-needed splits same-author multi-ticket stack into per-ticket items"
  :stack "[lang] Index Ali's letters in Nahj al-Balagha

          BUG-48 #resolve

          Owner: Alan Turing
          --------
          [lang] Parse Ali's aphorisms on justice and governance

          BUG-51 #progress

          Owner: Alan Turing"
  :stacks nil
  :actions (list)
  :expected-view ""
  :modulo ("Last fetched at [^—]* — press"))

(define-ng-view-test "reviews-needed produces one item for single-author single-ticket stack"
  :stack "[ui] Typeset Ali's sermon on monotheism (Khutbat al-Tawhid)

          BUG-99 #progress

          Owner: Alan Turing"
  :stacks nil
  :actions (list)
  :expected-view ""
  :modulo ("Last fetched at [^—]* — press"))

;;; ─── §3 · Ticket extraction edge cases ──────────────────────────────────────

(define-ng-view-test "extract-jira-tickets uses last reference as primary ticket"
  :stack "[lang] Parse Ali's treaty with Muawiya at Siffin

          BUG-30 #progress
          BUG-49 #progress

          Owner: Grace Hopper"
  :stacks nil
  :actions (list)
  :expected-view ""
  :modulo ("Last fetched at [^—]* — press"))

(define-ng-view-test "reviews-needed drops jira-less changes from multi-ticket stack"
  :stack "[predict, refactor] Migrate Siffin arbitration records

          BUG-77 #progress

          Owner: Alan Turing
          --------
          [predict, refactor] Extract Kharijite dissent timeline

          Owner: Alan Turing
          --------
          [predict] Reject fabricated hadith about Nahrawan

          BUG-88 #resolve

          Owner: Alan Turing"
  :stacks nil
  :actions (list)
  :expected-view ""
  :modulo ("Last fetched at [^—]* — press"))

(define-ng-view-test "reviews-needed still shows item when no change has a jira ticket"
  :stack "[predict] Catalogue Ali's judges in Kufa

          Owner: Alan Turing
          --------
          [predict] Record Kumayl ibn Ziyad's governorship

          Owner: Alan Turing"
  :stacks nil
  :actions (list)
  :expected-view ""
  :modulo ("Last fetched at [^—]* — press"))

;;; ─── §4 · Reviewer rendering ─────────────────────────────────────────────────

(define-ng-view-test "please-review shows non-voting reviewer by name"
  :stack "[core, refactor] Simplify Ali's migration from Mecca to Medina

          BUG-123 #progress

          Owner: Me
          Added-Reviewer: Alan Turing
          --------
          [core] Model Ali sleeping in the Prophet's bed on Laylat al-Mabit

          BUG-123 #related

          Owner: Me"
  :stacks nil
  :actions (list)
  :expected-view ""
  :modulo ("Last fetched at [^—]* — press"))

(define-ng-view-test "please-review surfaces bot with negative Code-Review"
  :stack "[core] Record Ali's conquest of Khaybar

          BUG-42 #progress

          Owner: Me
          Added-Reviewer: Alan Turing
          Reviewer-Negative: Skynet"
  :stacks nil
  :actions (list)
  :expected-view ""
  :modulo ("Last fetched at [^—]* — press"))

(define-ng-view-test "please-review filters out bot with positive Code-Review"
  :stack "[core] Document Ali lifting the gate of Khaybar

          BUG-42 #progress

          Owner: Me
          Added-Reviewer: Alan Turing
          Reviewer: Skynet"
  :stacks nil
  :actions (list)
  :expected-view ""
  :modulo ("Last fetched at [^—]* — press"))

;;; ─── §5 · Multi-ticket stacks ────────────────────────────────────────────────

(define-ng-view-test "multi-ticket stack produces one work-item per ticket"
  :stack "[lang] Encode Ali's arbitration at Dumat al-Jandal

          BUG-49 #resolve

          Owner: Me
          --------
          [lang] Model the Kharijite revolt after Siffin

          BUG-48 #resolve

          Owner: Me"
  :stacks nil
  :actions (list)
  :expected-view ""
  :modulo ("Last fetched at [^—]* — press"))

;;; ─── §6 · Jira-less items ────────────────────────────────────────────────────

(define-ng-view-test "jira-less item renders with empty Jira column"
  :stack "[lang] Transcribe Ali's du'a known as Du'a Kumayl

          !NO_JIRA

          Owner: Me
          Added-Reviewer: Grace Hopper
          Attention: yes"
  :stacks nil
  :actions (list)
  :expected-view ""
  :modulo ("Last fetched at [^—]* — press"))

;;; ─── §7 · Age display ────────────────────────────────────────────────────────

(define-ng-view-test "age column is non-empty when lastUpdated is set"
  :stack "[base] Summarise Ali's caliphate in Kufa (36-40 AH)

          BUG-77 #progress

          Owner: Me
          Added-Reviewer: Alan Turing
          Last-Updated: 5 months ago"
  :stacks nil
  :actions (list)
  :expected-view ""
  :modulo ("Last fetched at [^—]* — press"))

(define-ng-view-test "age column is empty when no lastUpdated on change"
  :stack "[base] Draft Ali's letter to the people of Egypt

          BUG-88 #progress

          Owner: Me
          WIP: yes"
  :stacks nil
  :actions (list)
  :expected-view ""
  :modulo ("Last fetched at [^—]* — press"))

;;; ─── §8 · Sorting ────────────────────────────────────────────────────────────

(define-ng-view-test "sort: urgent first, stalest-first everywhere, sidequests last"
  :stack nil
  :stacks ("[base] Record the Battle of the Camel (Jamal)

            BUG-10 #progress

            Owner: Grace Hopper
            Last-Updated: 6 months ago"

           "[base] Document Ali's sermon after Siffin

            BUG-20 #progress

            Owner: Alan Turing
            Last-Updated: 2 days ago"

           "[base] Fix Ali's appointment of Malik al-Ashtar

            BUG-30 #progress

            Owner: Ada Lovelace
            Last-Updated: 1 days ago
            Urgent: yes"

           "[base] Catalogue Ali's sayings on patience

            !NO_JIRA

            Owner: Grace Hopper
            Last-Updated: 1 years ago"

           "[base] Index Ali's rulings on the Bayt al-Mal

            !NO_JIRA

            Owner: Alan Turing
            Last-Updated: 3 days ago")
  :actions (list)
  :expected-view ""
  :modulo ("Last fetched at [^—]* — press"))

;;; ─── §9 · Help-echo ──────────────────────────────────────────────────────────

(define-ng-view-test "help-echo surfaces metadata from rich fixture"
  :stack "[core] Compile Ali's guidance on distributing the treasury

          BUG-99 #progress

          Owner: Ali ibn Abi Talib
          Reviewer: Hassan ibn Ali
          Reviewer: Hussain ibn Ali
          Last-Updated: 3 days ago
          Patchset: 7
          Comments: 12
          Size: 300x150
          Verified: -1"
  :stacks nil
  :actions (list)
  :expected-view ""
  :modulo ("Last fetched at [^—]* — press")
  :hover "Open this item.\n[size L] [ps 7] [12 comments] [2 reviewers] [CI ✗]\n🔴 CI is red — fix the build before anything else.")

(provide 'gerrit-tests)
;;; gerrit-tests.el ends here
