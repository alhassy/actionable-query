;;; review.el --- Weekly Review / Promise standup composer  -*- lexical-binding: t; -*-
;;
;; A single dashboard section, appended at the very bottom of `*Dashboard*',
;; that reads like a Slack standup post I can share ("here's what I've done +
;; what I promise to do"):
;;
;;   :white_check_mark: Resolved   -- Jira Done this week   (oag-jira-done-this-week)
;;   Monday Standup                -- annotated ticket list (oag-reviews-needed +
;;                                    oag-my-changes-needing-action)
;;   :hammer: In Progress          -- open WIP changes       (dashboard/work-in-progress)
;;   :construction: Impediments    -- awaiting review        (oag-please-review-my-work)
;;   :palm_tree: OOO               -- gcalcli, the week ahead (review/ooo, async)
;;   :calendar: Planned            -- forward-planning prose  (my choice, seeded)
;;
;; The whole thing is composed from views that already exist in the dashboard, so
;; it inherits their async + caching for free (`:insert reuse-cache': plain `C-c
;; d' reuses cache and honors the slow-gate; `C-u C-c d' forces fresh).  Only
;; ambient runtime touchpoints are `my/copy-as-slack' (stays in init.el) and the
;; `my\*-url' / `my\gcalcli-calendar' private vars --- each guarded so this file
;; degrades rather than errors when one is absent.  It depends on nothing in
;; wr.el / WR.org.
;;
;; The guidance prose (Sprint-Doc / reported-by-me links, Wins/brag-doc nudge,
;; archive prompt, forward-planning checklist) is lifted verbatim from wr.el as
;; pure strings --- no code dependency.

(require 'org-agenda-gerrit)

;; `dashboard--heading-link' and `dashboard--hider' live in dashboard.el, which
;; loads this file; they are bound by the time `review/weekly-promise' runs.
(declare-function dashboard--heading-link "dashboard")
(declare-function dashboard--hider "dashboard")
(declare-function my/copy-as-slack "init")
;; The `s'-key daily standup lives in work-calendar.el (loaded before us by
;; dashboard.el); we reuse its side-effect-free builders at copy time.
(declare-function dashboard--standup-work-items "work-calendar")
(declare-function dashboard--standup-org "work-calendar")

;; ── 🌴 OOO --- self-contained async gcalcli view ──────────────────────────────
;;
;; Modelled on `dashboard--gcal-fetch-account' (work-calendar.el): same gcalcli
;; args, TSV header-skip, and plain `(nth N f)' column reads.  Two departures:
;; we ask for the *week ahead* (not just today), and we *keep* OOO/PTO/holiday
;; rows (work-calendar drops them as noise).  Async via `make-process' so a
;; plain `C-c d' never blocks on gcalcli.  TSV columns (--details all):
;;   0 id  1 start_date  2 start_time  3 end_date  4 end_time  …  9 title …

(defvar review--ooo-cache nil
  "Cons (WEEK-KEY . ROWS) memoizing the last OOO gcalcli pull, or nil.
`review--ooo-objects' reuses it for the same week so a plain refresh doesn't
re-shell out; a fresh fetch (via `C-u C-c d') resets it.")

(defvar review-ooo-title-re "OOO\\|PTO\\|holiday\\|vacation\\|leave\\|\\boff\\b"
  "Case-insensitive regexp matching calendar titles that count as time off.")

(defun review--week-range ()
  "Return (START . END) \"YYYY-MM-DD\" strings for today .. today+7 (exclusive).
END is exclusive, mirroring gcalcli's `agenda' convention."
  (let* ((today (calendar-current-date))
         (abs   (calendar-absolute-from-gregorian today))
         (fmt   (lambda (a)
                  (pcase-let ((`(,m ,d ,y) (calendar-gregorian-from-absolute a)))
                    (format "%04d-%02d-%02d" y m d)))))
    (cons (funcall fmt abs) (funcall fmt (+ abs 7)))))

(defun review--ooo-parse (raw start)
  "Parse gcalcli --tsv RAW into OOO row plists (:date :title), from date START.
Keeps only rows whose title matches `review-ooo-title-re'."
  (let (rows)
    (dolist (line (cdr (split-string raw "\n" t)))  ; cdr: skip the header row
      (let ((f (split-string line "\t")))
        (when (and (>= (length f) 10)
                   (let ((case-fold-search t))
                     (string-match-p review-ooo-title-re (nth 9 f))))
          (push (list :date (nth 1 f) :title (nth 9 f)) rows))))
    (setq rows (nreverse rows))
    (setq review--ooo-cache (cons start rows))
    rows))

(defun review--ooo-objects (callback)
  "Async `:objects' for `review/ooo': deliver OOO rows for the week ahead.
Reuses `review--ooo-cache' for the same week; otherwise shells out to gcalcli
via `make-process'.  Degrades to no rows (the section then hides) when gcalcli
is missing/unconfigured --- never errors mid-dashboard."
  (pcase-let ((`(,start . ,end) (review--week-range)))
    (cond
     ((and review--ooo-cache (equal (car review--ooo-cache) start))
      (funcall callback (cdr review--ooo-cache)))
     ((or (not (boundp 'my\gcalcli-calendar)) (not (executable-find "gcalcli")))
      (funcall callback nil))
     (t
      (let ((buf (generate-new-buffer " *review-ooo*")))
        (make-process
         :name "review-ooo" :buffer buf :noquery t
         :command (list "gcalcli" "--calendar" (symbol-value 'my\gcalcli-calendar)
                        "agenda" start end "--tsv" "--details" "all")
         :sentinel
         (lambda (proc _event)
           (when (memq (process-status proc) '(exit signal))
             (let ((rows (and (zerop (process-exit-status proc))
                              (review--ooo-parse
                               (with-current-buffer buf (buffer-string)) start))))
               (kill-buffer buf)
               (funcall callback rows))))))))))

(actionable-query-defview review/ooo "🌴 OOO --- the week ahead"
  :objects #'review--ooo-objects
  :no-footer t
  :columns '((:name "Date"  :width 12
                    :getter (lambda (o &rest _) (plist-get o :date)))
             (:name "Off"
                    :getter (lambda (o &rest _) (plist-get o :title)))))

;; ── The composite section ─────────────────────────────────────────────────────

(defun review--maybe-link (url label fallback)
  "Org link [[URL][LABEL]] when URL var is bound to a string, else FALLBACK text.
URL is a symbol (a `my\\*-url' private var); guarded so this file degrades when
private.el hasn't set it."
  (if (and (boundp url) (stringp (symbol-value url)))
      (format "[[%s][%s]]" (symbol-value url) label)
    fallback))

(defun review--section (view label reuse-cache &optional prose)
  "Insert a `*** LABEL' heading linked to VIEW, optional PROSE, then splice VIEW.
Mirrors the dashboard's per-section idiom: a point-marker before the heading and
`dashboard--hider' so an empty/all-scheduled section is dropped on a plain
refresh.  PROSE (a string) is inserted between heading and table."
  (let ((start (point-marker)))
    (insert "\n\n*** " (dashboard--heading-link view label) "\n\n")
    (when prose (insert prose "\n\n"))
    (funcall view :insert reuse-cache
             :on-inserted (dashboard--hider reuse-cache start))))

(defun review/weekly-promise (reuse-cache)
  "Insert the Weekly Review / Promise section into the current buffer.
REUSE-CACHE is threaded to every sub-view's `:insert' (t = reuse cache + honor
the slow-gate; \\='fetch-latest = force fresh), so this whole section inherits
the dashboard's async + prefix-arg caching without any bespoke code.  Composes
existing `oag-*' / `dashboard/*' views and folds in the review-ritual prose."
  ;; Dashboard-facing guidance.  The copy command supplies its own
  ;; "progress report" divider, so this line is worded to avoid duplicating it.
  (insert (propertize
           "Read, curate the next-action verbs, fill in OOO / Planned, then hit the “📋 Copy as Slack post” link above.\n"
           'face '(:foreground "gray40" :slant italic)))

  ;; Each section may carry its own private tail: content below a
  ;; `review-privacy-separator' line, up to the next heading, is stripped from
  ;; the Slack post (see `review--strip-private') but stays visible in the
  ;; dashboard.  So the shareable substance sits above each section's separator;
  ;; the ritual I run for myself sits below it.

  ;; ✅ Resolved --- what shipped this week; private tail = the accountability
  ;; block (sprint doc, archiving, brag-doc win).
  (review--section
   'oag-jira-done-this-week "✅ Resolved --- what shipped this week" reuse-cache)
  (insert "\n" review-privacy-separator "\n")
  (insert (format "\n1. %s\n"
                  (review--maybe-link 'my\sprint-doc-url "🤔 Update Sprint Doc"
                                      "🤔 Update Sprint Doc")))
  (insert "\n2. *Archive completed and cancelled tasks*\n")
  (insert "   /Archiving is an act of closure. It says: this is finished./ /A clean task list is a calm mind./\n")
  (insert "   - Look through the ~:LOG:~ for useful information to file away into my References.\n")
  (insert "   - If there's useful info, capture it with ~C-c C-c~, then archive the original tree for clocking purposes.\n")
  (insert "   - If clocking purposes do not matter, just delete the tree.\n")
  (insert "\n3. ➕ Wins: What went well and why? What could have caused things to go\n")
  (insert "   so well --- can I duplicate it next week?  Re-read\n")
  (insert "   [[https://jvns.ca/blog/brag-documents/][Get your work recognized: write a brag document]].\n")

  ;; Monday Standup --- today's schedule-derived standup is prepended at copy
  ;; time (the `s' standup, via `dashboard--standup-*').  The two ticket
  ;; sub-views below are the review substance behind it.
  (insert "\n\n** Monday Standup\n\n")
  (review--section 'oag-reviews-needed "👀 Whom am I blocking?" reuse-cache)
  (review--section 'oag-my-changes-needing-action "🔧 Feedback I need to address" reuse-cache)

  ;; 🔨 In Progress --- reuse the warm cache slot from the dashboard bottom.
  (review--section 'dashboard/work-in-progress "🔨 In Progress" reuse-cache)

  ;; 🚧 Impediments --- my open changes still awaiting review.
  (review--section 'oag-please-review-my-work "🚧 Impediments --- awaiting review" reuse-cache)

  ;; 🌴 OOO --- the week ahead, from gcalcli (async, self-contained).
  (review--section 'review/ooo "🌴 OOO --- the week ahead" reuse-cache)

  ;; 📆 Planned --- my choice; the whole forward-planning ritual is private
  ;; (below the separator), so the shared post carries only the "Planned"
  ;; heading, which I fill in by hand before posting.
  (insert "\n\n** 📆 Planned\n\n")
  (insert review-privacy-separator "\n\n")
  (insert (propertize "Aim for ~3 Jiras, since rework requires time!\n\n"
                      'face '(:foreground "gray40" :slant italic)))
  (insert "1. 🔀 What will I focus on this week?\n")
  (insert "2. What $10k tasks do I want done? Why or why not?\n")
  (insert "3. 😟 Anything I'm worried about, concerned about, or uneasy with? Surface it now --- this is the time.\n\n")
  (insert (format "*What's my manager interested in for this sprint?* %s\n"
                  (review--maybe-link 'my\sprint-doc-url "Open Sprint Doc 🤔" "(set my\\sprint-doc-url)")))
  (insert (format "*Tickets I reported (personal interest):* %s\n\n"
                  (review--maybe-link 'my\jira-reported-by-me-url "Open in Jira 🎯" "(set my\\jira-reported-by-me-url)")))
  (insert "*Prioritize and schedule!*\n\n")
  (insert "0. [ ] For the Waiting list, have others completed their tasks?\n")
  (insert "1. [ ] *Check Calendar*. Look at the company calendar for the upcoming 2 weeks;\n")
  (insert "   add items to the todo list if needed.\n")
  (insert "2. [ ] Find relevant tasks: What are my sprint goals and quarterly goals?\n")
  (insert "   What is assigned to me in Jira /for this sprint/?  Any upcoming deadlines?\n")
  (insert "   Look at Someday/Maybe for anything worth doing.  Task priorities drift ---\n")
  (insert "   relook recently-processed tasks; some may no longer be necessary.\n")
  (insert "3. [ ] Assign a [[https://radreads.co/10k-work/][dollar value]] to work: $10 / $100 / $1k / $10k.\n")
  (insert "   Schedule the $10k tasks on the calendar so the week ends with something of significance.\n")
  (insert "4. [ ] Add efforts to the week's tasks, then sanity-check each day is realistic ---\n")
  (insert "   look at the daily agenda for next week; don't overload and keep pushing things.\n")
  (insert "5. [ ] Decide the week's tasks and [[https://dansilvestre.com/time-blocking/][time-block]] the calendar.\n")
  (insert "   *Embrace trade-offs.* Choosing one task says `no' to many others --- that's a good thing.\n")
  (insert "6. [ ] Study next week's agenda: important scheduled tasks or deadlines, and any\n")
  (insert "   preparatory work needed.\n"))

;; ── Slack-copy affordance ──────────────────────────────────────────────────────

(defvar review-privacy-separator "----Everything Below This Line Is Private----"
  "Line marking the start of a section's private tail.
Everything from this line up to the next heading (or end of section) is
stripped from the copied Slack post by `review--strip-private'.")

(defun review--strip-private (text)
  "Remove every private span from TEXT: a `review-privacy-separator' line and all
following lines up to (but not including) the next `*'-heading or end of TEXT.
Per-section, so each review section can hide its own tail independently.
Walks lines (Emacs regexps have no lookahead), toggling a `private' state on a
separator line and clearing it at the next heading."
  (let ((private nil) kept)
    (dolist (line (split-string text "\n"))
      (cond
       ((string-prefix-p review-privacy-separator line) (setq private t))
       ((string-match-p "\\`\\*+ " line) (setq private nil) (push line kept))
       ((not private) (push line kept))))
    (string-join (nreverse kept) "\n")))

(defvar review-shortcode-emoji
  '((":white_check_mark:" . "✅") (":hammer:" . "🔨") (":construction:" . "🚧")
    (":palm_tree:" . "🌴") (":calendar:" . "📆") (":fire:" . "🔥")
    (":warning:" . "⚠️") (":rotating_light:" . "🚨") (":memo:" . "📝")
    (":eyes:" . "👀") (":wrench:" . "🔧") (":waving_black_flag:" . "🏴")
    (":tada:" . "🎉") (":rocket:" . "🚀") (":bug:" . "🐛") (":sos:" . "🆘")
    (":thinking_face:" . "🤔") (":pray:" . "🤲") (":dart:" . "🎯"))
  "Slack `:shortcode:' → Unicode emoji.  Codes absent here are left verbatim.")

(defun review--emojify-shortcodes (text)
  "Replace known Slack `:shortcode:'s in TEXT with emoji, leaving unknown ones.
Only exact keys in `review-shortcode-emoji' are touched, so an unmapped
`:foo_bar:' passes through unchanged."
  (replace-regexp-in-string
   ":[a-z0-9_+-]+:"
   (lambda (m) (or (cdr (assoc m review-shortcode-emoji)) m))
   text t t))

(defvar review-copy-strip-lines
  '("^Last fetched at .*"                       ; vtable fetch-status footer
    "^[⏳⌛] fetching .*"                         ; unsettled async placeholder (either glyph)
    "^[0-9]+ unread$"                            ; vtable count footers
    "^[0-9]+ snoozed forever.*"
    "^No hearted entries yet.*"
    "^ *(nothing here right now) *$"             ; empty-view sentinel
    "^Today's standup (from the schedule).*"     ; dashboard-only guidance
    "^Read, curate the next-action verbs.*")
  "Regexps for whole lines to drop from the copied Slack post --- vtable UI
chrome (fetch-status, counts, hearts) and dashboard-only guidance that has no
place in a shared standup.")

(defun review--strip-chrome (text)
  "Drop `review-copy-strip-lines' matches from TEXT, then collapse blank runs."
  (let ((out (replace-regexp-in-string
              (concat "\\(?:" (string-join review-copy-strip-lines "\\|") "\\)\n?")
              "" text)))
    ;; Collapse 3+ blank lines (left by removals) down to a single blank line.
    (replace-regexp-in-string "\n\\{3,\\}" "\n\n" out)))

(defun review--todays-standup-org ()
  "Today's `s'-key standup as an Org string, or nil when there's nothing to share.
Reuses work-calendar's side-effect-free builders (no clipboard, no browser)."
  (when (fboundp 'dashboard--standup-work-items)
    (pcase-let ((`(,work . ,meetings) (dashboard--standup-work-items)))
      (unless (and (null work) (null meetings))
        (dashboard--standup-org work meetings)))))

(defun review/copy-section-as-slack ()
  "Copy today's standup + the Weekly Review / Promise section as a Slack post.
Prepends today's schedule-derived standup (the `s' key, via
`dashboard--standup-*') and a progress-report divider, then the section body.
Delegates Org→Slack conversion to `my/copy-as-slack' (stars, links,
name→@handle), and finally maps known `:shortcode:'s to emoji
\(`review--emojify-shortcodes')."
  (interactive)
  (unless (fboundp 'my/copy-as-slack)
    (user-error "`my/copy-as-slack' is unbound --- it lives in init.el"))
  (save-excursion
    (goto-char (point-min))
    (if (re-search-forward "^\\* 📝 .*Weekly Review / Promise" nil t)
        (let (body subtree-end)
          (org-back-to-heading t)
          ;; Section body: everything one line past the heading (so the heading
          ;; and its self-referential "Copy as Slack post" link stay out).
          (save-excursion (org-end-of-subtree t t) (setq subtree-end (point)))
          (forward-line 1)
          (setq body (buffer-substring-no-properties (point) subtree-end))
          (let* ((standup (review--todays-standup-org))
                 (combined (concat
                            (when standup (concat standup "\n\n"))
                            "/Below is a progress report since last week and what I intend to do this week./\n\n"
                            ;; Strip each section's private tail on the RAW org
                            ;; (stars intact) --- `my/copy-as-slack' later drops
                            ;; the stars, so the heading boundary must be found first.
                            (review--strip-private body))))
            ;; `my/copy-as-slack' kill-news the converted text; re-read it, map
            ;; shortcodes to emoji, and put the final post back on the kill-ring.
            (my/copy-as-slack combined)
            ;; Post-process the converted text so a shared post is clean:
            ;;   - flatten our `[label](elisp:…)' heading links to plain labels
            ;;     (they only drive in-dashboard refresh --- meaningless in Slack),
            ;;   - strip vtable chrome + dashboard-only guidance,
            ;;   - map known `:shortcode:'s to emoji.
            (kill-new
             (review--emojify-shortcodes
              (review--strip-chrome
               (replace-regexp-in-string
                "\\[\\([^]]*\\)\\](elisp:.*?))" "\\1" (current-kill 0)))))
            (message "Copied Weekly Review / Promise as a Slack post%s."
                     (if standup " (with today's standup)" ""))))
      (user-error "Weekly Review / Promise section not found in this buffer"))))

(provide 'review)
;;; review.el ends here
