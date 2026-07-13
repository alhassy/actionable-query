;;; dashboard.el --- One-buffer overview: email + RSS + Gerrit/Jira + org-ql  -*- lexical-binding: t; -*-
;;
;; `M-x dashboard' opens a single org-mode buffer, structured as:
;;
;;   ⚡ Top goals for the month  -- org-ql, no heading, right under the title
;;   * 📩 Process Inbox          -- Email, Quick Captures, RSS
;;                                + Whom am I blocking? / Feedback I need
;;                                  to address (+ Waiting on others, nested)
;;                                  (org-agenda-gerrit.el lenses 1 & 2)
;;   * 📆 Planning               -- Jira urgent-not-started, Jira-vs-Gerrit
;;                                disagreement, Overdue (deadline + scheduled,
;;                                nested), Reduce open loops, Gerrit: WIP
;;   * 📋 Urgent and unstarted   -- Priority A unscheduled (org-ql)
;;
;; splicing `actionable-mail/gmail-inbox' (actionable-mail.el),
;; `dashboard/rss-feeds' (on top of actionable-query/data/aq-data-rss.el),
;; and the rest via the `:insert 'fetch-latest' contract, so each view's
;; own actions work in place.

;; Self-locate so sibling applications resolve regardless of where the repo
;; lives.  `actionable-mail' pulls in `actionable-query', which already
;; `require's `rss'/`org-ql' from core --- so we add this folder (for
;; `whats-app' + our own widgets) and the `org-agenda-gerrit' sub-package.
(let ((here (file-name-directory (or load-file-name buffer-file-name))))
  (add-to-list 'load-path here)
  (add-to-list 'load-path (expand-file-name "org-agenda-gerrit" here))
  (require 'actionable-mail)
  (require 'ts)
  (require 'rss)              ; aq--fetch-rss/atom (feed parsers live in applications/)
  (require 'org-agenda-gerrit)
  (require 'whats-app)
  (require 'org-timestamp)
  (load-file (expand-file-name "timezone-convertor.el" here))
  (load-file (expand-file-name "celsius-fahrenheit-convertor.el" here))
  (load-file (expand-file-name "work-calendar.el" here)))

;; The `dashboard/rss-feeds' view + its feed list / actions live in
;; `rss.el' (required above); the dashboard just splices that view into
;; its layout below.

;; The queries below are lifted straight from my own curated
;; `my/define-agenda-ql-section' blocks in init.org (the ones that
;; back `C-c a', the "Daily Agenda: org-ql sections") --- same
;; queries, now also rendered here so they show up the moment I open
;; the dashboard, no need to invoke the full agenda.

(defun dashboard--unwidth (columns &rest names)
  "Copy COLUMNS, dropping `:width' from any column whose `:name' is in NAMES.
Lets a critical column (Heading, Subject, …) auto-size to its widest cell
so titles never truncate, without mutating the shared column defvar."
  (mapcar (lambda (col)
            (if (member (plist-get col :name) names)
                (let ((c (copy-sequence col))) (cl-remf c :width) c)
              col))
          columns))

(actionable-query-defview dashboard/inbox "📩 Quick Captures"
  :org-ql (tags "inbox")
  :columns (dashboard--unwidth aq-org-ql-columns "Heading"))

(defun dashboard--deadline-days (ts)
  "Whole days from today until org timestamp string TS, or nil.
Negative once the deadline is past."
  (when (and ts (string-match "[0-9]\\{4\\}-[0-9][0-9]-[0-9][0-9]" ts))
    (- (org-time-string-to-absolute (match-string 0 ts))
       (org-today))))

(defun dashboard--subtree-progress (marker)
  "Return (DONE . TOTAL) descendant TODO entries under MARKER's tree, or nil.
Mirrors org's own statistics-cookie counting --- every descendant heading
with a TODO keyword counts, and those in a done state are complete."
  (when (and (markerp marker) (marker-buffer marker))
    (org-with-point-at marker
      (org-back-to-heading t)
      (let ((self (point)) (done 0) (total 0))
        (org-map-entries
         (lambda ()
           (unless (= (point) self)
             (when (org-get-todo-state)
               (setq total (1+ total))
               (when (org-entry-is-done-p) (setq done (1+ done))))))
         nil 'tree)
        (cons done total)))))

(defun dashboard--top-goal-help-echo (o)
  "Help-echo for a top-goal row: days-to-deadline + subtree completion %."
  (let* ((days (dashboard--deadline-days (plist-get o :deadline)))
         (prog (dashboard--subtree-progress (plist-get o :marker)))
         (done (car prog)) (total (cdr prog)))
    (string-join
     (delq nil
           (list
            (when days
              (cond ((< days 0)  (format "⚠️ Deadline was %d day%s ago!" (abs days) (if (= 1 (abs days)) "" "s")))
                    ((= days 0)  "⏰ Deadline is TODAY.")
                    (t           (format "%d day%s until the deadline." days (if (= 1 days) "" "s")))))
            (when (and prog (> total 0))
              (format "%d%% complete (%d/%d sub-tasks done)."
                      (round (* 100.0 (/ done (float total)))) done total))
            (when (and prog (zerop total))
              "No sub-tasks yet --- break this goal down.")))
     "  ")))

(actionable-query-defview dashboard/top-goals "⚡ Top goals for the month"
  :org-ql (tags-local "Top")
  ;; Just Heading + a self-labelled date --- no Todo/Pri/Tags clutter.
  :columns '((:name "" :getter (lambda (o &rest _) (plist-get o :heading)))
             (:name "" :getter (lambda (o &rest _)
                                 (cond ((plist-get o :deadline)
                                        (format "Deadline: %s" (plist-get o :deadline)))
                                       ((plist-get o :scheduled)
                                        (format "Scheduled: %s" (plist-get o :scheduled)))
                                       (t "")))))
  :help-echo #'dashboard--top-goal-help-echo)

(actionable-query-defview dashboard/overdue "📆 Overdue"
  :org-ql (and (not (habit)) (not (tags "Top")) (not (done))
               (scheduled :to today) (not (scheduled :on today))
               (not (regexp "SCHEDULED:[^\n]*[.+]?[+][0-9]+[dwmy]")))
  :columns (dashboard--unwidth aq-org-ql-columns "Heading"))

(defun dashboard--age-since (timestamp)
  "Days between now and TIMESTAMP (an org timestamp string), or nil."
  (when timestamp
    (floor (ts-diff (ts-now) (ts-parse-org timestamp)) 86400)))  ; seconds/day

(defun dashboard--waiting-age (o)
  "Days since item O's `:closed' timestamp, or nil if it has none."
  (dashboard--age-since (plist-get o :closed)))

(defun dashboard--open-loop-age (o)
  "Days since item O's `:scheduled', `:deadline', or `:created', whichever exists first."
  (dashboard--age-since (or (plist-get o :scheduled)
                             (plist-get o :deadline)
                             (plist-get o :created))))

(defun dashboard--age-columns (age-fn heading-name)
  "Build [Pri | Age | HEADING-NAME | Tags] vtable columns, Age via AGE-FN."
  (list
   (list :name "Pri" :width 4 :align 'center
         :getter (lambda (o &rest _) (if-let ((p (plist-get o :priority))) (char-to-string p) "")))
   (list :name "Age" :width 8 :align 'center
         :getter (lambda (o &rest _) (if-let ((days (funcall age-fn o)))
                                          (format "%dd" days)
                                        "?"))
         :formatter (lambda (v &rest _)
                      (propertize v 'face (if (string= v "?")
                                               '(:foreground "gray60")
                                             '(:foreground "orange red" :weight bold)))))
   (list :name heading-name  ; no :width -> vtable sizes to the widest heading
         :getter (lambda (o &rest _) (plist-get o :heading)))
   (list :name "Tags" :width 20
         :getter (lambda (o &rest _) (string-join (plist-get o :tags) " ")))))

(defvar dashboard-waiting-columns
  (dashboard--age-columns #'dashboard--waiting-age "Task")
  "Vtable column specs for `dashboard/waiting' --- everything here is
already known WAITING, so the Todo column is redundant, and a
Deadline/Scheduled column is dropped in favor of `Age', the number of
days since the item's `:closed' timestamp (the date I sent the ask).")

(defvar dashboard-open-loops-columns
  (dashboard--age-columns #'dashboard--open-loop-age "Heading")
  "Vtable column specs for `dashboard/open-loops' --- Age is days since
whichever of `:scheduled'/`:deadline'/`:created' exists first.")

(actionable-query-defview dashboard/waiting "💢 Waiting on others"
  :objects (lambda ()
             (cl-sort (aq--org-ql-fetch
                       '(and (todo "WAITING")
                             (not (and (tags "Work") (not (tags "Personal")))))
                       (org-agenda-files))
                      #'> :key (lambda (o) (or (dashboard--waiting-age o) -1))))
  :columns dashboard-waiting-columns)

(actionable-query-defview dashboard/open-loops "🤡 Please 𝒓𝒆𝒅𝒖𝒄𝒆 the number of (unscheduled) open loops"
  :objects (lambda ()
             (cl-sort (aq--org-ql-fetch
                       '(and (todo "STARTED")
                             (level '> 1)
                             (not (tags-local "Someday" "Top" "SocialCredit"))
                             (not (scheduled :from today))
                             (not (and (tags "Work") (not (tags "Personal")))))
                       (org-agenda-files))
                      #'> :key (lambda (o) (or (dashboard--open-loop-age o) -1))))
  :columns dashboard-open-loops-columns)

(actionable-query-defview dashboard/deadline-overdue "⏰ Overdue (by deadline)"
  :org-ql (deadline :to today)
  :columns (dashboard--unwidth aq-org-ql-columns "Heading"))

(actionable-query-defview dashboard/priority-a-unscheduled "🔴 Priority A, unscheduled"
  :org-ql (and (todo) (priority "A") (not (deadline)) (not (scheduled)))
  :columns (dashboard--unwidth aq-org-ql-columns "Heading"))

(actionable-query-defview dashboard/work-in-progress "🚧 Gerrit: Work In Progress"
  :gerrit-query "status:open owner:self -is:abandoned is:wip -attention:self"
  :row-colors   '("light cyan" "azure" "alice blue")
  ;; Author column dropped (always me here); Subject auto-sizes (no truncation).
  :columns      (dashboard--unwidth
                 (cl-remove-if (lambda (col) (equal (plist-get col :name) "Author"))
                               org-agenda-gerrit-columns)
                 "Subject")
  :help-echo    (org-agenda-gerrit--help-echo-tiered
                 :fresh      "🌱 Is this ready? Self-review, ensure right reviewers, keep stacks small."
                 :stale      "Idle for weeks — self-review, add context, then resume."
                 :very-stale "Dead weight? Abandon in Gerrit or commit to finishing it."))

(defun dashboard--heading-link (view label)
  "Org heading text LABEL wrapped as an elisp link that refreshes VIEW in place.
Clicking re-runs (VIEW :insert \\='fetch-latest), re-rendering just that
section's vtable."
  (format "[[elisp:(funcall-interactively '%s)][%s]]" view label))

(defun dashboard--section-hideable-p (objects org-fn)
  "Non-nil when a section holding OBJECTS should be omitted from the dashboard.
True when there are no OBJECTS at all, or when *every* row is already
scheduled today-or-later (via ORG-FN) --- nothing there needs action now.
A row that is unscheduled or overdue keeps the section (returns nil)."
  (or (null objects)
      (seq-every-p (lambda (o) (aq-row-scheduled-on-or-after-today-p o org-fn))
                   objects)))

(defun dashboard--hider (reuse-cache start)
  "Return an `:on-inserted' closure that omits a section when hideable.
START is a marker at the section's first char (before its heading).  When
REUSE-CACHE is `t' and the delivered rows are empty / all-scheduled (see
`dashboard--section-hideable-p'), the closure deletes START..END --- the
whole section: heading, prose, table, footer.  Returns nil on `fetch-latest'
so nothing is hidden (the view then gets `:on-inserted nil')."
  (when (eq reuse-cache t)
    (lambda (objs _beg end org-fn)
      (when (dashboard--section-hideable-p objs org-fn)
        (let ((inhibit-read-only t))
          (delete-region start end)
          (goto-char start))))))

(defun dashboard (&optional force-fresh)
  "Open (or refresh) the *Dashboard* buffer.
With a prefix arg (`C-u M-x dashboard'), bypass each view's slow-fetch
gate and force a fresh fetch everywhere via `:insert \\='fetch-latest'.
Without it, `:insert t' is used, so previously-slow views stay capped
until asked for again --- plain `M-x dashboard' should not hammer
every section on every call."
  (interactive "P")
  (let ((reuse-cache (if force-fresh 'fetch-latest t)))
    (with-current-buffer (get-buffer-create "*Dashboard*")
      (let ((inhibit-read-only t))
        (erase-buffer)
        (org-mode)
        (insert "#+title: Dashboard\n\n")
        (insert "#+note: Avoid AI, you enjoy coding and PL so take pride and joy in “artisanally handcrafted code”! Also ‘./ai_usage.sh’!\n\n")
        ;; Clock + weather share one line, no heading --- the flags/emoji are
        ;; self-explanatory (hover for locations / conditions).
        (dashboard/world-clock :insert reuse-cache)
        ;; Drop the clock vtable's trailing newline so the weather vtable
        ;; splices onto the *same* line:  🇯🇵 … 🇮🇳 …  🌤️ 20°C 68°F
        (when (eq (char-before) ?\n) (delete-char -1))
        (insert " ~  ")
        (dashboard/weather :insert reuse-cache)
        (insert "\n\n")
        ;; Center the top-goals heading + its rows: org-ql is synchronous (its
        ;; splice lands before the call returns), so the region from here to
        ;; point after the view covers the rendered content.
        (let ((goals-start (point)))
          (insert (dashboard--heading-link 'dashboard/top-goals "⚡ Top goals for the month") "\n")
          (dashboard/top-goals :insert reuse-cache)
          ;; `center-region' centers within `fill-column' (default 80), too
          ;; narrow for these wide lines; bind it to the display width so they
          ;; center across the whole window (falling back to the frame when the
          ;; dashboard isn't shown in a window yet).
          (let ((fill-column (if-let ((w (get-buffer-window (current-buffer))))
                                 (window-width w)
                               (frame-width))))
            (center-region goals-start (point))))
        (insert "\n\n/Ensure what you're working on is in service of these goals ---or change the goals!/\n\n")

        (dashboard/work-calendar :insert reuse-cache)

        (insert "\n* 📩 Process Inbox" "\n\n")
        
        (let ((start (point-marker)))
          (insert "\n\n** " (dashboard--heading-link 'oag-reviews-needed "👀 Whom am I blocking?")
                  "\n\n---review their work---\n\n")
          (oag-reviews-needed :insert reuse-cache
                              :on-inserted (dashboard--hider reuse-cache start)))

        (let ((start (point-marker)))
          (insert "\n\n** " (dashboard--heading-link 'oag-my-changes-needing-action "🔧 Feedback I need to address")
                  "\n\n---address it, then re-publish---\n\n")
          (oag-my-changes-needing-action :insert reuse-cache
                                         :on-inserted (dashboard--hider reuse-cache start)))

        (let ((start (point-marker)))
          (insert "\n\n** "
                  (dashboard--heading-link 'whatsapp/contacts "🫶 Social Connection") "\n\n")
          (insert "---Reach out to someone today; relationships need tending. Press `d' to send a greeting.---\n\n")
          (whatsapp/contacts :insert reuse-cache
                             :on-inserted (dashboard--hider reuse-cache start)))

        (let ((start (point-marker)))
          (insert "\n\n** "
                  (dashboard--heading-link 'actionable-mail/gmail-inbox "Personal Email") "\n\n")
          (actionable-mail/gmail-inbox :insert reuse-cache
                                       :on-inserted (dashboard--hider reuse-cache start)))
        (let ((start (point-marker)))
          (insert "\n\n** " (dashboard--heading-link 'dashboard/inbox "Quick Captures") "\n\n")
          (dashboard/inbox :insert reuse-cache
                           :on-inserted (dashboard--hider reuse-cache start)))
        (let ((start (point-marker)))
          (insert "\n\n** " (dashboard--heading-link 'dashboard/rss-feeds "RSS") "\n\n")
          (dashboard/rss-feeds :insert reuse-cache
                               :on-inserted (dashboard--hider reuse-cache start)))

        (insert "\n\n* 📋 Urgent and unstarted")
        (let ((start (point-marker)))
          (insert "\n\n** "
                  (dashboard--heading-link 'dashboard/priority-a-unscheduled "🔴 Priority A, unscheduled") "\n\n")
          (insert "---Either schedule this stuff, or make peace with the fact that it's not high priority---\n\n")
          (dashboard/priority-a-unscheduled :insert reuse-cache
                                            :on-inserted (dashboard--hider reuse-cache start)))
        (insert "\n\n* 📆 Planning")
        (let ((start (point-marker)))
          (insert "\n\n** "
                  (dashboard--heading-link 'dashboard/waiting "💢 I've been waiting on these for over a week, send reminder!") "\n\n")
          (dashboard/waiting :insert reuse-cache
                             :on-inserted (dashboard--hider reuse-cache start)))
        (let ((start (point-marker)))
          (insert "\n\n** " (dashboard--heading-link 'oag-jira-urgent-not-started "🔥 Jira: Urgent Not Yet Started") "\n\n")
          (insert "---pick one, scope it, push a draft---\n\n")
          (oag-jira-urgent-not-started :insert reuse-cache
                                       :on-inserted (dashboard--hider reuse-cache start)))
        (let ((start (point-marker)))
          (insert "\n\n** " (dashboard--heading-link 'oag-jira-active-no-gerrit "⚠️ Jira says active, Gerrit disagrees") "\n\n")
          (insert (concat
                   "These tickets are marked In Progress or In Review in Jira, yet\n"
                   "none of my open Gerrit changes reference them.  For each row,\n"
                   "pick exactly one:\n"
                   "  - Push a draft change that cites the ticket in its footer, or\n"
                   "  - Move the ticket back to To Do / Blocked --- the status is lying, or\n"
                   "  - Reassign it, if someone else is actually carrying the work.\n"
                   "Leaving a ticket here is a promise you are silently breaking.\n\n"))
          (oag-jira-active-no-gerrit :insert reuse-cache
                                     :on-inserted (dashboard--hider reuse-cache start)))
        (insert "\n\n** 📆 Overdue")
        (let ((start (point-marker)))
          (insert "\n\n*** "
                  (dashboard--heading-link 'dashboard/deadline-overdue "Past deadline due-date") "\n\n")
          (dashboard/deadline-overdue :insert reuse-cache
                                      :on-inserted (dashboard--hider reuse-cache start)))
        (let ((start (point-marker)))
          (insert "\n\n*** " (dashboard--heading-link 'dashboard/overdue "Past scheduled start date") "\n\n")
          (dashboard/overdue :insert reuse-cache
                             :on-inserted (dashboard--hider reuse-cache start)))
        (let ((start (point-marker)))
          (insert "\n\n** "
                  (dashboard--heading-link 'dashboard/open-loops "🤡 Please 𝒓𝒆𝒅𝒖𝒄𝒆 the number of (unscheduled) open loops") "\n\n")
          (dashboard/open-loops :insert reuse-cache
                                :on-inserted (dashboard--hider reuse-cache start)))
        (let ((start (point-marker)))
          (insert "\n\n** " (dashboard--heading-link 'dashboard/work-in-progress "🚧 Gerrit: Work In Progress") "\n\n")
          (dashboard/work-in-progress :insert reuse-cache
                                      :on-inserted (dashboard--hider reuse-cache start))))
      (goto-char (point-min))))
  (pop-to-buffer "*Dashboard*"))

;; Loading the calendar feature makes Org timestamps open it on click.
(actionable-query-enable-timestamp-views)

(provide 'dashboard)
;;; dashboard.el ends here
