;;; dashboard.el --- One-buffer overview: email + RSS + Gerrit/Jira + org-ql  -*- lexical-binding: t; -*-
;;
;; `M-x dashboard' opens a single org-mode buffer, structured as:
;;
;;   ⚡ Top goals for the month  -- org-ql, no heading, right under the title
;;   * 📩 Process Inbox          -- Email, Quick Captures, RSS
;;   * 📋 Urgent and unstarted   -- Priority A unscheduled (org-ql)
;;                                + Whom am I blocking? / Feedback I need
;;                                  to address (+ Waiting on others, nested)
;;                                  (org-agenda-gerrit.el lenses 1 & 2)
;;   * 📆 Planning               -- Jira urgent-not-started, Jira-vs-Gerrit
;;                                disagreement, Overdue (deadline + scheduled,
;;                                nested), Reduce open loops, Gerrit: WIP
;;
;; splicing `actionable-mail/gmail-inbox' (actionable-mail.el),
;; `dashboard/rss-feeds' (on top of actionable-query/data/aq-data-rss.el),
;; and the rest via the `:insert 'fetch-latest' contract, so each view's
;; own actions work in place.

(require 'actionable-mail)
(require 'ts)
(load-file "~/actionable-query/data/aq-data-rss.el")
(load-file "~/actionable-query/data/aq-data-org-ql.el")
(add-to-list 'load-path "~/actionable-query/org-agenda-gerrit")
(require 'org-agenda-gerrit)
(add-to-list 'load-path "~/actionable-query/whats-app")
(require 'whats-app)
(require 'aq-org-timestamp)

(defvar dashboard-rss-feeds nil
  "List of (NAME KIND URL) feeds shown on the dashboard. KIND is `rss' or `atom'.")
(setq dashboard-rss-feeds
      '(("Hacker News"        rss  "https://news.ycombinator.com/rss")
        ;; ("Lobste.rs"          rss  "https://lobste.rs/rss")
        ("Planet Emacslife"   atom "https://planet.emacslife.com/atom.xml")
        ("Bubbles"            atom "https://bubbles.town/feed")
        ("r/shia"             atom "https://www.reddit.com/r/shia.rss")))

(defvar dashboard-rss-actions
  `(("RET" "Open in browser" ,(lambda (o) (browse-url (plist-get o :url))))
    ("w"   "Copy URL"        ,(lambda (o) (kill-new (plist-get o :url))
                                (message "Copied: %s" (plist-get o :url))))
    ("c"   "Capture as TODO" ,(lambda (o)
                                (org-capture-string
                                 (format "* TODO [[%s][%s]]" (plist-get o :url) (plist-get o :title))
                                 "t")))))

(actionable-query-defview dashboard/rss-feeds "📰 RSS feeds"
  :auto-refresh "30 minutes"
  :objects
  (lambda (callback)
    (let* ((results (make-hash-table :test #'equal))
           (pending (length dashboard-rss-feeds)))
      (dolist (feed dashboard-rss-feeds)
        (cl-destructuring-bind (name kind url) feed
          (funcall (if (eq kind 'atom) #'aq--fetch-atom #'aq--fetch-rss)
                   url
                   (lambda (items)
                     (puthash name items results)
                     (setq pending (1- pending))
                     (when (zerop pending)
                       (funcall callback
                                (cl-loop for (name _kind _url) in dashboard-rss-feeds
                                         append (list name (gethash name results)))))))))))
  :columns    '((:name "Date"
                        :width 12
                        :getter    (lambda (o &rest _) (aq--format-pubdate (plist-get o :date)))
                        :displayer (lambda (v w _) (propertize (truncate-string-to-width v w)
                                                          'face '(:height 0.8 :foreground "gray50"))))
                (:name "Title"  ; no :width -> vtable sizes to the widest title
                       :getter (lambda (o &rest _) (or (plist-get o :title) "?"))))
  :help-echo  (lambda (o) (plist-get o :description))
  :actions    dashboard-rss-actions)

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

;; ── World clock ──────────────────────────────────────────────────────────
;; The current time across the zones I collaborate in.  No fetch, no cache:
;; the time "now" in any zone is pure local computation via
;; `format-time-string' with a TZ string --- so `g' just re-renders.

(defvar dashboard-clock-zones
  '(("🇯🇵" "Asia/Tokyo"          "Japan (Tokyo)")
    ("🇨🇦" "America/Toronto"     "Canada (Toronto)")
    ("🇺🇸" "America/Los_Angeles" "California (Los Angeles)")
    ("🇮🇳" "Asia/Kolkata"        "India (Kolkata)"))
  "Zones shown by `dashboard/world-clock', in display order.
Each entry is (FLAG TZ LOCATION): FLAG is the emoji shown in the cell, TZ is
any value the OS accepts in the `TZ' env var (an IANA zone name), and
LOCATION is the human name surfaced via help-echo / tooltip.")

(defvar dashboard--clock-anchor nil
  "Universal time (a `current-time'-style value) the world-clock is pinned to.
Nil means \"now\" --- each open re-reads the live clock.  Editing any row's
Time sets this so every other zone recomputes off the same instant; `g'
clears it back to nil (live).")

(defun dashboard--clock-instant ()
  "The instant the clock is showing: the pinned anchor, or now."
  (or dashboard--clock-anchor (current-time)))

(defun dashboard--clock-parse (input tz)
  "Parse user INPUT (e.g. \"3pm\", \"15:30\", \"tomorrow 9am\") as a time in zone TZ.
Returns a universal time.  Leans on `org-read-date', interpreting the typed
time as wall-clock in TZ so the other zones convert correctly."
  (let* ((parsed (org-read-date t t input nil (dashboard--clock-instant)))
         ;; `org-read-date' parses against the *local* zone; re-encode the same
         ;; wall-clock fields under TZ so "3pm" means 3pm in THAT city.
         (dt     (decode-time parsed (current-time-zone) t)))
    (encode-time (append (list (decoded-time-second dt) (decoded-time-minute dt)
                               (decoded-time-hour dt) (decoded-time-day dt)
                               (decoded-time-month dt) (decoded-time-year dt))
                         (list nil -1 tz)))))

(defun dashboard--clock-cell (zone)
  "Render ZONE ((FLAG TZ LOCATION)) as \"🇯🇵 Thu 4:00 AM\" with a location tooltip.
The LOCATION rides a `help-echo' text-property, so hovering the cell reveals
which city the flag stands for."
  (let* ((flag (nth 0 zone)) (tz (nth 1 zone)) (loc (nth 2 zone))
         (time (format-time-string "%a %-I:%M %p" (dashboard--clock-instant) tz)))
    (propertize (format "%s %s" flag time) 'help-echo loc)))

(defun dashboard--clock-columns ()
  "Build one vtable column per zone --- each cell `flag + time', editable.
Editing a cell pins that wall-clock instant (via `dashboard--clock-parse'),
so every other zone recomputes off it; the table reverts whole on `e'."
  (mapcar
   (lambda (zone)
     (list :name "" :width 18
           :getter (lambda (&rest _) (dashboard--clock-cell zone))
           :editable t
           :setter (lambda (_o new-value)
                     (setq dashboard--clock-anchor
                           (dashboard--clock-parse new-value (nth 1 zone))))))
   dashboard-clock-zones))

(actionable-query-defview dashboard/world-clock "🕐 World clock"
  ;; A single row; each zone is its own column, so the four clocks sit on one
  ;; line.  The lone object is a placeholder --- every column getter ignores it
  ;; and reads its zone from the closure built by `dashboard--clock-columns'.
  :objects (lambda () (list (list :clock t)))
  ;; Header-line (not a buffer row) so the empty column names don't print a
  ;; blank line --- which also lets the weather table butt onto the same line.
  :use-header-line t
  :no-footer t
  :columns (dashboard--clock-columns)
  :actions `(("G" "Reset to the live time (drop the pinned instant)"
              ,(lambda (_o)
                 (setq dashboard--clock-anchor nil)
                 (actionable-query-refresh-current-view)
                 (message "World clock back to live time.")))))

;; ── Weather ────────────────────────────────────────────────────────────────
;; A tiny "what's it like outside" widget atop the dashboard.  I work with
;; Americans and never internalised Fahrenheit, so every row shows BOTH units
;; side by side --- the point of an /actionable/ query: not a bare number, but
;; one I can act on.  Seeded from my current location (wttr.in geolocates by
;; IP, no API key), fetched async so the dashboard opens instantly, and cached
;; so later opens reuse it; `g'/`G' (or `C-u M-x dashboard') refetch.

(defvar dashboard--weather-cache nil
  "Cached weather plist (:c :f :feels-c :feels-f :desc :place), or nil.
Populated by the async fetch in `dashboard/weather'; cleared by `G'.")

(defun dashboard--weather-parse (json-string)
  "Parse wttr.in `?format=j1' JSON-STRING into a weather plist, or nil on garbage."
  (ignore-errors
    (let* ((d    (json-parse-string json-string :object-type 'alist))
           (cur  (elt (alist-get 'current_condition d) 0))
           (area (elt (alist-get 'nearest_area d) 0))
           (name (lambda (k) (alist-get 'value (elt (alist-get k area) 0)))))
      (list :c       (alist-get 'temp_C cur)
            :f       (alist-get 'temp_F cur)
            :feels-c (alist-get 'FeelsLikeC cur)
            :feels-f (alist-get 'FeelsLikeF cur)
            :desc    (alist-get 'value (elt (alist-get 'weatherDesc cur) 0))
            :place   (string-join (delq nil (list (funcall name 'areaName)
                                                  (funcall name 'region)))
                                  ", ")))))

(defun dashboard--weather-fetch (callback)
  "Curl wttr.in for the current location async, then call CALLBACK with two rows.
Each row is a plist (:label :c :f).  A failed/missing curl yields no rows
\(the prose-bottom then says so) rather than freezing or erroring the view."
  (if (not (executable-find "curl"))
      (funcall callback nil)
    (let ((buf (generate-new-buffer " *dashboard-weather*")))
      (make-process
       :name "dashboard-weather" :buffer buf :noquery t
       :command '("curl" "-s" "--max-time" "10" "wttr.in/?format=j1")
       :sentinel
       (lambda (proc _event)
         (when (memq (process-status proc) '(exit signal))
           (let ((w (dashboard--weather-parse
                     (with-current-buffer buf (buffer-string)))))
             (kill-buffer buf)
             (setq dashboard--weather-cache w)
             (funcall callback
                      (when w (list (dashboard--weather-row w)))))))))))

(defun dashboard--weather-row (w)
  "Build the single weather vtable row plist from weather plist W."
  (list :c (plist-get w :feels-c) :f (plist-get w :feels-f)
        :desc (plist-get w :desc) :place (plist-get w :place)))

(defun dashboard--weather-objects (callback)
  "Async `:objects': reuse `dashboard--weather-cache' if present, else fetch."
  (if dashboard--weather-cache
      (funcall callback (list (dashboard--weather-row dashboard--weather-cache)))
    (dashboard--weather-fetch callback)))

(defun dashboard--weather-emoji (desc)
  "Map a wttr.in DESC string to a weather emoji (best-effort, default 🌤️)."
  (let ((d (downcase (or desc ""))))
    (cond ((string-match-p "thunder\\|storm" d) "⛈️")
          ((string-match-p "snow\\|sleet\\|ice"  d) "❄️")
          ((string-match-p "rain\\|drizzle\\|shower" d) "🌧️")
          ((string-match-p "fog\\|mist\\|haze"   d) "🌫️")
          ((string-match-p "overcast\\|cloud"    d) "☁️")
          ((string-match-p "clear\\|sunny"       d) "☀️")
          (t "🌤️"))))

(defun dashboard--weather-advice (celsius)
  "One actionable line for CELSIUS (a number-ish string), in the README's voice."
  (let ((c (string-to-number (or celsius "15"))))
    (cond
     ((< c 0)  "Frost abroad --- coat, hat, and gloves; tarry not outside.")
     ((< c 10) "A chill prevails --- let a jacket be thy companion.")
     ((< c 20) "Mild and temperate --- go forth in peace.")
     ((< c 30) "The sun smiles gently --- a fair day; dress light.")
     (t        "Great heat besets the land --- drink water, seek the shade."))))

(defun dashboard--weather-set-temp (o new unit)
  "Set row O's UNIT (`:c' or `:f') to NEW, recomputing the sibling unit.
NEW may carry a stray degree suffix (\"72°F\"); we read the leading number.
Lets the weather rows double as a °C↔°F converter (the README's example):
type a Fahrenheit a colleague quoted and read the Celsius you understand."
  (unless (string-match "-?[0-9]+\\.?[0-9]*" new)
    (user-error "Temperature must be a number, got %S" new))
  (let* ((n (string-to-number (match-string 0 new)))
         (c (if (eq unit :c) n (/ (* (- n 32) 5.0) 9)))
         (f (if (eq unit :f) n (+ (* c 9.0 (/ 1.0 5)) 32))))
    (plist-put o :c (format "%d" (round c)))
    (plist-put o :f (format "%d" (round f)))))

(actionable-query-defview dashboard/weather "🌡️ Weather"
  :auto-refresh "1 hour"
  :objects #'dashboard--weather-objects
  :actions `(("G" "Refetch the weather now (g reuses the cache)"
              ,(lambda (_o)
                 (setq dashboard--weather-cache nil)
                 (actionable-query-refresh-current-view)
                 (message "Refetching weather…"))))
  ;; Header-line (not a buffer row): empty column names print no blank line,
  ;; so the weather sits on the world-clock's line.  The unit stays visible
  ;; per-row (the getter appends °C/°F).
  :use-header-line t
  :no-footer t
  :columns `((:name "" :width 3
                    :getter ,(lambda (o &rest _)
                               (propertize (dashboard--weather-emoji (plist-get o :desc))
                                           'help-echo (format "%s in %s"
                                                              (or (plist-get o :desc) "?")
                                                              (or (plist-get o :place) "?")))))
             (:name "" :width 6 :align right :editable t
                    :getter ,(lambda (o &rest _) (format "%s°C" (plist-get o :c)))
                    :setter ,(lambda (o new) (dashboard--weather-set-temp o new :c)))
             (:name "" :width 6 :align right :editable t
                    :getter ,(lambda (o &rest _) (format "%s°F" (plist-get o :f)))
                    :setter ,(lambda (o new) (dashboard--weather-set-temp o new :f))))
  :prose-bottom
  (let ((w dashboard--weather-cache))
    (insert (if w
                (propertize (format "%s in %s.  %s"
                                    (plist-get w :desc) (plist-get w :place)
                                    (dashboard--weather-advice (plist-get w :c)))
                            'face 'success)
              (propertize "Fetching weather… (needs curl on PATH)"
                          'face '(:foreground "gray50"))))))

;; ── Viewing date ─────────────────────────────────────────────────────────
;; The calendar normally shows today, but `dashboard-work-calendar-date' lets it show
;; any date.  All three event sources (gcal, scheduled-org, holidays) resolve
;; their day through `dashboard--effective-date', and the bottom-prose date
;; stamp lets the user jump to another day.

(defvar dashboard-work-calendar-date nil
  "Date the work-calendar is showing, as (MONTH DAY YEAR), or nil for today.
Global (not buffer-local): the render path re-enters `org-mode', which wipes
buffer-locals, so the viewed day must live outside the buffer.  Only one
work-calendar is viewed at a time, so a global is fine.")

(defun dashboard--effective-date ()
  "The (MONTH DAY YEAR) the calendar is currently showing (today if unset)."
  (or dashboard-work-calendar-date (calendar-current-date)))

(defun dashboard--effective-day-string ()
  "The effective viewing date as a \"YYYY-MM-DD\" string (gcalcli's format)."
  (pcase-let ((`(,m ,d ,y) (dashboard--effective-date)))
    (format "%04d-%02d-%02d" y m d)))

(defun dashboard--effective-next-day-string ()
  "The day after the effective date as \"YYYY-MM-DD\" (gcalcli's exclusive end)."
  (pcase-let ((`(,m ,d ,y) (dashboard--effective-date)))
    (let ((next (calendar-gregorian-from-absolute
                 (1+ (calendar-absolute-from-gregorian (list m d y))))))
      (format "%04d-%02d-%02d" (nth 2 next) (nth 0 next) (nth 1 next)))))

(defun dashboard--effective-org-date ()
  "The effective viewing date as an Org timestamp inner string \"YYYY-MM-DD Dow\"."
  (pcase-let ((`(,m ,d ,y) (dashboard--effective-date)))
    (format-time-string "%Y-%m-%d %a"
                        (encode-time 0 0 12 d m y))))

;; Self-contained gcal fetch: shell out to gcalcli for the viewing date.
;; Kept local to dashboard.el rather than reaching into init.org's tangled
;; `my/standup-from-schedule--gcal-events', which isn't on the load path here.
;; TSV columns (--details all): 0 id 1 start_date 2 start_time 3 end_date
;; 4 end_time 5 html_link 6 hangout_link 7 conf_type 8 conf_uri 9 title
;; 10 location 11 description ...  Requires `my\gcalcli-calendar' (private.el).

(defvar dashboard--gcal-cache nil
  "Cons (DAY-STRING . EVENTS) memoizing the last gcalcli pull, or nil.
`dashboard--gcal-events-today' reuses it for the same day so a plain `g'
refresh doesn't re-shell out; `G' / `dashboard--gcal-refetch' clears it.")

(defvar dashboard--gcal-fetched-at nil
  "Time of the last actual gcalcli fetch (a `current-time' value), or nil.")

(defun dashboard--gcal-clear-cache ()
  "Drop the cached gcalcli results so the next pull re-fetches."
  (setq dashboard--gcal-cache nil))

(defun dashboard--gcal-events-today ()
  "Google Calendar events for the viewing date as plists (:start :end :title
:location :description :conference :url), noisy entries filtered out.  Cached
per day (see `dashboard--gcal-cache'); call `dashboard--gcal-clear-cache' to
force a re-fetch.  Errors if gcalcli is missing/unconfigured rather than
returning a misleading nil."
  ;; We don't work weekends --- skip the (slow) gcalcli calls on Sat/Sun.
  (if (memq (calendar-day-of-week (dashboard--effective-date)) '(0 6))  ; 0=Sun 6=Sat
      nil
    (let ((day (dashboard--effective-day-string)))
      (if (equal (car dashboard--gcal-cache) day)
          (cdr dashboard--gcal-cache)
        (let ((events (dashboard--gcal-fetch-today)))
          (setq dashboard--gcal-cache    (cons day events)
                dashboard--gcal-fetched-at (current-time))
          events)))))

(defvar dashboard-gcal-accounts nil
  "Extra gcalcli accounts to pull, beyond the default `my\\gcalcli-calendar'.
Each entry is (CALENDAR . CONFIG-FOLDER): CALENDAR is the --calendar name,
CONFIG-FOLDER is gcalcli's --config-folder for a *separate* Google login
\(or nil to use the default config).  Set in private.el.

A personal calendar on a *different* Google account is easier to add via its
secret iCal URL --- see `dashboard-ics-calendars' --- which needs no second
gcalcli login.")

(defun dashboard--gcal-fetch-today ()
  "Shell out to gcalcli for today's events across all configured accounts.
Pulls the default `my\\gcalcli-calendar' plus every entry in
`dashboard-gcal-accounts', merging the results.  See
`dashboard--gcal-events-today'."
  (unless (boundp 'my\gcalcli-calendar)        ; secrets live outside the repo
    (load "~/Dropbox/private.el" 'no-error 'no-message))
  (unless (boundp 'my\gcalcli-calendar)
    (user-error "`my\\gcalcli-calendar' is unbound --- set it in ~/Dropbox/private.el"))
  (unless (executable-find "gcalcli")
    (user-error "`gcalcli' not found on PATH --- brew install gcalcli"))
  (let ((accounts (cons (cons my\gcalcli-calendar nil)   ; default work account
                        dashboard-gcal-accounts)))
    (cl-loop for (cal . config-folder) in accounts
             append (dashboard--gcal-fetch-account cal config-folder))))

(defun dashboard--gcal-fetch-account (calendar config-folder)
  "Fetch today's events from CALENDAR via gcalcli, optionally under CONFIG-FOLDER.
CONFIG-FOLDER selects a separate gcalcli login (for another Google account);
nil uses the default config.  Returns event plists; a failing account logs a
message and yields nil rather than aborting the whole fetch."
  (let* ((today    (dashboard--effective-day-string))
         (tomorrow (dashboard--effective-next-day-string))
         (args (append (when config-folder
                         (list "--config-folder" (expand-file-name config-folder)))
                       (list "--calendar" calendar
                             "agenda" today tomorrow
                             "--tsv" "--details" "all")))
         (ok t)
         (raw (with-output-to-string
                (with-current-buffer standard-output
                  (unless (zerop (apply #'call-process "gcalcli" nil t nil args))
                    (setq ok nil)))))
         (nilify (lambda (s) (if (or (null s) (string-empty-p s)) nil s)))
         events)
    (if (not ok)
        (progn
          (message "gcalcli failed for %s%s --- skipping (re-auth with `gcalcli init')"
                   calendar (if config-folder (format " (%s)" config-folder) ""))
          nil)
      (dolist (line (cdr (split-string raw "\n" t)))  ; cdr: skip header row
        (let ((f (split-string line "\t")))
          (when (and (>= (length f) 10) (equal (nth 1 f) today))
            (push (list :start       (funcall nilify (nth 2 f))
                        :end         (funcall nilify (nth 4 f))
                        :url         (funcall nilify (nth 5 f))
                        :conference  (funcall nilify (or (nth 8 f) (nth 6 f)))
                        :title       (nth 9 f)
                        :location    (funcall nilify (nth 10 f))
                        :description (funcall nilify (nth 11 f)))
                  events))))
      ;; gcalcli's --tsv schema omits attendees (a known limitation), but its
      ;; human-readable view lists them.  Make one supplementary call, parse a
      ;; title→attendees map, and graft it onto the events.
      (let ((att-by-title (dashboard--gcal-attendees-by-title today tomorrow calendar config-folder)))
        (dolist (ev events)
          (when-let ((a (gethash (plist-get ev :title) att-by-title)))
            (plist-put ev :attendees a))))
      ;; Drop generic/noisy entries (Busy/Home/Office/OOO) --- they never act.
      (cl-remove-if (lambda (ev)
                      (let ((title (plist-get ev :title)))
                        (or (null title)
                            (string-match-p "\\`\\(?:Busy\\|Home\\|Office\\|OOO\\|.* OOO.*\\)\\'" title))))
                    (nreverse events)))))

(defun dashboard--gcal-attendees-by-title (today tomorrow calendar config-folder)
  "Title -> (list of attendee emails) for CALENDAR's events TODAY..TOMORROW.
CONFIG-FOLDER selects gcalcli's account config (nil = default).  gcalcli's
`--tsv' drops attendees, so we scrape its human-readable `--details
attendees' output: each event headline is followed by an \"Attendees:\"
block of `… <email>' lines until the next headline."
  (let ((table (make-hash-table :test #'equal))
        (raw (with-output-to-string
               (with-current-buffer standard-output
                 (ignore-errors
                   (apply #'call-process "gcalcli" nil t nil
                          (append (when config-folder
                                    (list "--config-folder" (expand-file-name config-folder)))
                                  (list "--calendar" calendar
                                        "agenda" today tomorrow
                                        "--details" "attendees" "--details" "title")))))))
        cur)
    (dolist (line (split-string raw "\n"))
      ;; Strip ANSI colour codes gcalcli emits, then trim.
      (let ((clean (string-trim (replace-regexp-in-string "\033\\[[0-9;]*m" "" line))))
        (cond
         ;; An "  HH:MM   Title" line starts a new event.
         ((string-match "\\`[0-9][0-9]?:[0-9][0-9]\\s-+\\(.+\\)\\'" clean)
          (setq cur (string-trim (match-string 1 clean))))
         ;; "… <email>" attendee line under the current event.
         ((and cur (string-match "<\\([^<>@ ]+@[^<>@ ]+\\)>" clean))
          (push (match-string 1 clean) (gethash cur table)))
         ((string-empty-p clean) nil))))
    ;; Reverse each list back to source order.
    (maphash (lambda (k v) (puthash k (nreverse v) table)) table)
    table))

;; ── ICS (secret iCal URL) source ───────────────────────────────────────────
;; A personal calendar on a *different* Google account is a pain to auth via
;; gcalcli (4.x ignores --config-folder for init and bundles no OAuth client).
;; Its secret iCal URL (Calendar Settings → \"Secret address in iCal format\")
;; sidesteps all of that, zero-auth.  But a Google ICS export is the whole
;; history, with today's occurrences hiding inside RRULEs --- so rather than
;; hand-roll recurrence expansion, we lean on Emacs's own machinery: import the
;; ICS to a temp diary file with `icalendar-import-buffer', then ask
;; `diary-list-entries' for the target day (the diary engine expands RRULEs).

(defvar dashboard-ics-calendars nil
  "Personal calendars to merge in via their secret iCal URLs.
Each entry is (LABEL . URL); LABEL tags the source (currently informational),
URL is the calendar's \"Secret address in iCal format\".  Set in private.el:

  (setq dashboard-ics-calendars
        \\='((\"Personal\" . \"https://calendar.google.com/calendar/ical/…/basic.ics\")))")

(defvar dashboard--ics-cache nil
  "Cons (DAY-STRING . EVENTS) caching the merged ICS events for one day.")

(defun dashboard--gcal-day-url (date)
  "Google Calendar day-view URL for DATE ((M D Y) list).
Google's ICS export carries no per-event URL, so `w' on an ICS event lands
on its day in the web calendar (where you click the event itself)."
  (pcase-let ((`(,m ,d ,y) date))
    (format "https://calendar.google.com/calendar/u/0/r/day/%d/%d/%d" y m d)))

(defun dashboard--ics-entry->plist (entry)
  "Turn a `diary-list-entries' ENTRY into a calendar event plist, or nil.
ENTRY is (DATE TEXT …); TEXT looks like \"HH:MM-HH:MM Title\\n Desc: …\".
Returns (:start :end :title :location :description :url) --- the shape
`dashboard--gcal-events-today' yields, so ICS events merge through one path.
`:url' is the event's day in Google Calendar (the export has no event URL)."
  (let* ((text  (nth 1 entry))
         (head  (car (split-string text "\n")))   ; first line: time + title
         (rest  (cdr (split-string text "\n")))
         (url   (dashboard--gcal-day-url (nth 0 entry)))
         (field (lambda (tag) (cl-loop for l in rest
                                       when (string-match (concat "\\` *" tag ": *\\(.*\\)") l)
                                       return (string-trim (match-string 1 l))))))
    (when head
      (if (string-match "\\`\\([0-9][0-9]?:[0-9][0-9]\\)\\(?:-\\([0-9][0-9]?:[0-9][0-9]\\)\\)? *\\(.*\\)" head)
          (list :start (match-string 1 head)
                :end   (match-string 2 head)
                :title (string-trim (match-string 3 head))
                :location    (funcall field "Location")
                :description (funcall field "Desc")
                :url url)
        ;; All-day (no leading time): whole head is the title.
        (list :start nil :end nil :title (string-trim head)
              :location (funcall field "Location")
              :description (funcall field "Desc")
              :url url)))))

(defun dashboard--ics-events-for (date)
  "Personal-calendar events for DATE ((M D Y) list), cached per day.
Curls each URL in `dashboard-ics-calendars', imports it to a temp diary file
via `icalendar-import-buffer', and lists DATE's entries with
`diary-list-entries' (which expands recurring events).  A failing fetch is
skipped with a message."
  (unless (boundp 'dashboard-ics-calendars)
    (load "~/Dropbox/private.el" 'no-error 'no-message))
  (when (bound-and-true-p dashboard-ics-calendars)
    (require 'icalendar)
    (require 'diary-lib)
    (let ((day (dashboard--effective-day-string)))
      (if (equal (car dashboard--ics-cache) day)
          (cdr dashboard--ics-cache)
        (let ((events
               (cl-loop for (label . url) in dashboard-ics-calendars
                        append (dashboard--ics-fetch-one label url date))))
          (setq dashboard--ics-cache (cons day events))
          events)))))

(defun dashboard--ics-fetch-one (label url date)
  "Curl URL, import to a temp diary, return DATE's event plists.  LABEL names it."
  (let ((ics (with-output-to-string
               (with-current-buffer standard-output
                 (unless (zerop (call-process "curl" nil t nil
                                              "-sL" "--max-time" "15" url))
                   (message "ICS fetch failed for %s" label))))))
    (when (and ics (string-match-p "VEVENT" ics))
      (let ((tmp (make-temp-file "aq-ics-" nil ".diary")))
        (unwind-protect
            (progn
              (with-temp-buffer (insert ics) (icalendar-import-buffer tmp t t))
              (let* ((diary-file tmp)
                     (diary-list-include-blanks nil)
                     (diary-display-function #'ignore)
                     (entries (ignore-errors (diary-list-entries date 1 t))))
                (delq nil (mapcar #'dashboard--ics-entry->plist entries))))
          (ignore-errors (delete-file tmp))
          (ignore-errors (delete-file (concat tmp "~"))))))))

(defun dashboard--ics-clear-cache ()
  "Drop the cached ICS results so the next pull re-fetches."
  (setq dashboard--ics-cache nil))

;; ── Org day-agenda source ─────────────────────────────────────────────────
;; Clicking an org timestamp shows a one-day agenda: SCHEDULED + DEADLINE +
;; active body timestamps + diary sexps, repeater-aware.  Rather than
;; re-implement that, we reuse org's own `org-agenda-get-day-entries', which
;; yields propertized strings carrying `txt' / `time-of-day' / `org-marker' /
;; `tags' / `type'.  This is a strict superset of the old SCHEDULED-only scan.

(defun dashboard--tod->hhmm (tod)
  "Convert an org `time-of-day' integer (e.g. 930) to \"HH:MM\", or nil."
  (when (integerp tod)
    (format "%02d:%02d" (/ tod 100) (% tod 100))))

(defun dashboard--clean-agenda-title (txt &optional todo)
  "Clean agenda heading TXT, returning (TITLE . COUNTDOWN).
Strips a leading TODO keyword + `[#A]' priority and a trailing `:tag:tag:'
block.  An org countdown suffix --- `⟪ -- in 6 days\\&⟫' on diary/anniversary
sexps --- is pulled OUT of the title into COUNTDOWN (\"in 6 days\"), so the
view can show it in the Time column instead of inline noise.  COUNTDOWN is nil
when absent.  TODO is the entry's known keyword (from `todo-state')."
  (let ((s (string-trim txt)) countdown)
    ;; Countdown marker `⟪ -- in 6 days\&⟫' anywhere in the heading.
    (when (string-match "⟪[ \t]*-*[ \t]*\\(.*?\\)[ \t]*\\\\?&?[ \t]*⟫" s)
      (setq countdown (string-trim (match-string 1 s))
            s (string-trim (replace-regexp-in-string "[ \t]*⟪.*?⟫[ \t]*" " " s))))
    ;; Trailing tag block:  "... :foo:bar:"
    (setq s (string-trim
             (replace-regexp-in-string "[ \t]+:[[:alnum:]_@#%:]+:[ \t]*\\'" "" s)))
    ;; Leading known TODO keyword (exact match, robust when org-todo-keywords-1
    ;; isn't populated --- we use the per-entry keyword the agenda gave us).
    (when (and todo (string-prefix-p (concat todo " ") s))
      (setq s (string-trim (substring s (length todo)))))
    ;; Leading priority cookie "[#A] ".
    (setq s (string-trim (replace-regexp-in-string "\\`\\[#[A-Z]\\][ \t]*" "" s)))
    (cons s (and countdown (not (string-empty-p countdown)) countdown))))

(defun dashboard--entry-effort-minutes (marker)
  "Minutes from the Effort property of the Org entry at MARKER, or nil.
Parses org's `H:MM' / bare-minute forms via `org-duration-to-minutes'."
  (when-let* (((markerp marker)) ((marker-buffer marker))
              (eff (org-with-point-at marker (org-entry-get (point) "Effort"))))
    (ignore-errors (truncate (org-duration-to-minutes eff)))))

(defun dashboard--agenda-events-for (date)
  "Calendar plists for org agenda items landing ON DATE (a (MONTH DAY YEAR) list).
Pulls SCHEDULED / DEADLINE / active body timestamps / sexps via
`org-agenda-get-day-entries' across `(org-agenda-files)', mapping each to
\(:start :end :title :tags :marker :type).  DONE items and habits are
dropped, and so are overdue carry-over / deadline-lookahead items whose own
date isn't DATE --- org-agenda piles those onto today, but a day view wants
only what is actually on the day.  All-day items have nil :start."
  (require 'org-agenda)
  (let ((org-agenda-new-buffers nil)
        (target-abs (calendar-absolute-from-gregorian date))
        events)
    (unwind-protect
        (dolist (file (org-agenda-files))
          (dolist (e (save-window-excursion
                       (apply #'org-agenda-get-day-entries file date
                              '(:scheduled :deadline :timestamp :sexp))))
            ;; `ts-date' is the item's own date; drop carry-over (past-scheduled)
            ;; and deadline-lookahead whose date isn't the day we're viewing.
            (when (let ((tsd (get-text-property 0 'ts-date e)))
                    (or (null tsd) (equal tsd target-abs)))
            (let* ((tod    (get-text-property 0 'time-of-day e))
                   (dur    (let ((d (get-text-property 0 'duration e)))
                             (and (numberp d) (truncate d))))   ; org gives a float
                   (start  (dashboard--tod->hhmm tod))
                   (todo   (get-text-property 0 'todo-state e))
                   (marker (or (get-text-property 0 'org-hd-marker e)
                               (get-text-property 0 'org-marker e)))
                   ;; End from the timestamp's own time-range (duration), or ---
                   ;; failing that --- synthesized from the entry's EFFORT, so a
                   ;; single-time task still claims a block for overlap checks.
                   (effort-min (and (integerp tod) (not (and dur (> dur 0))) marker
                                    (dashboard--entry-effort-minutes marker)))
                   (end-min (cond ((and (integerp tod) dur (> dur 0))
                                   (+ (* (/ tod 100) 60) (% tod 100) dur))
                                  ((and (integerp tod) effort-min)
                                   (+ (* (/ tod 100) 60) (% tod 100) effort-min))))
                   (end    (when end-min
                             (dashboard--tod->hhmm (+ (* (/ end-min 60) 100) (% end-min 60)))))
                   (habit  (and marker
                                (org-with-point-at marker (org-is-habit-p))))
                   (title+cd (dashboard--clean-agenda-title
                              (substring-no-properties
                               (or (get-text-property 0 'txt e) e))
                              todo)))
              (unless (or (member todo org-done-keywords) habit)
                (push (list :start  start
                            :end    end
                            :effort-derived (and effort-min t)
                            :title  (car title+cd)
                            :countdown (cdr title+cd)
                            :tags   (mapcar #'substring-no-properties
                                            (get-text-property 0 'tags e))
                            :marker marker
                            :type   (get-text-property 0 'type e))
                      events))))))
      ;; Release any files org opened for us, so repeated calls don't leak.
      (org-release-buffers org-agenda-new-buffers))
    (nreverse events)))

(defun dashboard--hhmm->min (hhmm)
  "\"HH:MM\" -> minutes-from-midnight, or nil."
  (when (and hhmm (string-match "\\`\\([0-9][0-9]?\\):\\([0-9][0-9]\\)\\'" hhmm))
    (+ (* 60 (string-to-number (match-string 1 hhmm)))
       (string-to-number (match-string 2 hhmm)))))

(defun dashboard--event-interval (o)
  "Event O's [start,end) in minutes.  A missing/blank end is treated as a
1-minute instant, so two events sharing a start time still collide."
  (when-let ((s (dashboard--hhmm->min (plist-get o :start))))
    (cons s (or (dashboard--hhmm->min (plist-get o :end)) (1+ s)))))

(defun dashboard--mark-overlaps (events)
  "Return EVENTS with `:overlap t' on any event whose time interval overlaps
another's (half-open test, mirroring the agenda's overlap highlighter)."
  (let ((ivs (mapcar #'dashboard--event-interval events)))
    (cl-loop for o in events for a in ivs for i from 0
             collect (if (and a (cl-loop for b in ivs for j from 0
                                         thereis (and (/= i j) b
                                                      (< (car a) (cdr b))
                                                      (< (car b) (cdr a)))))
                         (plist-put (copy-sequence o) :overlap t)
                       o))))

(defun dashboard--event-key (o)
  "Dedup key for event O: its title + start time.
Once an agenda key mints an Org tree from a gcal row, the same meeting
shows up twice on the next refresh --- once from gcalcli, once as the new
scheduled Org item.  They share title + start, so this collapses them."
  (cons (plist-get o :title) (plist-get o :start)))

(defvar dashboard--canadian-holidays
  '((holiday-fixed 1 1   "🇨🇦 New Year's Day")
    (holiday-easter-etc -2 "🇨🇦 Good Friday")
    (holiday-float 5 1 -1 "🇨🇦 Victoria Day" 24)   ; Monday on-or-before May 24
    (holiday-fixed 7 1   "🇨🇦 Canada Day")
    (holiday-float 8 1 1 "🇨🇦 Civic Holiday")       ; 1st Monday of August
    (holiday-float 9 1 1 "🇨🇦 Labour Day")          ; 1st Monday of September
    (holiday-fixed 9 30  "🇨🇦 Truth & Reconciliation Day")
    (holiday-float 10 1 2 "🇨🇦 Thanksgiving")        ; 2nd Monday of October
    (holiday-fixed 11 11 "🇨🇦 Remembrance Day")
    (holiday-fixed 12 25 "🇨🇦 Christmas Day")
    (holiday-fixed 12 26 "🇨🇦 Boxing Day"))
  "Canadian statutory/federal holidays as Emacs `holiday-*' specs.")

(defvar displayed-month)                ; bound dynamically by `holiday-*' forms
(defvar displayed-year)

(defun dashboard--holiday-events-today ()
  "All-day calendar plists for any Canadian holiday on the viewing date.
Pure Elisp via Emacs's built-in `holidays' --- no network, no extra calendar."
  (require 'holidays)
  (let* ((date (dashboard--effective-date))   ; (month day year)
         hits)
    (dlet ((displayed-month (nth 0 date)) (displayed-year (nth 2 date)))
      (dolist (spec dashboard--canadian-holidays)
        (dolist (h (eval spec t))         ; each H is (DATE NAME)
          (when (equal (car h) date)
            (push (list :title (cadr h) :holiday t) hits)))))
    (nreverse hits)))

(defun dashboard--items-for (date)
  "Merged gcal + org-agenda + Canadian-holiday events for DATE ((M D Y) list),
sorted by start time, colliding events tagged `:overlap t'.  An event in both
gcal and org (same title + start) is kept once, preferring the Org-backed copy
so its marker drives RET / agenda commands.  Holidays are all-day, so they
sort to the end and never trigger overlap warnings.  Binds
`dashboard-work-calendar-date' so the gcal/holiday sources resolve to DATE."
  (let* ((dashboard-work-calendar-date date)
         (org-events (dashboard--agenda-events-for date))
         (seen       (make-hash-table :test #'equal))
         (ext-uniq   nil))
    (dolist (o org-events) (puthash (dashboard--event-key o) t seen))
    ;; Keep gcal + personal-ICS events not already covered by an Org item,
    ;; deduping across both external sources by title+start.
    (dolist (g (append (dashboard--gcal-events-today)
                       (dashboard--ics-events-for date)))
      (unless (gethash (dashboard--event-key g) seen)
        (puthash (dashboard--event-key g) t seen)
        (push g ext-uniq)))
    (dashboard--insert-now-row
     date
     (dashboard--mark-overlaps
      (cl-sort (append (nreverse ext-uniq) org-events
                       (dashboard--holiday-events-today))
               #'string< :key #'dashboard--sort-key)))))

(defun dashboard--countdown-days (cd)
  "Days-away as an integer from a countdown string CD, or nil.
Handles `today'/`tomorrow', `in N days', and an ISO `YYYY-MM-DD' date (days
from today).  Used only to order all-day countdown rows among themselves."
  (when cd
    (cond
     ((string-match-p "\\`today\\'" cd) 0)
     ((string-match-p "\\`tomorrow\\'" cd) 1)
     ((string-match "\\`in \\([0-9]+\\) days?\\'" cd)
      (string-to-number (match-string 1 cd)))
     ((string-match "\\`\\([0-9]\\{4\\}\\)-\\([0-9][0-9]\\)-\\([0-9][0-9]\\)\\'" cd)
      (- (calendar-absolute-from-gregorian
          (list (string-to-number (match-string 2 cd))
                (string-to-number (match-string 3 cd))
                (string-to-number (match-string 1 cd))))
         (calendar-absolute-from-gregorian (calendar-current-date)))))))

(defun dashboard--sort-key (o)
  "Sort key for event O: timed events by `HH:MM'; all-day countdown rows after
them, ordered by days-away; plain all-day rows last.  A string so one
`string<' sort handles every case."
  (or (plist-get o :start)                      ; timed: "09:40"
      (when-let ((d (dashboard--countdown-days (plist-get o :countdown))))
        (format "99:%05d" (max 0 d)))           ; countdown: after timed, by proximity
      "99:99999"))                              ; plain all-day: dead last

(defconst dashboard--now-row-title
  "⏰⟵⏰⟵⏰⟵⏰⟵⏰⟵⏰⟵⏰⟵⟨ 𝒩ℴ𝓌 ⟩⟶⏰⟶⏰⟶⏰⟶⏰⟶⏰⟶⏰⟶⏰"
  "Title of the synthetic current-time row, mirroring the org-agenda now-line.")

(defun dashboard--today-p (date)
  "Non-nil when DATE ((M D Y) list) is the actual current day."
  (equal date (calendar-current-date)))

(defun dashboard--insert-now-row (date events)
  "Splice a `⟨ 𝒩ℴ𝓌 ⟩' marker row into sorted EVENTS at the current time.
Only when DATE is today --- a past/future day has no meaningful \"now\".
The marker is `:now t' so face/overlap logic can treat it specially, and it
re-sorts in by `:start' (current HH:MM)."
  (if (not (dashboard--today-p date))
      events
    (let* ((now (decode-time))
           (now-min (+ (* 60 (nth 2 now)) (nth 1 now)))
           (hhmm (format "%02d:%02d" (nth 2 now) (nth 1 now)))
           (marker (list :now t :start hhmm :title dashboard--now-row-title))
           ;; Tag every timed event that has already ended (or started, if it
           ;; has no end) as `:past' so `dashboard--row-face' greys it.
           (tagged (mapcar
                    (lambda (o)
                      (let ((end (dashboard--hhmm->min (or (plist-get o :end)
                                                           (plist-get o :start)))))
                        (if (and end (<= end now-min))
                            (plist-put (copy-sequence o) :past t)
                          o)))
                    events)))
      (cl-stable-sort (cons marker tagged)
                      #'string< :key #'dashboard--sort-key))))

(defvar dashboard-work-calendar-span 1
  "How many days the work-calendar shows, starting at the effective date.
1 = a single day (flat, no headings).  >1 = that many consecutive days,
each under a `Monday {June 29 2026}' group heading.  Set via the `v' keymap
\(`v t' / `v w' / `v N').")

(defun dashboard--day-heading (date)
  "Group heading for DATE ((M D Y) list): \"Monday {June 29 2026}\"."
  (pcase-let ((`(,m ,d ,y) date))
    (format-time-string "%A {%B %-d %Y}" (encode-time 0 0 12 d m y))))

(defun dashboard--day-after (date n)
  "Return DATE ((M D Y) list) advanced by N days."
  (pcase-let ((`(,m ,d ,y) date))
    (let ((g (calendar-gregorian-from-absolute
              (+ n (calendar-absolute-from-gregorian (list m d y))))))
      (list (nth 0 g) (nth 1 g) (nth 2 g)))))

(defun dashboard--calendar-items ()
  "Calendar items for the view.  When `dashboard-work-calendar-span' is 1, a
flat list for the effective date.  When >1, a grouped plist
\(\"Monday {…}\" ITEMS \"Tuesday {…}\" ITEMS …) over that many consecutive
days, which the render path lays out as one titled vtable per day."
  (let ((start (dashboard--effective-date))
        (span  (max 1 dashboard-work-calendar-span)))
    (if (= span 1)
        (dashboard--items-for start)
      (cl-loop for i from 0 below span
               for date = (dashboard--day-after start i)
               append (list (dashboard--day-heading date)
                            (dashboard--items-for date))))))

(defun dashboard--12h (hhmm)
  "\"20:00\" -> \"8:00pm\", \"08:00\" -> \"8:00am\".  Passes through if unparsable."
  (if (and hhmm (string-match "\\`\\([0-9][0-9]?\\):\\([0-9][0-9]\\)\\'" hhmm))
      (let* ((h (string-to-number (match-string 1 hhmm)))
             (m (match-string 2 hhmm)))
        (format "%d:%s%s" (let ((x (% h 12))) (if (= x 0) 12 x)) m (if (< h 12) "am" "pm")))
    hhmm))

(defun dashboard--minutes-until (start)
  "Whole minutes from now until START (a today's \"HH:MM\" clock string), or nil.
Negative once the event has begun."
  (when (and start (string-match "\\`\\([0-9]+\\):\\([0-9]+\\)" start))
    (let* ((now   (decode-time))
           (now-m (+ (* 60 (nth 2 now)) (nth 1 now)))
           (ev-m  (+ (* 60 (string-to-number (match-string 1 start)))
                     (string-to-number (match-string 2 start)))))
      (- ev-m now-m))))

(defun dashboard--humanize-mins (mins)
  "Render a non-negative minute count MINS as e.g. \"43min\" or \"1h 32min\"."
  (let ((h (/ mins 60)) (m (% mins 60)))
    (cond ((zerop h) (format "%dmin" m))
          ((zerop m) (format "%dh" h))
          (t         (format "%dh %dmin" h m)))))

(defun dashboard--gcal-help-echo (o)
  "Action + colored countdown help-echo for calendar event O."
  (let* ((action (string-join
                  (delq nil (list (when (plist-get o :conference) "z → join Zoom")
                                  (when (plist-get o :conference) "Z → copy Zoom link")
                                  (when (plist-get o :url) "w → open in web")))
                  "   "))
         (mins   (dashboard--minutes-until (plist-get o :start)))
         (human  (when mins (dashboard--humanize-mins (abs mins))))
         (desc   (when-let ((d (plist-get o :description))) (concat "\n\n" d)))
         ;; (countdown-string . face) by how far out the event is.
         (cd (cond
              ((null mins) nil)
              ((< mins 0)
               (cons (format "started %s ago --- jump in!" human)
                     '(:foreground "red" :weight bold)))
              ((< mins 10)
               (cons (format "%s until event --- grab a tea/snack or hit the bathroom now, then SPEAK UP in the discussion, don't just lurk!" human)
                     '(:foreground "red" :weight bold)))
              ((< mins 60)
               (cons (format "%s until event --- wrap up what you're doing and get your context ready." human)
                     '(:foreground "yellow")))
              ((< mins 120)
               (cons (format "%s until event --- heads up, it's within the next two hours." human)
                     '(:foreground "yellow")))
              (t
               (cons (format "%s until event --- prepare well and participate; as a remote employee, \"to be\" is \"to be seen\"!" human)
                     '(:foreground "green" :weight bold))))))
    (concat
     (when (plist-get o :overlap)
       (propertize "⚠️ COLLISION! This clashes with another event --- reschedule one (C-c C-s) or rethink your day; you can't be in two places at once.\n"
                   'face '(:foreground "red" :weight bold)))
     ;; A started org task with no end and no effort to derive one: nudge to
     ;; time-block it.  (`:effort-derived' means we already inferred an end.)
     (when (and (plist-get o :start)
                (null (plist-get o :end))
                (let ((m (plist-get o :marker))) (and (markerp m) (marker-buffer m))))
       (propertize "⏳ Not time-blocked! Press `E' to set an effort --- we'll block out that long and flag any clashes.\n"
                   'face '(:foreground "dark orange" :weight bold)))
     ;; A :Work: event outside the 8am-4pm block: stern reprimand.
     (when (and (dashboard--work-item-p o)
                (plist-get o :start)
                (not (dashboard--working-hours-p o)))
       (propertize "🛑 WORK outside 8am-4pm?! What are you doing with your life? This is family time --- could you be with your kids instead? Protect the boundary.\n"
                   'face '(:foreground "red" :weight bold)))
     (unless (string-empty-p action) (concat action "\n"))
     (when cd (propertize (car cd) 'face (cdr cd)))
     desc)))

(defconst dashboard--work-color "#8B5A2B"   ; saddle/suitcase brown
  "Face colour for working-hours / `:Work:'-tagged calendar rows.")
(defconst dashboard--off-color "forest green"
  "Face colour for non-working-hours calendar rows.")

(defun dashboard--work-item-p (o)
  "Non-nil when event O is tagged :Work: (case-insensitive)."
  (cl-some (lambda (tag) (string-equal-ignore-case tag "Work"))
           (plist-get o :tags)))

(defun dashboard--working-hours-p (o)
  "Non-nil when event O starts within the 8am-4pm working block."
  (when-let ((m (dashboard--hhmm->min (plist-get o :start))))
    (and (>= m (* 8 60)) (< m (* 16 60)))))   ; [08:00, 16:00)

(defconst dashboard--past-color "gray55"
  "Face colour for calendar rows whose time is already past (today only).")

(defun dashboard--row-face (o)
  "Face for event O's row.
Precedence: the `⟨ 𝒩ℴ𝓌 ⟩' marker (bold magenta) > past rows (grey) > overlap
red > work/working-hours brown > off-hours green.  Past wins over overlap: a
clash that already elapsed isn't worth a red alarm."
  (cond
   ((plist-get o :now)  '(:foreground "magenta" :weight bold))
   ((plist-get o :past) (list :foreground dashboard--past-color))
   ((plist-get o :overlap) '(:foreground "red" :weight bold))
   ((or (dashboard--work-item-p o) (dashboard--working-hours-p o))
    (list :foreground dashboard--work-color :weight 'bold))
   (t (list :foreground dashboard--off-color))))

(defun dashboard--overlap-propertize (o str)
  "Colour STR by O's row face (see `dashboard--row-face')."
  (propertize str 'face (dashboard--row-face o)))

(defun dashboard--calendar-serializer (o)
  "Serialize calendar event O into an `:org-serializer' SPEC.
Returns (TITLE :SCHEDULED <today @ start> :ZOOM … :LOCATION … :ATTENDEES …),
dropping any field the event lacks --- so an agenda key minting a tree from
a gcal row gets it scheduled for the viewing date at the known start time,
with the Zoom link/location/attendees we already have."
  (let* ((today (dashboard--effective-org-date))
         (start (plist-get o :start))
         (end   (plist-get o :end))
         ;; Org time-range syntax: <date HH:MM-HH:MM>; falls back to a single
         ;; time when the event has no end.
         (stamp (when start (format "<%s %s%s>" today start (if end (concat "-" end) ""))))
         (conf  (or (plist-get o :conference) (plist-get o :url)))
         (loc   (plist-get o :location))
         (att   (plist-get o :attendees)))
    `(,(or (plist-get o :title) "Calendar event")
      ,@(when stamp (list :SCHEDULED stamp))
      ,@(when conf  (list :ZOOM conf))
      ,@(when loc   (list :LOCATION loc))
      ,@(when att   (list :ATTENDEES (if (listp att) (string-join att ", ") att))))))

(defun dashboard--shift-hhmm (hhmm delta-min)
  "Shift \"HH:MM\" by DELTA-MIN minutes, clamped to [00:00, 23:59].  Nil-safe."
  (when-let ((m (dashboard--hhmm->min hhmm)))
    (let ((m2 (max 0 (min 1439 (+ m delta-min)))))
      (format "%02d:%02d" (/ m2 60) (% m2 60)))))

(defun dashboard--reschedule-by (delta-min)
  "Reschedule the calendar row at point by DELTA-MIN minutes (today).
Creates the Org tree first if the row has none (so it works on gcal rows
too), shifts start and end together (preserving duration), writes a new
SCHEDULED, then refreshes keeping point on the row."
  (let* ((obj (vtable-current-object))
         (marker (aq-agenda--marker-or-create))   ; create-on-miss
         (new-start (dashboard--shift-hhmm (plist-get obj :start) delta-min))
         (new-end   (dashboard--shift-hhmm (plist-get obj :end)   delta-min))
         (today (dashboard--effective-org-date)))
    (if (not new-start)
        (message "No start time to shift on this row")
      (org-with-remote-undo (marker-buffer marker)
        (with-current-buffer (marker-buffer marker)
          (org-with-point-at marker
            (org-entry-put (point) "SCHEDULED"
                           (format "<%s %s%s>" today new-start
                                   (if new-end (concat "-" new-end) ""))))))
      (message "Rescheduled to %s%s"
               (dashboard--12h new-start)
               (if new-end (concat "–" (dashboard--12h new-end)) ""))
      (aq-agenda--update-line marker))))

(actionable-query-defview dashboard/work-calendar "📅 Today's Work Calendar"
  :objects #'dashboard--calendar-items
  ;; Org-backed rows expose their marker so C-c C-s / t / : etc. work; gcal
  ;; rows have no marker and stay link-only.
  :org (lambda (o) (plist-get o :marker))
  ;; When an agenda key mints a tree from a gcal row, name it after the event
  ;; and prefill SCHEDULED (today @ start) + Zoom/location/attendees.
  :org-serializer #'dashboard--calendar-serializer
  :columns `((:name "Time" :width 18
                    :getter ,(lambda (o &rest _)
                               (dashboard--overlap-propertize o
                                (let ((s (plist-get o :start)) (e (plist-get o :end))
                                      (cd (plist-get o :countdown)))
                                  (cond ((and s e) (format "%s–%s" (dashboard--12h s) (dashboard--12h e)))
                                        (s (dashboard--12h s))
                                        ;; A countdown sexp (anniversary/holiday): "⟪in 6 days⟫".
                                        (cd (format "⟪%s⟫" cd))
                                        (t "all-day"))))))
             (:name "Event"  ; no :width -> vtable sizes to the widest title
                    :getter ,(lambda (o &rest _)
                               (dashboard--overlap-propertize o (or (plist-get o :title) "?")))))
  :help-echo #'dashboard--gcal-help-echo
  :actions `(("z" "Open the Zoom/meeting call"
              ,(lambda (o)
                 (if-let ((link (plist-get o :conference)))
                     (browse-url link)
                   (message "No Zoom/meeting link for this event"))))
             ("Z" "Copy the Zoom URL (to share)"
              ,(lambda (o)
                 (if-let ((link (plist-get o :conference)))
                     (progn (kill-new link) (message "Copied Zoom link: %s" link))
                   (message "No Zoom/meeting link for this event"))))
             ("w" "Browse the event in the web (or jump to its org tree)"
              ,(lambda (o)
                 (cond
                  ((plist-get o :url) (browse-url (plist-get o :url)))
                  ;; No web link, but an org-backed row → jump to its tree.
                  ((let ((m (plist-get o :marker))) (and (markerp m) (marker-buffer m)))
                   (message "This is an org-agenda event!")
                   (aq-nav-goto-row-heading))
                  (t (message "No web link for this event")))))
             ("E" "Set effort --- blocks out that long, recomputes clashes"
              ,(lambda (_o)
                 (aq-agenda-set-effort)
                 ;; Re-derive ends + overlaps: a single-row update can't recompute
                 ;; clashes against peers, so revert the whole view's objects
                 ;; (re-runs `dashboard--items-for' → fresh `mark-overlaps').
                 (actionable-query-refresh-current-view)
                 (message "Effort set --- time-blocked and clashes re-checked.")))
             ("G" "Refetch from gcalcli + ICS (g reuses cache)"
              ,(lambda (_o)
                 (dashboard--gcal-clear-cache)
                 (dashboard--ics-clear-cache)
                 (actionable-query-refresh-current-view)
                 (message "Refetched calendar from gcalcli + ICS.")))
             ("M-<up>" "Reschedule 15 min earlier"
              ,(lambda (_o) (dashboard--reschedule-by -15)))
             ("M-<down>" "Reschedule 15 min later"
              ,(lambda (_o) (dashboard--reschedule-by 15)))
             ("D" "Pick a date to view"
              ,(lambda (_o) (dashboard--pick-date)))
             ("T" "Jump back to today"
              ,(lambda (_o) (dashboard--goto-today)))
             ("v" "View span: v t today / v w week / v N next-N-days"
              ,(lambda (_o) (dashboard-view-dispatch)))
             ("s" "Share the day's work items to Slack standup"
              ,(lambda (_o) (dashboard-share-standup))))
  ;; No date-stamp / fetch-status prose, and no core "Last fetched" footer.
  :no-footer t)

(defun dashboard--rerender-calendar ()
  "Fully re-render the calendar (vtable + prose-bottom) for `dashboard-work-calendar-date'.
A plain `actionable-query-refresh-current-view' only reverts the vtable and
leaves the date stamp / fetch-status prose stale, so re-invoke the view."
  (dashboard--gcal-clear-cache)
  (dashboard--ics-clear-cache)
  (dashboard/work-calendar))

(defun dashboard--pick-date ()
  "Prompt for a date (via `org-read-date'), then re-render the calendar for it.
Accepts everything `org-read-date' does --- \"wed\", \"+1w\", \"2026-07-01\", …"
  (interactive)
  (let* ((picked (org-read-date nil nil nil "Show work calendar for"))  ; "YYYY-MM-DD"
         (parts  (mapcar #'string-to-number (split-string picked "-"))))
    (setq dashboard-work-calendar-date (list (nth 1 parts) (nth 2 parts) (nth 0 parts)))  ; (M D Y)
    (dashboard--rerender-calendar)))

(defun dashboard--goto-today ()
  "Reset the calendar to today and re-render."
  (interactive)
  (setq dashboard-work-calendar-date nil)
  (dashboard--rerender-calendar))

;; ── Multi-day span commands (the `v' prefix) ──────────────────────────────

(defun dashboard--view-days (n &optional start)
  "Show N consecutive days starting at START (or today), grouped by day."
  (setq dashboard-work-calendar-date start
        dashboard-work-calendar-span (max 1 n))
  (dashboard--rerender-calendar))

(defun dashboard-view-today ()
  "`v t' --- show just today (single day, no headings)."
  (interactive)
  (dashboard--view-days 1 nil))

(defun dashboard-view-week ()
  "`v w' --- show the current week, Monday through Sunday, grouped by day."
  (interactive)
  (let* ((today    (calendar-current-date))
         (abs      (calendar-absolute-from-gregorian today))
         (dow      (calendar-day-of-week today))        ; 0=Sun … 6=Sat
         (back     (if (= dow 0) 6 (1- dow)))           ; days since Monday
         (monday   (calendar-gregorian-from-absolute (- abs back))))
    (dashboard--view-days 7 (list (nth 0 monday) (nth 1 monday) (nth 2 monday)))))

(defun dashboard-view-next-days (n)
  "`v N' --- show the next N days starting today, grouped by day."
  (interactive "nShow how many days: ")
  (dashboard--view-days n nil))

(defun dashboard-view-dispatch ()
  "The `v' prefix: read one more key --- t/w/<digit> --- and switch the span."
  (interactive)
  (let ((k (read-char "v … (t)oday  (w)eek  (1-9) next-N-days: ")))
    (cond
     ((eq k ?t) (dashboard-view-today))
     ((eq k ?w) (dashboard-view-week))
     ((and (>= k ?1) (<= k ?9)) (dashboard--view-days (- k ?0) nil))
     (t (message "v: expected t, w, or a digit 1-9")))))

;; ── Share the day's work as a Slack standup (the `s' key) ─────────────────

(defvar dashboard-standup-slack-url nil
  "Browser URL of the Slack standup channel, opened by `dashboard-share-standup'.
Set in private.el, e.g. \"https://app.slack.com/client/TXXXX/CYYYY\".")

(defvar dashboard--standup-noise-rx
  (rx (or "pray" "prayer" "quran" "dua" "lunch" "breakfast" "dinner"
          "nap" "ritual" "wind" "sleep" "family" "kids" "taekwondo"
          "shave" "shower" "run" "read" "🤲" "📿" "😴"))
  "Case-insensitive regexp for non-work calendar items to omit from the standup.")

(defun dashboard--standup-shareable-p (o)
  "Non-nil when timed calendar row O belongs in the standup.
Rituals (matching `dashboard--standup-noise-rx') and the `⟨ 𝒩ℴ𝓌 ⟩' marker are
always excluded.  Past that:
 - Org-backed rows (a live `:marker') are real tasks --- FWD/Gerrit work minted
   via RET/C-c C-s --- so they qualify at *any* hour (an early deep-work block
   shouldn't be dropped just for starting before 8am).
 - Markerless gcal meetings qualify only within working hours (8am-4pm)."
  (let* ((title (or (plist-get o :title) ""))
         ;; A standup-shaped work line carries a `::' action or a ticket link;
         ;; such rows are exempt from the ritual noise-rx, so a ticket whose
         ;; title merely contains a noise word (e.g. "family ethernet-switching")
         ;; isn't mistaken for a ritual.
         (work-shaped (string-match-p "::\\|FWD-[0-9]\\|BUG-[0-9]\\|\\[\\[" title)))
    (and (plist-get o :start)                           ; timed
         (not (plist-get o :now))                       ; not the ⟨ 𝒩ℴ𝓌 ⟩ marker
         (or work-shaped
             (not (let ((case-fold-search t))           ; rituals: only when not work-shaped
                    (string-match-p dashboard--standup-noise-rx title))))
         (or (let ((m (plist-get o :marker)))            ; org-backed task → any hour
               (and (markerp m) (marker-buffer m)))
             (dashboard--working-hours-p o)))))          ; gcal meeting → 8am-4pm

(defun dashboard--standup-heading-line (o)
  "Org heading text for row O, preferring its node's headline over the calendar title.
A Gerrit/Jira row minted via RET/C-c C-s carries a standup-ready headline
\(`[[jira][TICKET]] Title :: <action>') --- read that off the marker so `s'
reuses the work the `:org-serializer' already did.  Falls back to the
calendar title for markerless or plain rows."
  (or (when-let ((m (plist-get o :marker)) ((marker-buffer m)))
        (org-with-point-at m
          ;; Raw heading keeps the [[jira][TICKET]] link so `my/copy-as-slack'
          ;; turns it into Slack's <url|TICKET>; strip only the TODO/tags/cookie.
          (substring-no-properties (org-get-heading t t t t))))
      (plist-get o :title)))

(defun dashboard--standup-work-items ()
  "The day's shareable work items as (TICKET? . LINE) splits, meetings last.
A row backed by an Org node yields its standup headline (ticket + action);
a markerless meeting yields just its title.  Returns two lists in a cons:
\(WORK-LINES . MEETING-TITLES), preserving each in calendar order."
  (let (work meetings)
    (dolist (o (dashboard--items-for (dashboard--effective-date)))
      (when (dashboard--standup-shareable-p o)
        (if (plist-get o :marker)
            (push (dashboard--standup-heading-line o) work)
          (push (or (plist-get o :title) "?") meetings))))
    (cons (nreverse work) (nreverse meetings))))

(defun dashboard--roman (n)
  "Lower-case Roman numeral for N in 1..39 (plenty for a day's meetings)."
  (let ((parts '((10 . "x") (9 . "ix") (5 . "v") (4 . "iv") (1 . "i")))
        (s ""))
    (dolist (p parts s)
      (while (>= n (car p)) (setq s (concat s (cdr p)) n (- n (car p)))))))

(defun dashboard--standup-org (work meetings)
  "Render WORK lines + MEETINGS titles as an Org list for `my/copy-as-slack'.
Work items are a numbered top level; meetings collapse into a single
`Meetings ::' item with Roman-numeral sub-points."
  (let ((lines (cl-loop for it in work for n from 1
                        collect (format "%d. %s" n it))))
    (when meetings
      (setq lines (append lines
                          (list (format "%d. Meetings ::" (1+ (length work))))
                          (cl-loop for m in meetings for i from 1
                                   collect (format "   %s. %s" (dashboard--roman i) m)))))
    (string-join lines "\n")))

(defun dashboard-share-standup ()
  "`s' --- aggregate the day's work, copy as Slack mrkdwn, open the channel.
Sources only WORK items (`:Work:' / 8am-4pm), omitting rituals.  Each row
backed by an Org node contributes its standup headline (clickable ticket +
`:: <action>', minted by the view's `:org-serializer'); meetings fold into a
trailing `Meetings ::' group.  Conversion to Slack mrkdwn --- links, the
@-mention name map --- is delegated to `my/copy-as-slack'."
  (interactive)
  (pcase-let ((`(,work . ,meetings) (dashboard--standup-work-items)))
    (if (and (null work) (null meetings))
        (message "No shareable work items for %s." (dashboard--effective-org-date))
      (my/copy-as-slack (dashboard--standup-org work meetings))
      (if dashboard-standup-slack-url
          (browse-url dashboard-standup-slack-url)
        (message "Set `dashboard-standup-slack-url' (private.el) to auto-open the channel."))
      (message "Standup copied (%d item%s) --- paste into Slack (⌘⇧F to format)."
               (+ (length work) (length meetings))
               (if (= 1 (+ (length work) (length meetings))) "" "s")))))

(defun dashboard--heading-link (view label)
  "Org heading text LABEL wrapped as an elisp link that refreshes VIEW in place.
Clicking re-runs (VIEW :insert \\='fetch-latest), re-rendering just that
section's vtable."
  (format "[[elisp:(funcall-interactively '%s)][%s]]" view label))

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

        (insert "\n* 📩 Process Inbox\n\n** "
                (dashboard--heading-link 'whatsapp/contacts "🫶 Social Connection") "\n\n")
        (insert "---Reach out to someone today; relationships need tending. Press `d' to send a greeting.---\n\n")
        (whatsapp/contacts :insert reuse-cache)
        (insert "\n\n** "
                (dashboard--heading-link 'actionable-mail/gmail-inbox "Email") "\n\n")
        (actionable-mail/gmail-inbox :insert reuse-cache)
        (insert "\n\n** " (dashboard--heading-link 'dashboard/inbox "Quick Captures") "\n\n")
        (dashboard/inbox :insert reuse-cache)
        (insert "\n\n** " (dashboard--heading-link 'dashboard/rss-feeds "RSS") "\n\n")
        (dashboard/rss-feeds :insert reuse-cache)

        (insert "\n\n* 📋 Urgent and unstarted\n\n** "
                (dashboard--heading-link 'dashboard/priority-a-unscheduled "🔴 Priority A, unscheduled") "\n\n")
        (insert "---Either schedule this stuff, or make peace with the fact that it's not high priority---\n\n")
        (dashboard/priority-a-unscheduled :insert reuse-cache)
        (insert "\n\n** " (dashboard--heading-link 'oag-reviews-needed "👀 Whom am I blocking?")
                "\n\n---review their work---\n\n")
        (oag-reviews-needed :insert reuse-cache)
        (insert "\n\n** " (dashboard--heading-link 'oag-my-changes-needing-action "🔧 Feedback I need to address")
                "\n\n---address it, then re-publish---\n\n")
        (oag-my-changes-needing-action :insert reuse-cache)

        (insert "\n\n* 📆 Planning\n\n** "
                (dashboard--heading-link 'dashboard/waiting "💢 I've been waiting on these for over a week, send reminder!") "\n\n")
        (dashboard/waiting :insert reuse-cache)
        (insert "\n\n** " (dashboard--heading-link 'oag-jira-urgent-not-started "🔥 Jira: Urgent Not Yet Started") "\n\n")
        (insert "---pick one, scope it, push a draft---\n\n")
        (oag-jira-urgent-not-started :insert reuse-cache)
        (insert "\n\n** " (dashboard--heading-link 'oag-jira-active-no-gerrit "⚠️ Jira says active, Gerrit disagrees") "\n\n")
        (insert (concat
                 "These tickets are marked In Progress or In Review in Jira, yet\n"
                 "none of my open Gerrit changes reference them.  For each row,\n"
                 "pick exactly one:\n"
                 "  - Push a draft change that cites the ticket in its footer, or\n"
                 "  - Move the ticket back to To Do / Blocked --- the status is lying, or\n"
                 "  - Reassign it, if someone else is actually carrying the work.\n"
                 "Leaving a ticket here is a promise you are silently breaking.\n\n"))
        (oag-jira-active-no-gerrit :insert reuse-cache)
        (insert "\n\n** 📆 Overdue\n\n*** "
                (dashboard--heading-link 'dashboard/deadline-overdue "Past deadline due-date") "\n\n")
        (dashboard/deadline-overdue :insert reuse-cache)
        (insert "\n\n*** " (dashboard--heading-link 'dashboard/overdue "Past scheduled start date") "\n\n")
        (dashboard/overdue :insert reuse-cache)
        (insert "\n\n** "
                (dashboard--heading-link 'dashboard/open-loops "🤡 Please 𝒓𝒆𝒅𝒖𝒄𝒆 the number of (unscheduled) open loops") "\n\n")
        (dashboard/open-loops :insert reuse-cache)
        (insert "\n\n** " (dashboard--heading-link 'dashboard/work-in-progress "🚧 Gerrit: Work In Progress") "\n\n")
        (dashboard/work-in-progress :insert reuse-cache))
      (goto-char (point-min))))
  (pop-to-buffer "*Dashboard*"))

;; Loading the calendar feature makes Org timestamps open it on click.
(actionable-query-enable-timestamp-views)

(provide 'dashboard)
;;; dashboard.el ends here
