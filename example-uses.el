;;; neato-examples.el --- Example neato.el views  -*- lexical-binding: t; -*-

;;; ─── example neato.el views ──────────────────────────────────────────────────────────

(neato-defview "Non-Org Query: Images in ~/Downloads"
  :prose (fortune-of-the-day--agenda-insert-quote)
  :objects (lambda () (directory-files "~/Downloads" t "\\.jpg\\'"))
  :columns `(( :name "Name" :width 30 :primary ascend
               :formatter file-name-nondirectory)
             ( :name "Thumbnail" :width "300px"
               :displayer ,(lambda (value max-width _tbl)
                             (propertize "*" 'display
                                         (create-image value nil nil
                                                       :max-width max-width)))))
  :actions `(("RET" "Open in Emacs"          find-file)
             ("o"   "Open in macOS (Preview)" ,(lambda (f) (shell-command (format "open %s" (shell-quote-argument f)))))
             ("w"   "Copy path to kill-ring"  ,(lambda (f) (kill-new f) (neato--message "Copied: %s" f)))
             ("r"   "Rename file"             ,(lambda (f)
                                                 (let ((new (read-string "Rename to: " (file-name-nondirectory f))))
                                                   (rename-file f (expand-file-name new (file-name-directory f)))
                                                   (vtable-revert))))
             ("d"   "Delete to Trash"         ,(lambda (f)
                                                 (when (yes-or-no-p (format "Delete %s? " (file-name-nondirectory f)))
                                                   (delete-file f :trash)
                                                   (vtable-revert))))
             ("hi" "Hokie" (lambda (x) (message-box "HI: got %s" x)))
             ("ho" "pokie" (lambda (x) (message-box "HO: got %s" x))))
  :row-colors '("green1" "HotPink2")
  :help-echo (lambda (f)
               (propertize
                (concat "File size: " (file-size-human-readable (file-attribute-size (file-attributes f))))
                'face 'org-agenda-structure))
  :separator-width 5)

;; See it with:
;; (org-ql-view "Non-Org Query: Images in ~/Downloads")


;;; ─── RSS feed views ─────────────────────────────────────────────────────────

(defconst neato-tests--rss-xml
  "<?xml version=\"1.0\"?>
<rss version=\"2.0\"><channel>
  <item>
    <title>Hello World</title>
    <link>https://example.com/hello</link>
    <pubDate>Thu, 07 May 2026 10:00:00 +0000</pubDate>
    <description>&lt;p&gt;Some &lt;b&gt;HTML&lt;/b&gt; here.&lt;/p&gt;</description>
    <category>emacs</category>
    <category>lisp</category>
  </item>
  <item>
    <title>Minimal Item</title>
    <link>https://example.com/minimal</link>
  </item>
</channel></rss>")


(defconst neato-tests--atom-xml
  "<?xml version=\"1.0\"?>
<feed xmlns=\"http://www.w3.org/2005/Atom\">
  <entry>
    <title>Atom Entry</title>
    <link href=\"https://example.com/atom\"/>
    <updated>2026-05-07T10:00:00Z</updated>
    <content type=\"html\">&lt;p&gt;Content &lt;b&gt;here&lt;/b&gt;&lt;/p&gt;</content>
    <category term=\"emacs\"/>
    <category term=\"atom\"/>
  </entry>
  <entry>
    <title>Minimal Atom</title>
    <link href=\"https://example.com/minimal\"/>
    <summary>A summary.</summary>
  </entry>
</feed>")

;; Objects are plists: (:title "…" :url "…" :date "…" :description "…" :categories (…))

(defvar neato-rss-columns
  '((:name "Date"
           :width 12
           :getter     (lambda (o &rest _) (neato--format-pubdate (plist-get o :date)))
           :displayer  (lambda (v w _) (propertize (truncate-string-to-width v w)
                                              'face '(:height 0.8 :foreground "gray50"))))
    (:name "Title"
           :width 68
           :getter (lambda (o &rest _) (or (plist-get o :title) "?")))
    (:name "Category"
           :width 20
           :getter    (lambda (o &rest _) (string-join (or (plist-get o :categories) '()) ", "))
           :displayer (lambda (v w _) (propertize (truncate-string-to-width v w)
                                             'face '(:height 0.8 :foreground "gray50")))))
  "Standard vtable column specs for RSS 2.0 feed views.")

(defvar neato-rss-actions
  `(("RET" "Open in browser"
     ,(lambda (o) (browse-url (plist-get o :url))))
    ("e"   "Open in eww (Emacs)"
     ,(lambda (o) (eww (plist-get o :url))))
    ("w"   "Copy URL"
     ,(lambda (o) (kill-new (plist-get o :url)) (message "Copied: %s" (plist-get o :url))))
    ;; `c' is the star/save mechanism — capturing to org is intentional.
    ("c"   "Capture as TODO to org"
     ,(lambda (o)
        (org-capture-string
         (format "* TODO [[%s][%s]]" (plist-get o :url) (plist-get o :title))
         "t"))))
  "Standard actions for RSS 2.0 feed views.")

(defvar neato-rss-help-echo
  (lambda (o)
    (let ((d (plist-get o :description)))
      (unless (string-empty-p d) d)))
  "Standard help-echo function for RSS 2.0 feed views: shows the article description.")


(neato-defview "feed/Hacker News"
  ;; RSS 2.0 <item>-based feed.
  :snooze-period    'tomorrow
  :auto-refresh     "30 minutes"
  :objects    (lambda (callback) (neato--fetch-rss "https://news.ycombinator.com/rss" callback))
  :columns          neato-rss-columns
  :use-header-line  nil
  :row-colors       '("thistle" "thistle1" "thistle2" "thistle3")
  :help-echo        neato-rss-help-echo
  :actions `(,@neato-rss-actions
             ("s" "Search HN comments"
              ,(lambda (o)
                 (browse-url (concat "https://hn.algolia.com/?query="
                                     (url-hexify-string (plist-get o :title))))))
             ("y" "Search YouTube"
              ,(lambda (o)
                 (browse-url (concat "https://www.youtube.com/results?search_query="
                                     (url-hexify-string (plist-get o :title))))))
             ("G" "Google the title"
              ,(lambda (o)
                 (browse-url (concat "https://www.google.com/search?q="
                                     (url-hexify-string (plist-get o :title))))))))

;; See it with:
;; (org-ql-view "feed/Hacker News")


(neato-defview "feed/Lobste.rs"
  ;; RSS 2.0 <item>-based feed.
  :snooze-period   'next-week
  :auto-refresh    "1 hour"
  :objects   (lambda (callback) (neato--fetch-rss "https://lobste.rs/rss" callback))
  :columns         neato-rss-columns
  :use-header-line nil
  :row-colors      '("LightCyan" "LightCyan1" "LightCyan2" "LightCyan3")
  :help-echo       neato-rss-help-echo
  :actions         neato-rss-actions)

;; See it with:
;; (org-ql-view "feed/Lobste.rs")


(neato-defview "feed/Reddit r/emacs"
  ;; RSS 2.0 <item>-based feed.
  :snooze-period   'tomorrow
  :auto-refresh    "1 hour"
  :objects   (lambda (callback) (neato--fetch-rss "https://www.reddit.com/r/emacs/.rss" callback))
  :columns         neato-rss-columns
  :use-header-line nil
  :row-colors      '("MistyRose" "MistyRose1" "MistyRose2" "MistyRose3")
  :help-echo       neato-rss-help-echo
  :actions         neato-rss-actions)

;; See it with:
;; (org-ql-view "feed/Reddit r/emacs")


(neato-defview "feed/Planet Emacslife"
  ;; Atom feed — uses `neato--fetch-atom' / `neato--parse-atom-items'.
  :snooze-period   'tomorrow
  :auto-refresh    "1 hour"
  :objects   (lambda (callback) (neato--fetch-atom "https://planet.emacslife.com/atom.xml" callback))
  :columns         neato-rss-columns
  :use-header-line nil
  :row-colors      '("LemonChiffon" "LemonChiffon1" "LemonChiffon2" "LemonChiffon3")
  :help-echo       neato-rss-help-echo
  :actions         neato-rss-actions)

;; See it with:
;; (org-ql-view "feed/Planet Emacslife")


;;
;; curl -s https://alhassy.com/rss | less
;;
(neato-defview "feed/Tech News (grouped) x2"
  ;; Combined HN + Lobste.rs view using the grouped-plist mechanism.
  ;; The :objects callback delivers ("Group Title" objects-list …);
  ;; neato detects the alternating string/list shape and renders one titled
  ;; vtable per group.  Both sources are RSS 2.0; Atom is not supported.
  :snooze-period 'tomorrow
  :auto-refresh  "30 minutes"
  :objects
  (lambda (callback)
    (let* ((results (make-hash-table :test #'equal))
           (done    0)
           (feeds   '(("Hacker News" . "https://news.ycombinator.com/rss")
                      ("Life & CS"   . "https://alhassy.com/rss")
                      ("Lobste.rs"   . "https://lobste.rs/rss")))
           (n-feeds (length feeds)))
      (dolist (feed feeds)
        (let ((gn  (car feed))
              (url (cdr feed)))
          (neato--fetch-rss
           url
           (let ((group-name gn))
             (lambda (items)
               (puthash group-name items results)
               (setq done (1+ done))
               (when (= done n-feeds)
                 ;; Deliver in a stable order matching `feeds'.
                 (funcall callback
                          (cl-loop for (name . _) in feeds
                                   append (list name (gethash name results))))))))))))
  :columns         neato-rss-columns
  :use-header-line nil
  :row-colors      '("honeydew" "honeydew1" "honeydew2" "honeydew3")
  :help-echo       neato-rss-help-echo
  :actions `(,@neato-rss-actions
             ("s" "Search HN comments"
              ,(lambda (o)
                 (browse-url (concat "https://hn.algolia.com/?query="
                                     (url-hexify-string (plist-get o :title))))))))

;; See it with:
;; (org-ql-view "feed/Tech News (grouped) x2")


(neato-defview "demo/Loading Spinner Tests"
  ;; Two async groups with artificial delays: fruits (3s) and veggies (5s).
  ;; Open it, watch "⏳ Loading…" animate until the 5s timer fires, then
  ;; both grouped vtables render together.
  :objects
  (lambda (callback)
    (let* ((results (make-hash-table :test #'equal))
           (pending 2)
           (deliver-group
            (lambda (name items)
              (puthash name
                       (mapcar (lambda (s) (list :name s)) items)
                       results)
              (setq pending (1- pending))
              (when (zerop pending)
                (funcall callback
                         (list "Fruits"     (gethash "Fruits" results)
                               "Vegetables" (gethash "Vegetables" results)))))))
      ;; When this executes, in 3 seconds, “pending” will be 1.
      (run-with-timer 3 nil deliver-group "Fruits" '("Apple" "Banana" "Orange"))
      ;; When this executes, in 5 seconds, “pending” will be 0, and so “callback” will be fired!
      (run-with-timer 5 nil deliver-group "Vegetables" '("Carrots" "Celery" "Cucumbers" "Cabbage"))))
  :columns '((:name "Item" :width 30
                    :getter (lambda (o &rest _) (plist-get o :name))))
  :row-colors '("thistle" "thistle1" "thistle2" "thistle3")
  :actions '(("RET" "Show name"
              (lambda (o) (message "Selected: %s" (plist-get o :name))))))
;;
;; (org-ql-view "demo/Loading Spinner Tests")

;; --------------------------------------------------------------------------------

;; Another example idea: Theme browser, load all Emacs themes and as you scroll between themes, it auto loads!
;; Another example idea: Colour browser, see all colours in emacs, “c” to copy all marked colours into an Elisp list that y9ou can paste elsewhere. Actually, more useful might be to add completing read for Emacs colours! Look at my completing read notes for how to do that!
;; Another example idea: Face browser, see all defined faces in emacs,  “c” to copy all marked colours into an Elisp list that y9ou can paste elsewhere. Also “.” to see source definition, if possible.

;; --------------------------------------------------------------------------------

(defun neato--gcalcli-parse (raw)
  "Parse gcalcli --tsv stdout into (:when … :title …) plists."
  (cl-loop for line in (split-string raw "\n" t)
           unless (string-prefix-p "start_date" line)
           collect (let ((cols (split-string line "\t")))
                     (list :when  (string-trim (format "%s %s"
                                                       (nth 0 cols)
                                                       (or (nth 1 cols) "")))
                           :title (nth 4 cols)))))

(neato-defview "Calendar events for Monday"
  :columns '((:name "When"
                    :width 22
                    :getter (lambda (o &rest _) (plist-get o :when)))
             (:name "Title"
                    :getter (lambda (o &rest _) (plist-get o :title))))
  :objects (lambda (callback)
             (let* ((now    (decode-time))
                    (dow    (nth 6 now))
                    (days   (mod (- 1 dow) 7))
                    (days   (if (= days 0) 7 days))
                    (monday  (time-add (current-time) (* days 86400)))
                    (tuesday (time-add monday 86400))
                    (d0      (format-time-string "%m/%d" monday))
                    (d1      (format-time-string "%m/%d" tuesday)))
               (neato--cli-async (format "gcalcli agenda --tsv %s %s" d0 d1)
                                 #'neato--gcalcli-parse
                                 callback)))
  :actions '(("RET" "Show details" (lambda (o) (message "%s — %s" (plist-get o :when) (plist-get o :title))))))

;; (org-ql-view "Calendar events for Monday")


(neato-defview "🔥 Top CPU" 
  :objects (lambda (callback) 
             (neato--cli-async "ps -eo pid,%cpu,comm" 
                               nil   ; default: lines-as-strings 
                               callback)) 
  :actions '(("RET" "Kill process" 
              (lambda (line) 
                (let ((pid (string-to-number (car (split-string line))))) 
                  (when (yes-or-no-p (format "kill PID %d? " pid)) 
                    (signal-process pid 'SIGTERM)))))))

;; --------------------------------------------------------------------------------


;; A git log browser 😁
(neato-defview "🗓️ What I've worked on this week"
  :columns '((:name "Commit" :getter (lambda (obj &rest _) (plist-get obj :message))))
  :objects   (my/git-log-recent "~/.emacs.d")
  :actions '(("RET" "See commit" (lambda (row) (-let [default-directory "~/.emacs.d"]
                                            (magit-show-commit (plist-get row :sha))
                                            (other-window -1)))))
  ;; Essentially “follow” mode, as cursor moves, the other window shows associated diff.
  ;; TODO: Because of this example, should we rename :help-echo to be something more general, like :on-hover???? 
  :help-echo  (lambda (row) (-let [default-directory "~/.emacs.d"]
                         (magit-show-commit (plist-get row :sha))
                         (other-window -1)
                         (message (format "Other window shows magit-show-commit for %s" (plist-get row :sha))))))
;;
;;
(defun my/git-log-recent (repo)
  "Return REPO's commits by me in the past week as (SHA . DISPLAY) conses."
  (let* ((default-directory repo)
         (author (string-trim (shell-command-to-string "git config user.email")))
         (raw (shell-command-to-string
               (format "git log --author=%s --since='1 week ago' --oneline"
                       (shell-quote-argument author)))))
    (->> (split-string raw "\n" t)
         (-keep (lambda (line)
                  (when (string-match "\\([a-f0-9]+\\) \\(.*\\)" line)
                    (list :sha     (match-string 1 line)
                          :message (match-string 2 line))))))))

;; --------------------------------------------------------------------------------


;; ** Example — Favourite YouTube videos
;; 
;; Query: a static list you curate by hand. Action: =browse-url=. The
;; dedicated-buffer view becomes a "lean back and pick something"
;; remote.
;; 
;; Demonstrates the simplest possible pattern: static data, one action,
;; zero plumbing.
;;

(defvar my/favourite-videos
  '(("Growing a Language - Guy Steele" . "https://www.youtube.com/watch?v=lw6TaiXzHAE")
    ("Dua Abi Hamza" . "https://www.youtube.com/watch?v=b2Eq3Ltc1Bs")
    ("EmacsConf 2023: Editor Integrated REPL Driven Development for all languages - Musa Al-hassy" . "https://www.youtube.com/watch?v=1bk0pqpMCfQ")
    )
  "Alist of (TITLE . URL) — my go-to rewatches.")

(neato-defview "🎬 Favourite videos x.1 "
  :columns '((:name "Title" :formatter (lambda (o &rest _)  o)))
  :objects          my/favourite-videos
  :actions '( ("RET" "Watch it!" (lambda (it) (browse-url-default-browser (cdr it))))))
