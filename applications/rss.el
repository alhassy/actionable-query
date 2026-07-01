;;; rss.el --- RSS 2.0 / Atom feed fetchers for actionable-query  -*- lexical-binding: t; -*-

;; Author: Musa Al-hassy <alhassy@gmail.com>

;;; Commentary:

;; Feed-specific data sources built on the generic plumbing in
;; `core/fetchers.el' (`aq--strip-html', `aq--format-pubdate'):
;;
;;   `aq--fetch-rss'   — fetch + parse an RSS 2.0 <item>-based feed.
;;   `aq--fetch-atom'  — fetch + parse an Atom <entry>-based feed.
;;
;; Both deliver a list of plists with the keys `:title', `:url', `:date',
;; `:description', `:categories' --- directly consumable by a view
;; configured with `actionable-query-rss-columns'.

;;; Code:

(require 'cl-lib)
(require 'xml)
(require 'url)
(require 'fetchers)        ; `aq--strip-html', `aq--format-pubdate'

(defun aq--parse-rss-items (xml-root)
  "Parse XML-ROOT (from `xml-parse-region') into a list of plists.
Supports RSS 2.0 <item>-based feeds."
  (let* ((channel (car (xml-get-children (car xml-root) 'channel)))
         (items   (xml-get-children channel 'item)))
    (mapcar
     (lambda (item)
       (let* ((title  (car (xml-node-children (car (xml-get-children item 'title)))))
              (link   (car (xml-node-children (car (xml-get-children item 'link)))))
              (date   (car (xml-node-children (car (xml-get-children item 'pubDate)))))
              (desc   (car (xml-node-children (car (xml-get-children item 'description)))))
              (cats   (mapcar (lambda (c) (car (xml-node-children c)))
                              (xml-get-children item 'category)))
              (clean  (when (stringp desc) (aq--strip-html desc))))
         (list :title       (if (stringp title) title "")
               :url         (if (stringp link)  link  "")
               :date        (if (stringp date)  date  "")
               :description (or clean "")
               :categories  (cl-remove-if-not #'stringp cats))))
     items)))

(defvar actionable-query-rss-columns
  '((:name "Date"
           :width 12
           :getter    (lambda (o &rest _) (aq--format-pubdate (plist-get o :date)))
           :displayer (lambda (v w _) (propertize (truncate-string-to-width v w)
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

(defun aq--parse-atom-items (xml-root)
  "Parse XML-ROOT (from `xml-parse-region') into a list of plists.
Supports Atom <entry>-based feeds (e.g. Planet Emacslife).
Differences from RSS 2.0:
  <entry>  instead of <item>
  <updated> instead of <pubDate>
  <link href=\"…\"> attribute instead of <link> text node
  <content> (or <summary>) instead of <description>
  <category term=\"…\"> attribute instead of <category> text node"
  ;; xml-parse-region drops the xmlns attribute, so the root element is just
  ;; 'feed regardless of the Atom namespace declaration.
  (let* ((feed    (car xml-root))
         (entries (xml-get-children feed 'entry)))
    (mapcar
     (lambda (entry)
       (let* ((title   (car (xml-node-children (car (xml-get-children entry 'title)))))
              ;; <link href="…"> — value lives in the attribute, not text content.
              (link-el (car (xml-get-children entry 'link)))
              (link    (and link-el (xml-get-attribute link-el 'href)))
              (date    (car (xml-node-children (car (xml-get-children entry 'updated)))))
              ;; Prefer <content> over <summary> for the description snippet.
              (content-el (or (car (xml-get-children entry 'content))
                              (car (xml-get-children entry 'summary))))
              (desc    (car (xml-node-children content-el)))
              ;; <category term="emacs"> — value in attribute.
              (cats    (mapcar (lambda (c) (xml-get-attribute c 'term))
                               (xml-get-children entry 'category)))
              (clean   (when (stringp desc) (aq--strip-html desc))))
         (list :title       (if (stringp title) title "")
               :url         (or link "")
               :date        (if (stringp date)  date  "")
               :description (or clean "")
               :categories  (cl-remove-if-not #'stringp cats))))
     entries)))

(defun aq--fetch-feed (url parser callback)
  "Fetch URL asynchronously, parse with PARSER, and call CALLBACK with result plists.
A malformed response (HTTP error page, broken XML, …) delivers nil rather
than silently dropping the callback — callers fanning out to several feeds
rely on CALLBACK always firing exactly once."
  (url-retrieve
   url
   (lambda (_status)
     (funcall callback
              (or (condition-case err
                      (progn
                        (goto-char (point-min))
                        (re-search-forward "\n\n")
                        (funcall parser (xml-parse-region (point) (point-max))))
                    (error (message "aq--fetch-feed: %s failed: %S" url err) nil))
                  nil)))
   nil t t))

(defun aq--fetch-rss (url callback)
  "Fetch URL asynchronously, parse as RSS 2.0, and call CALLBACK with item plists."
  (aq--fetch-feed url #'aq--parse-rss-items callback))

(defun aq--fetch-atom (url callback)
  "Fetch URL asynchronously, parse as Atom, and call CALLBACK with entry plists."
  (aq--fetch-feed url #'aq--parse-atom-items callback))

;;; ─── the dashboard's RSS feed view ───────────────────────────────────────────

(require 'actionable-query)            ; `actionable-query-defview'

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

(provide 'rss)
;;; rss.el ends here
