;;; aq-data-rss.el --- RSS 2.0 / Atom / CLI fetchers for actionable-query  -*- lexical-binding: t; -*-

;; Author: Musa Al-hassy <alhassy@gmail.com>

;;; Commentary:

;; Async data sources for `actionable-query-defview':
;;
;;   `aq--cli-async'   — spawn a shell command, parse its stdout, deliver.
;;   `aq--fetch-rss'   — fetch + parse an RSS 2.0 <item>-based feed.
;;   `aq--fetch-atom'  — fetch + parse an Atom <entry>-based feed.
;;
;; All three deliver a list of plists with the keys `:title', `:url',
;; `:date', `:description', `:categories' — directly consumable by a
;; view configured with `actionable-query-rss-columns'.

;;; Code:

(require 'cl-lib)
(require 'xml)
(require 'url)

(defun aq--cli-async (command parser callback)
  "Spawn COMMAND asynchronously and deliver parsed objects to CALLBACK.
COMMAND is a string; it is split on whitespace to form the argv.
PARSER is a function (raw-stdout-string → list-of-objects); defaults to
splitting stdout on newlines, yielding one string per non-empty line.
CALLBACK is the 1-arg actionable-query deliver function."
  (let ((argv (if (listp command) command (split-string command))))
    (make-process
     :name (car argv)
     :command argv
     :buffer (generate-new-buffer " *actionable-query-cli*")
     :sentinel
     (lambda (proc _)
       (when (eq (process-status proc) 'exit)
         (let ((raw (with-current-buffer (process-buffer proc) (buffer-string))))
           (kill-buffer (process-buffer proc))
           (funcall callback
                    (funcall (or parser (lambda (s) (split-string s "\n" t)))
                             raw))))))))

(defun aq--strip-html (str &optional max-width)
  "Strip HTML tags from STR and truncate to MAX-WIDTH (default 200)."
  (truncate-string-to-width
   (replace-regexp-in-string "<[^>]\\{1,\\}>" "" str)
   (or max-width 200)))

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

(defun aq--format-pubdate (date-string)
  "Format an RSS pubDate string as YYYY-MM-DD, or return DATE-STRING as-is."
  (or (and (stringp date-string)
           (ignore-errors
             (format-time-string "%Y-%m-%d" (date-to-time date-string))))
      date-string ""))

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

(provide 'aq-data-rss)
;;; aq-data-rss.el ends here
